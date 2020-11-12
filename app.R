## in this file, I generate a shiny app that allows users to visualize two proportions with a beta distribution
## additionally, the distribution of the % difference between the two proportions from 1m simulations is visualized

library(shiny)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)


# Define UI ----
ui <- fluidPage(
  titlePanel("Bayesian Comparision of Proportions"),
  sidebarLayout(
    sidebarPanel(
      h1('Proportion #1 Input'),
      ##
      h3('Prior Input'),
      numericInput("prior_size_a", "Sample Size:", 50, min = 1, max = 10000),
      sliderInput("prior_prop_a", "Proportion:",min = 0, max = 1, value = 0.45),
      ##
      h3('Likelihood Input'),
      numericInput("size_a", "Sample Size:", 1000, min = 1, max = 10000),
      sliderInput("prop_a", "Proportion:",min = 0, max = 1, value = 0.5),

      h1('Proportion #2  Input'),
      ##
      h3('Prior Input'),
      numericInput("prior_size_b", "Sample Size:", 50, min = 1, max = 10000),
      sliderInput("prior_prop_b", "Proportion:",min = 0, max = 1, value = 0.50),
      ##
      h3('Likelihood Input'),
      numericInput("size_b", "Sample Size:", 1000, min = 1, max = 10000),
      sliderInput("prop_b", "Proportion:",min = 0, max = 1, value = 0.45),

      h2('Simulation Input'),
      numericInput("simulation_volume", "Simulation Volume:", 500000, min = 1, max = 10000000),
    ),

    mainPanel(
      h1('Summary Views of Distributions'),
      splitLayout(cellWidths = c("30%", "30%", "40%"), plotOutput("prior_distributions"), plotOutput("likelihood_distributions"), plotOutput("posterior_distributions")),
      h1('Summary Views of Implied Differences'),
      splitLayout(cellWidths = c("30%", "30%", "40%"), plotOutput("prior_differential"), plotOutput("likelihood_differential"), plotOutput("posterior_differential")),
      tableOutput("table_summary")
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {

  priors <- reactive({

    sample_a <- data.frame(value=rbeta(input$simulation_volume,input$prior_size_a*input$prior_prop_a,input$prior_size_a-(input$prior_size_a*input$prior_prop_a)))
    sample_a$sample <- 'prior a'

    sample_b <- data.frame(value=rbeta(input$simulation_volume,input$prior_size_b*input$prior_prop_b,input$prior_size_b-(input$prior_size_b*input$prior_prop_b)))
    sample_b$sample <- 'prior b'

    samples <- rbind(sample_a,sample_b)
    samples
  })

  prior_diff <- reactive({

    samples <- data.frame(prop_a=rbeta(input$simulation_volume,input$prior_size_a*input$prior_prop_a,input$prior_size_a-(input$prior_size_a*input$prior_prop_a)),prop_b=rbeta(input$simulation_volume,input$prior_size_b*input$prior_prop_b,input$prior_size_b-(input$prior_size_b*input$prior_prop_b)))
    samples$diff <- (samples$prop_b/samples$prop_a)-1
    samples

  })

  likelihood <- reactive({

    sample_a <- data.frame(value=rbeta(input$simulation_volume,input$size_a*input$prop_a,input$size_a-(input$size_a*input$prop_a)))
    sample_a$sample <- 'likelihood a'

    sample_b <- data.frame(value=rbeta(input$simulation_volume,input$size_b*input$prop_b,input$size_b-(input$size_b*input$prop_b)))
    sample_b$sample <- 'likelihood b'

    samples <- rbind(sample_a,sample_b)
    samples
  })

  likelihood_diff <- reactive({

    samples <- data.frame(prop_a=rbeta(input$simulation_volume,input$size_a*input$prop_a,input$size_a-(input$size_a*input$prop_a)),prop_b=rbeta(input$simulation_volume,input$size_b*input$prop_b,input$size_b-(input$size_b*input$prop_b)))
    samples$diff <- (samples$prop_b/samples$prop_a)-1
    samples

  })

  posterior <- reactive({

    a_trials = input$prior_size_a+input$size_a
    a_alpha = round((input$prior_size_a*input$prior_prop_a)+(input$size_a*input$prop_a),digits=0)
    a_beta = a_trials-a_alpha

    b_trials = input$prior_size_b+input$size_b
    b_alpha = round((input$prior_size_b*input$prior_prop_b)+(input$size_b*input$prop_b),digits=0)
    b_beta = b_trials-b_alpha

    simluation_volume = input$simulation_volume

    sample_a <- data.frame(value=rbeta(simluation_volume,a_alpha,a_beta))
    sample_a$sample <- 'proportion #1'

    sample_b <- data.frame(value=rbeta(simluation_volume,b_alpha,b_beta))
    sample_b$sample <- 'proportion #2'

    samples <- rbind(sample_a,sample_b)
    samples

  })

  posterior_diff <- reactive({

    a_trials = input$prior_size_a+input$size_a
    a_alpha = round((input$prior_size_a*input$prior_prop_a)+(input$size_a*input$prop_a),digits=0)
    a_beta = a_trials-a_alpha

    b_trials = input$prior_size_b+input$size_b
    b_alpha = round((input$prior_size_b*input$prior_prop_b)+(input$size_b*input$prop_b),digits=0)
    b_beta = b_trials-b_alpha

    simluation_volume = input$simulation_volume

    sample_a <- data.frame(value=rbeta(simluation_volume,a_alpha,a_beta))

    sample_b <- data.frame(value=rbeta(simluation_volume,b_alpha,b_beta))

    samples <- data.frame(prop_a=sample_a$value,prop_b=sample_b$value)
    samples$diff <- (samples$prop_b/samples$prop_a)-1
    samples

  })

  summary_prior <- reactive({

    a_trials <- input$prior_size_a
    a_alpha <- round(input$prior_size_a*input$prior_prop_a,digits=0)
    a_beta <-a_trials-a_alpha

    b_trials <- input$prior_size_b
    b_alpha <- round(input$prior_size_b*input$prior_prop_b,digits=0)
    b_beta <-b_trials-b_alpha

    simulation_volume <- input$simulation_volume

    samples <- data.frame(prop_a=rbeta(simulation_volume,a_alpha,a_beta),prop_b=rbeta(simulation_volume,b_alpha,b_beta))
    samples$diff <- (samples$prop_b/samples$prop_a)-1

    lower_diff <- paste(round(quantile(samples$diff,0.025)*100,digits=1),'%',sep='')
    upper_diff <- paste(round(quantile(samples$diff,0.975)*100,digits=1),'%',sep='')

    prop_a <- paste(round((a_alpha/a_trials)*100,digits=1),'%',sep='')
    prop_b <- paste(round((b_alpha/b_trials)*100,digits=1),'%',sep='')

    prior <- data.frame(model='prior',prop_a,prop_b,lower_diff,upper_diff,a_trials,b_trials)

    ## now, the likelihood

    a_trials <- input$size_a
    a_alpha <- round(input$size_a*input$prop_a,digits=0)
    a_beta <-a_trials-a_alpha

    b_trials <- input$size_b
    b_alpha <- round(input$size_b*input$prop_b,digits=0)
    b_beta <-b_trials-b_alpha

    samples <- data.frame(prop_a=rbeta(simulation_volume,a_alpha,a_beta),prop_b=rbeta(simulation_volume,b_alpha,b_beta))
    samples$diff <- (samples$prop_b/samples$prop_a)-1

    lower_diff <- paste(round(quantile(samples$diff,0.025)*100,digits=1),'%',sep='')
    upper_diff <- paste(round(quantile(samples$diff,0.975)*100,digits=1),'%',sep='')

    prop_a <- paste(round((a_alpha/a_trials)*100,digits=1),'%',sep='')
    prop_b <- paste(round((b_alpha/b_trials)*100,digits=1),'%',sep='')

    likelihood <- data.frame(model='likelihood',prop_a,prop_b,lower_diff,upper_diff,a_trials,b_trials)

    ## posterior
    a_trials = input$prior_size_a+input$size_a
    a_alpha = round((input$prior_size_a*input$prior_prop_a)+(input$size_a*input$prop_a),digits=0)
    a_beta = a_trials-a_alpha

    b_trials = input$prior_size_b+input$size_b
    b_alpha = round((input$prior_size_b*input$prior_prop_b)+(input$size_b*input$prop_b),digits=0)
    b_beta = b_trials-b_alpha

    simluation_volume = input$simulation_volume

    sample_a <- data.frame(value=rbeta(simluation_volume,a_alpha,a_beta))

    sample_b <- data.frame(value=rbeta(simluation_volume,b_alpha,b_beta))

    samples <- data.frame(prop_a=sample_a$value,prop_b=sample_b$value)
    samples$diff <- (samples$prop_b/samples$prop_a)-1

    lower_diff <- paste(round(quantile(samples$diff,0.025)*100,digits=1),'%',sep='')
    upper_diff <- paste(round(quantile(samples$diff,0.975)*100,digits=1),'%',sep='')

    prop_a <- paste(round((a_alpha/a_trials)*100,digits=1),'%',sep='')
    prop_b <- paste(round((b_alpha/b_trials)*100,digits=1),'%',sep='')

    posterior <- data.frame(model='posterior',prop_a,prop_b,lower_diff,upper_diff,a_trials,b_trials)

    ## combine
    summary <- rbind(prior,likelihood,posterior)

  })

  output$prior_distributions<-renderPlot({
    ggplot(priors(),aes(x=value,fill=sample)) + geom_density(alpha=0.2) + theme(legend.position = 'none',legend.title=element_blank()) + xlab('Range of Proportion Values') + ylab('Density') + scale_x_continuous(label=scales::percent) + ggtitle('prior')
  })

  output$likelihood_distributions<-renderPlot({
    ggplot(likelihood(),aes(x=value,fill=sample)) + geom_density(alpha=0.2) + theme(legend.position = 'none',legend.title=element_blank()) + xlab('Range of Proportion Values') + ylab('Density') + scale_x_continuous(label=scales::percent) + ggtitle('likelihood')
  })

  output$posterior_distributions<-renderPlot({
    ggplot(posterior(),aes(x=value,fill=sample)) + geom_density(alpha=0.2) + theme(legend.position = 'right',legend.title=element_blank()) + xlab('Range of Proportion Values') + ylab('Density') + scale_x_continuous(label=scales::percent) + ggtitle('posterior')
  })

  output$prior_differential<-renderPlot({
    ggplot(prior_diff(),aes(x=diff)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Deltas Between Proportion A and Proportion B') + ylab('(Proportion B / Proportion A)-1') + scale_x_continuous(label=scales::percent) + geom_vline(xintercept=0,linetype='dashed',colour='red') + ggtitle('prior')
  })

  output$likelihood_differential<-renderPlot({
    ggplot(likelihood_diff(),aes(x=diff)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Deltas Between Proportion A and Proportion B') + ylab('(Proportion B / Proportion A)-1') + scale_x_continuous(label=scales::percent) + geom_vline(xintercept=0,linetype='dashed',colour='red') + ggtitle('likelihood')
  })

  output$posterior_differential<-renderPlot({
    ggplot(posterior_diff(),aes(x=diff)) + geom_density(alpha=0.2) + theme(legend.position = 'bottom',legend.title=element_blank()) + xlab('Range of Possible Deltas Between Proportion A and Proportion B') + ylab('(Proportion B / Proportion A)-1') + scale_x_continuous(label=scales::percent) + geom_vline(xintercept=0,linetype='dashed',colour='red') + ggtitle('posterior')
  })

  output$table_summary<-renderTable({
    ##kable(summary_data(),format = 'html', col.names = c('lower diff','upper diff'),caption = 'summary of two proportions')
    summary_prior()
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
