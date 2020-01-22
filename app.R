#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# LOAD LIBRARIES ----------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(ggthemes)
library(showtext)



# PREP DATA ---------------------------------------------------------------


# > Simulate coin flip ------------------------------------------------------


outcomes <- c("heads", "tails")

flip_history <- vector()

coin_flip <- function() {
    
    flip_history <<- rbind(flip_history, sample(outcomes, 1, replace = T))
    
}


probs <- seq(0, 1, 0.0001)
results <- matrix(nrow = length(probs), ncol = 2)



# Load Roboto font --------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext::showtext_auto()



# UI -------------------------------------------------------------


ui <- dashboardPage(skin = "black",
    
    dashboardHeader(title = "Bayesian Coin Flipping", titleWidth = 350),
    
    dashboardSidebar(collapsed = TRUE),
    
    dashboardBody(
        

# > Summary boxes -----------------------------------------------------------


        fluidRow(
          
                box(width = 12,
                  valueBoxOutput("flips_box"),
                  valueBoxOutput("heads_box"),
                  valueBoxOutput("tails_box")
                )
          
        ),
      


# > Flip coin ------------------------------------------------------


        fluidRow(
          
                 box(
                   
                   height = 220, 
                   
                   title = "Flip your virtual coin",
                   
                   
                   p("Each time you flip a coin, you'll gather more evidence about the true probability of flipping heads"),
                   
                   p("This evidence is combined with your prior (which is that flipping heads is 50%.)"),
                   
                   actionButton("flip_coin", "Flip", icon = icon("coins")),

                    ),
                 

# > Set prior strength ----------------------------------------------------

                
                 box(
                   
                   height = 220,
                   
                   title = "Set strength of your prior",
                   
                   p("Generally the stronger your prior, the more evidence it takes to change your beliefs."),
                   
                   p("Test it out by seeing how slowly and quikly the posterior distribution changes based the choice of a strong or weak prior"),
                   
                   selectInput(
                     "prior_strength",
                     label = "Prior Stength:",
                     choices = c("Weak", "Strong"),
                     selected = "Weak"
                   )
                 )
          
                 

        ),
        

# > Beta plots -----------------------------

     
        fluidRow(
          
          box(
            width = 12, 
            
            title = "Beta distributions for prior, likelihood and posterior",
            
            plotOutput("beta_plots")
          ),
          
    )
  )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output) {
  

# > Set up reactive data ----------------------------------------------------
  

    values <- reactiveValues(df = NULL)

    
    observeEvent(input$flip_coin, {
      coin_flip()
      temp <- flip_history
      values$df <- temp
    })
    
    

# > Summary boxes -----------------------------------------------------------

    output$flips_box <- renderValueBox({
      
      valueBox(
        "Flips",
        length(values$df),
        icon = icon("coins"),
        color = "teal"
      )
      
    })
    
    output$heads_box <- renderValueBox({
      
      valueBox(
        "Heads",
        sum(values$df == "heads"),
        icon = icon("heading"),
        color = "teal"
      )
    })
    
    output$tails_box <- renderValueBox({
      
      valueBox(
        "Tails", 
        sum(values$df == "tails"),
        icon = icon("tumblr"),
        color = "teal"
      )
      
    })

# > Beta plots --------------------------------------------------------------


    output$beta_plots <- renderPlot({
        
        # adapt prior based on user input
      
        if (input$prior_strength == "Weak") {
          prior_alpha <- 10
          prior_beta <- 10} else {
            prior_alpha <- 250
            prior_beta <- 250
          }
      
        # > Set prior ---------------------------------------------------------------
      

        prior_beta_d <- dbeta(probs, prior_alpha, prior_beta)
      
        
        flips <- nrow(values$df)
        heads <- sum(values$df == "heads")
        tails <- sum(values$df == "tails") 
        
        # likliehood
        likl_beta_d <- dbeta(probs, heads, tails)
        
        # Posterior
        posterior_alpha <- prior_alpha + heads
        posterior_beta <- prior_beta + tails
        
        posterior_beta_d <- dbeta(probs, posterior_alpha, posterior_beta)
        
        ggplot() +
            geom_line(aes(x = probs, y = likl_beta_d, color = "likelihood"), size = 2, alpha = 0.6) +
            geom_line(aes(x = probs, y = prior_beta_d, color = "prior"), size = 2, alpha = 0.6) +
            geom_line(aes(x = probs, y = posterior_beta_d, color = "posterior"), size = 2, alpha = 0.6) +
            scale_color_manual(values = c("posterior" = "red", "prior" = "blue", "likelihood" = "black")) +
            labs(y = NULL, x = "Probability", title = NULL) +
            scale_x_continuous(breaks = seq(0, 1, 0.05)) +
            theme(text = element_text(family = "roboto"),
                panel.background = element_rect(fill = "white"),
                axis.line = element_line(colour = "grey"),
                axis.title.x = element_text(size = 16, face = "bold", margin = margin(15, 0, 0, 0)),
                axis.title.y = element_text(size = 16, face = "bold", margin = margin(0, 15, 0, 0)),
                axis.text = element_text(size = 12),
                legend.position = "top"
          )
        
        
    })
    

}

shinyApp(ui, server)

