#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui_sp <- fluidPage(
  # Application title
  titlePanel(" Portfolio Simulation From Brownian Asset prices"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Value of mu:", min = 0.01,
                  max = 0.05, value = 0.02),
      sliderInput("sigma", "Value of sigma:", min = 0.1,
                  max = 0.5, value = 0.35),
      sliderInput("N_Eq", "Number of Asset:", min = 4,
                  max = 20, value = 10),
      sliderInput("t_value", "Series_lenght:", min = 100,
                  max = 1000, value = 500)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("Assets_Plots"),
      
      h3("Derived Portfolio Quick Indicators : "),
      p(paste0("Mean of derived (equal weighted) portfolio is : ")),
      textOutput("ptf_return"),
      p(paste0("Volatility of derived (equal weighted) portfolio is : ")),
      textOutput("ptf_volatility")
    )
  )
)

