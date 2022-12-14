################# SIX PATHS ##################
## Main for Shiny App ##

setwd("D:/03 - SIX - PATHS/COURSERA/Data Science __ R/Data_Product_App/Data_Product_Shiny_App/")
library(shiny)
source('ui_sp.R')
source('server_sp.R')

# Run the application
shinyApp(ui = ui_sp, server = server_sp)
