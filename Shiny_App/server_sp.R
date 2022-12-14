#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
server_sp <- function(input, output, session) {
  library(reshape2) 
  ### Parameters
  N_Eq <-10 # Number of equities
  t <- 1000 # Time/Steps (Number of observed price by asset)
  mu <- 0.05
  sigma <- 0.2
  S0 <- 100
  
  gbm_loop <- function(N_Eq = 100, t = 25, mu = 0, sigma = 0.1, S0 = 100, dt = 1./365) {
    gbm <- matrix(ncol = N_Eq, nrow = t)
    for (simu in 1:N_Eq) { gbm[1, simu] <- S0
    for (day in 2:t) {epsilon <- rnorm(1)
    dt = 1 / 365
    gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) 
                                               * dt + sigma * epsilon * sqrt(dt))}}
    return(gbm)}
  
  ## generate portfolio
  df_eq_start <- reactive({gbm_loop(input$N_Eq, input$t_value, 
                                    input$mu, input$sigma, S0)})
  
  ## Compute portfolio return and volatility 
  # Return
  output$ptf_return <- renderText({
    df_eq <- df_eq_start()
    ## Set portfolio weight (Equal weighted portfolio in this example)
    pf_eq_w <- rep(1/dim(df_eq)[2],dim(df_eq)[2])
    ## Compute continuously compounded return of assets 
    df_eq_return <- diff(log(df_eq), lag=1)
    paste0(round(100*sum(df_eq_return * pf_eq_w)), 2)})
  
  output$ptf_volatility <- renderText({
    df_eq <- df_eq_start()
    ## Set portfolio weight (Equal weighted portfolio in this example)
    pf_eq_w <- rep(1/dim(df_eq)[2],dim(df_eq)[2])
    ## Compute continuously compounded return of assets 
    df_eq_return <- diff(log(df_eq), lag=1)
    sum(df_eq_return * pf_eq_w)
    # Portfolio Volatility
    df_eq_cov <- cov(df_eq_return)
    paste0(round(100*sqrt(t(pf_eq_w) %*% df_eq_cov %*% pf_eq_w), 2), "%")})
  
  
  
  output$Assets_Plots <- renderPlotly({
    df_eq <- df_eq_start()
    ## Set Colors for graph
    cols = rainbow(N_Eq)
    df_eq_plotly <- as.data.frame(df_eq)
    names(df_eq_plotly) <- paste0("Asset_ID_", 1:dim(df_eq_plotly)[2])
    
    df_eq_plotly$x_col <- (1:dim(df_eq_plotly)[1])
    df_eq_plotly_melt <- melt(df_eq_plotly, id = "x_col") 
    
    plot_ly(df_eq_plotly_melt, x=~x_col, y=~value, group_by=~variable, color=~variable, 
            mode = "lines") %>% layout(title = 'Dynamic of Brownian Asset price', 
                                       xaxis = list(title = 'Nb_Simul'), 
                                       font=t,  
                                       yaxis = list(title = 'Asset_Price'), 
                                       legend = list(title=list(text='Asset_ID')))
    
  })
}

