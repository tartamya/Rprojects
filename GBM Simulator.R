library(shiny)

ui <- fluidPage(
  titlePanel("Stock Price Simulator"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Drift (mu):", min = -0.2, max = 0.5, value = 0.1, step = 0.01),
      sliderInput("sigma", "Volatility (sigma):", min = 0.01, max = 1, value = 0.2, step = 0.01),
      sliderInput("mean", "Mean for rnorm:", min = -0.2, max = 0.2, value = 0, step = 0.01),
      sliderInput("sd", "Standard Deviation for rnorm:", min = 0.01, max = 2, value = 0.9, step = 0.01),
      sliderInput("T", "Time period (T in years):", min = 1, max = 5, value = 1, step = 0.1),
      sliderInput("seed", "Random Seed:", min = 1, max = 1000, value = 123, step = 1)
    ),

    mainPanel(
      plotOutput("gbmPlot"),
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$gbmPlot <- renderPlot({
    mu <- input$mu
    sigma <- input$sigma
    mean <- input$mean
    sd <- input$sd
    T <- input$T
    seed <- input$seed

    # Simulation code with user-specified seed and rnorm parameters
    n <- 252
    dt <- T / n
    set.seed(seed)
    z <- rnorm(n, mean = mean, sd = sd)
    cumulative_returns <- cumsum((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
    prices <- exp(cumulative_returns)

    # Plotting GBM
    plot(prices, type = "l",lwd=2.5, col = "blue", xlab = "Time", ylab = "Asset Price",
         main = "Simulated Stock Prices")

    # Add a line for the expected trend (drift)
    trend <- exp(mu * (0:n * dt))
    lines(trend, col = "red", lty = 2)
    abline(h=mean(prices), lwd=2, lty='dashed',col='blue')


    # Add legend
    # legend("topright", legend = c("Simulated Prices", "Expected Trend"),
    #        col = c("blue", "red"), lty = 1:2)
  })

  output$histPlot <- renderPlot({
    mu <- input$mu
    sigma <- input$sigma
    mean <- input$mean
    sd <- input$sd
    T <- input$T
    seed <- input$seed

    # Simulation code for histogram with user-specified seed and rnorm parameters
    n <- 252
    dt <- T / n
    set.seed(seed)
    z <- rnorm(n, mean = mean, sd = sd)
    cumulative_returns <- cumsum((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
    prices <- exp(cumulative_returns)

    # Plotting histogram
    hist(prices, probability=T,col = "skyblue", main = "Distribution of Simulated Prices",
         xlab = "Asset Price", ylab = "Frequency",breaks=40)
    lines(density(prices),lwd=2,col="blue")
    abline(v=tail(prices,1), lwd=2, lty='dashed',col='blue')
    abline(v=mean(prices)+sd(prices), lwd=2, lty='dashed',col=2)
    abline(v=mean(prices)-sd(prices), lwd=2, lty='dashed',col=2)
  })
}

shinyApp(ui, server)
