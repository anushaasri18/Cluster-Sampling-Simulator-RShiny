library(shiny)
library(dplyr)

# -----------------------------
# SAMPLE STRATIFIED POPULATION
# -----------------------------
strata_data <- data.frame(
  Stratum = paste("Stratum", 1:4),
  Nh = c(200, 150, 100, 50),        # population size
  Sh = c(10, 20, 30, 25),           # standard deviation
  Bias = c(1, 2, 1.5, 2.5),         # sampling bias
  Cost = c(5, 8, 12, 15),           # cost per unit
  Time = c(2, 3, 5, 6)              # time per unit
)

N <- sum(strata_data$Nh)
strata_data$Wh <- strata_data$Nh / N

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  
  titlePanel("Sampling 2: Stratified Sample Size Allocation (Population Mean)"),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("n",
                   "Total Sample Size (n):",
                   value = 100, min = 10),
      
      selectInput("method",
                  "Allocation Method:",
                  choices = c("Proportional",
                              "Neyman",
                              "Optimised (Cost & Time)"))
    ),
    
    mainPanel(
      tableOutput("allocation"),
      verbatimTextOutput("summary"),
      plotOutput("designPlot")
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output) {
  
  allocation <- reactive({
    
    df <- strata_data
    n <- input$n
    
    if (input$method == "Proportional") {
      
      df$nh <- n * df$Wh
      
    } else if (input$method == "Neyman") {
      
      weight <- df$Wh * sqrt(df$Sh^2 + df$Bias^2)
      df$nh <- n * weight / sum(weight)
      
    } else {
      
      weight <- (df$Wh * sqrt(df$Sh^2 + df$Bias^2)) /
        sqrt(df$Cost * df$Time)
      df$nh <- n * weight / sum(weight)
    }
    
    df$nh <- round(df$nh)
    df$TotalCost <- df$nh * df$Cost
    df$TotalTime <- df$nh * df$Time
    
    df
  })
  
  # -----------------------------
  # OUTPUT TABLE
  # -----------------------------
  output$allocation <- renderTable({
    allocation() %>%
      select(Stratum, nh, TotalCost, TotalTime)
  })
  
  # -----------------------------
  # SUMMARY TEXT
  # -----------------------------
  output$summary <- renderText({
    df <- allocation()
    paste(
      "Allocation Method :", input$method, "\n",
      "Total Sample Size :", sum(df$nh), "\n",
      "Total Cost        :", sum(df$TotalCost), "\n",
      "Total Time        :", sum(df$TotalTime)
    )
  })
  
  # -----------------------------
  # DESIGN OF EXPERIMENT PLOT
  # -----------------------------
  output$designPlot <- renderPlot({
    df <- allocation()
    plot(df$TotalCost, df$TotalTime,
         pch = 19,
         col = "blue",
         xlab = "Total Cost per Stratum",
         ylab = "Total Time per Stratum",
         main = "Experimental Design: Cost–Time Tradeoff")
    text(df$TotalCost, df$TotalTime,
         labels = df$Stratum, pos = 4)
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui = ui, server = server)
