library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

ui <- fluidPage(
  titlePanel("R Shiny Assessment - Cumulative Paid Claims Triangle"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Tab"),
      br(),
      fileInput("FileInput", "Choose Claims Data (Input) File (.xlsx)"),
      sliderInput("tfactor", "Tail Factor", value = 1.1, min = 0, max = 3, step = 0.1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input Claims Data", tableOutput("claimsData")),
        tabPanel("Cumulative Paid Claims", tableOutput("cumTriangle")),
        tabPanel("Graph", plotOutput("cumPlot"))
      )
    )
  )  
)

server <- function(input, output, session) {
  
  claims_data <- reactive({
    req(input$FileInput)
    read_excel(input$FileInput$datapath)
  })
  
  tfactor <- reactive({
    input$tfactor
  })
  
  cumulative_tri <- reactive({
    req(claims_data())
    
    cumulative_data <- claims_data() %>%
      group_by(Loss_Year) %>%
      arrange(Loss_Year, Development_Year) %>%
      mutate(Cumulative_Claims = cumsum(Paid_Claims_Amount)) %>%
      select(Loss_Year, Development_Year, Cumulative_Claims) %>%
      pivot_wider(names_from = Development_Year, 
                  values_from = Cumulative_Claims, 
                  values_fill = list(Cumulative_Claims = NA))
    
    # Ensure there is a column for Development Year 4
    if (!"4" %in% colnames(cumulative_data)) {
      cumulative_data <- cumulative_data %>%
        mutate(`4` = NA_real_)
    }
    
    # Projecting future paid claims
    for (i in 3:ncol(cumulative_data)) {
      if (i == 3) {
        # Find the last row and calculate factor for development year 2
        r <- max(which(!is.na(cumulative_data[, i])))
        factordy2 <- sum(unlist(cumulative_data[1:r, i]), na.rm = TRUE) / sum(unlist(cumulative_data[1:r, i - 1]), na.rm = TRUE)
        
        cumulative_data[is.na(cumulative_data[, i]), i] <- 
          as.integer(as.numeric(unlist(cumulative_data[is.na(cumulative_data[, i]), i - 1])) * factordy2)
        
      } else if (i!= ncol(cumulative_data)) {
        # Calculate factor for development year 3
        factordy3 <- sum(unlist(cumulative_data[1, i]), na.rm = TRUE) / sum(unlist(cumulative_data[1, i - 1]), na.rm = TRUE)
        
        cumulative_data[is.na(cumulative_data[, i]), i] <- 
          as.integer(as.numeric(unlist(cumulative_data[is.na(cumulative_data[, i]), i - 1])) * factordy3)
        
      } else {
        # Apply the tail factor to the last development year
        cumulative_data[, i] <- unlist(cumulative_data[, i - 1]) * tfactor()
      }
    }
    
    # Formatting numbers and row title
    colnames(cumulative_data)[1] <- "Loss Year"
    colnames(cumulative_data)[2:ncol(cumulative_data)] <- paste0("Development Year ", 1:(ncol(cumulative_data) - 1))
    
    cumulative_data
  })
  
  plot_data <- reactive({
    cumulative_data <- cumulative_tri() %>%
      pivot_longer(cols = -"Loss Year", 
                   names_to = "Development_Year", 
                   values_to = "Cumulative_Claims") %>%
      mutate(Development_Year = as.numeric(gsub("Development Year ", "", Development_Year)),
             Cumulative_Claims = as.numeric(Cumulative_Claims))
    
    ggplot(cumulative_data, aes(x = Development_Year, 
                                y = Cumulative_Claims, 
                                color = factor(`Loss Year`))) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 3) +
      geom_text(aes(label = scales::comma(Cumulative_Claims)), 
                vjust = -1.5, size = 4, show.legend = FALSE) +
      
      labs(title = "Cumulative Paid Claims Over Development Years", 
           x = "Development Year", 
           y = "Cumulative Claims Amount ($)", 
           color = "Loss Year") +
      
      theme_bw(base_size = 14) + 
      theme(legend.position = "bottom") +
      
      scale_y_continuous(labels = scales::comma, limits = c(min(cumulative_data$Cumulative_Claims), 
                                                            max(cumulative_data$Cumulative_Claims, na.rm = TRUE) * 1.1))
  })
  
  # Render claims data table
  output$claimsData <- renderTable({
    claims_data()
  }, align = "c", digits = 0)
  
  # Render cumulative triangle table
  output$cumTriangle <- renderTable({
    cumulative_tri()
  }, align = "c", digits = 0)
  
  # Render plot
  output$cumPlot <- renderPlot({
    plot_data()
  })
}

shinyApp(ui, server)