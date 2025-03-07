#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load necessary libraries
library(dplyr)
library(tidyr)
library(shiny)
library(DT)
library(readxl)
library(jsonlite)
library(rdflib)

options(shiny.maxRequestSize = 100 * 1024^2)  

# Function to clean data
data_cleaning <- function(df) {
  # Remove duplicate rows
  df <- df %>% distinct()
  
  # Identify and handle missing values
  # For numeric columns, impute with median; for categorical, impute with mode
  df <- df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
  df <- df %>% mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
  
  # Standardize column names (convert to lowercase and replace spaces with underscores)
  colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
  
  # Encode categorical variables using factor levels
  df <- df %>% mutate(across(where(is.character), as.factor))
  
  # Scale numerical variables (min-max scaling)
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- scale(df[num_cols], center = TRUE, scale = TRUE)
  
  return(df)
}

# Define the UI
ui <- fluidPage(
  titlePanel("Upload and Clean Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose a Data File", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".json", ".rdf")),
      selectInput("dataset", "Or choose a built-in dataset:",
                  choices = c("None", "mtcars", "iris"),
                  selected = "None"),
      actionButton("clean_data", "Clean Data")
    ),
    
    mainPanel(
      h3("Raw Data Preview"),
      tableOutput("contents"),
      h3("Cleaned Data Preview"),
      DTOutput("cleaned_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  data <- reactive({
    if (input$dataset != "None") {
      if (input$dataset == "mtcars") {
        return(mtcars)
      } else if (input$dataset == "iris") {
        return(iris)
      }
    }
    
    req(input$file1)
    file <- input$file1$datapath
    ext <- tools::file_ext(file)
    
    if (ext == "csv" || ext == "txt" || ext == "tsv") {
      return(read.table(file, header = TRUE, sep = ifelse(ext == "csv", ",", ifelse(ext == "txt", "\t", ","))))
    } else if (ext == "xlsx") {
      return(read_excel(file))
    } else if (ext == "json") {
      return(fromJSON(file))
    } else if (ext == "rdf") {
      rdf_data <- rdf_parse(file)
      return(rdf_to_data_frame(rdf_data))
    } else {
      return(NULL)
    }
  })
  
  output$contents <- renderTable({
    head(data(), 10)
  })
  
  cleaned_data <- eventReactive(input$clean_data, {
    req(data())
    data_cleaning(data())
  })
  
  output$cleaned_table <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(autoWidth = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
