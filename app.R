#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("shiny")
# install.packages("readxl")
# install.packages("jsonlite")
# install.packages("rdflib")


library(shiny)
library(readxl)
library(jsonlite)
library(rdflib)

options(shiny.maxRequestSize = 100 * 1024^2)  

# Define the UI
ui <- fluidPage(
  titlePanel("Upload"),
  
  # File input for uploading data
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose a Data File", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".json", ".rdf")),
      tags$hr(),
      
      # Dropdown for built-in datasets
      selectInput("dataset", "Or choose a built-in dataset:",
                  choices = c("None", "mtcars", "iris"),
                  selected = "None"),
      tags$hr(),
    ),
    
    mainPanel(
      h3("Data Preview"),
      tableOutput("contents")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to read the file based on input
  data <- reactive({
    # If a built-in dataset is selected, return it
    if (input$dataset != "None") {
      if (input$dataset == "mtcars") {
        return(mtcars)
      } else if (input$dataset == "iris") {
        return(iris)
      }
    }
    
    # If file is uploaded, process the uploaded file
    req(input$file1)  # Ensure a file is uploaded
    file <- input$file1$datapath
    ext <- tools::file_ext(file) # Get file extension
    
    # Read the file based on its extension
    if (ext == "csv" || ext == "txt" || ext == "tsv") {
      # Read CSV, TXT, TSV files
      return(read.table(file, header = TRUE, sep = ifelse(ext == "csv", ",", ifelse(ext == "txt", "\t", ","))))
    } else if (ext == "xlsx") {
      # Read Excel files
      return(read_excel(file))
    } else if (ext == "json") {
      # Read JSON files
      return(fromJSON(file))
    } else if (ext == "rdf") {
      # Read RDF files using rdflib
      rdf_data <- rdf_parse(file)
      return(rdf_to_data_frame(rdf_data))  # Convert RDF to data frame
    } else {
      return(NULL)
    }
  })
  
  # Render the data table
  output$contents <- renderTable({
    head(data(), 10)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
