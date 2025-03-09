library(dplyr)
library(tidyr)
library(shiny)
library(DT)
library(readxl)
library(jsonlite)
library(rdflib)
library(MASS)         # For Box-Cox transformation
library(bestNormalize) # For Yeo-Johnson transformation
library(ggplot2) 
library(plotly) # For interactive plots (EDA)

options(shiny.maxRequestSize = 100 * 1024^2)  

# Function to clean data
data_cleaning <- function(df) {
  df <- df %>% distinct()
  df <- df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
  df <- df %>% mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
  colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
  df <- df %>% mutate(across(where(is.character), as.factor))
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- scale(df[num_cols], center = TRUE, scale = TRUE)
  return(df)
}

# Define the UI
ui <- fluidPage(
  titlePanel("Upload, Clean, and Engineer Features"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose a Data File", 
                accept = c(".csv", ".txt", ".tsv", ".xlsx", ".json", ".rdf")),
      selectInput("dataset", "Or choose a built-in dataset:",
                  choices = c("None", "mtcars", "iris"),
                  selected = "None"),
      actionButton("clean_data", "Clean Data"),
      tags$hr(),
      
      h4("Numeric Feature Engineering"),
      uiOutput("fe_num_column_ui"),
      radioButtons("transformation_numeric", "Select Numeric Transformation:",
                   choices = c("Logarithm" = "log", 
                               "Square Root" = "sqrt", 
                               "Square" = "square", 
                               "Difference from Mean" = "diff_mean",
                               "Box-Cox" = "boxcox",
                               "Yeo-Johnson" = "yeojohnson",
                               "Min-Max Normalization" = "minmax")),
      textInput("num_new_col_name", "New Column Name", value = "new_feature"),
      actionButton("apply_numeric", "Apply Numeric Transformation"),
      tags$hr(),
      
      h4("Categorical Feature Engineering"),
      uiOutput("fe_cat_column_ui"),
      radioButtons("cat_transformation", "Select Categorical Transformation:",
                   choices = c("One-Hot Encoding" = "onehot", 
                               "Dummy Encoding" = "dummy")),
      textInput("cat_new_prefix", "New Column Prefix", value="oh"),
      actionButton("apply_categorical", "Apply Categorical Transformation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Data Preview", tableOutput("contents")),
        tabPanel("Cleaned Data Preview", DTOutput("cleaned_table")),
        tabPanel("Feature Engineered Data", DTOutput("fe_table")),
        tabPanel("Exploratory Data Analysis", 
                 uiOutput("eda_controls"),
                 plotOutput("eda_plot"),
                 verbatimTextOutput("summary_stats")
      )
    )
  )
)


# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the file
  data <- reactive({
    if (input$dataset != "None") {
      if (input$dataset == "mtcars") return(mtcars)
      if (input$dataset == "iris") return(iris)
    }
    req(input$file1)
    file <- input$file1$datapath
    ext <- tools::file_ext(file)
    if (ext %in% c("csv", "txt", "tsv")) {
      return(read.table(file, header = TRUE, 
                        sep = ifelse(ext == "csv", ",", ifelse(ext == "txt", "\t", ","))))
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
  
  # Raw data preview (first 10 rows)
  output$contents <- renderTable({
    head(data(), 10)
  })
  
  # Clean data when button is clicked
  cleaned_data <- eventReactive(input$clean_data, {
    req(data())
    data_cleaning(data())
  })
  
  output$cleaned_table <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(autoWidth = TRUE))
  })
  
  # Reactive value to store engineered data; initialized when data is cleaned
  engineered_data <- reactiveVal(NULL)
  observeEvent(cleaned_data(), {
    engineered_data(cleaned_data())
  })
  
  # Dynamic UI: numeric columns available for transformation
  output$fe_num_column_ui <- renderUI({
    req(engineered_data())
    numeric_cols <- names(engineered_data())[sapply(engineered_data(), is.numeric)]
    if (length(numeric_cols) == 0) {
      return(tags$p("No numeric columns available"))
    }
    selectInput("fe_num_column", "Select Numeric Column:", choices = numeric_cols)
  })
  
  # Dynamic UI: categorical columns available for transformation
  output$fe_cat_column_ui <- renderUI({
    req(engineered_data())
    cat_cols <- names(engineered_data())[sapply(engineered_data(), function(x) is.factor(x) || is.character(x))]
    if (length(cat_cols) == 0) {
      return(tags$p("No categorical columns available"))
    }
    selectInput("fe_cat_column", "Select Categorical Column:", choices = cat_cols)
  })
  
  # Apply numeric transformation and update engineered_data
  observeEvent(input$apply_numeric, {
    req(engineered_data())
    df <- engineered_data()
    col <- input$fe_num_column
    trans <- input$transformation_numeric
    new_col_name <- input$num_new_col_name
    req(col, trans, new_col_name)
    
    if (trans == "log") {
      df[[new_col_name]] <- ifelse(df[[col]] > 0, log(df[[col]]), NA)
    } else if (trans == "sqrt") {
      df[[new_col_name]] <- ifelse(df[[col]] >= 0, sqrt(df[[col]]), NA)
    } else if (trans == "square") {
      df[[new_col_name]] <- df[[col]]^2
    } else if (trans == "diff_mean") {
      df[[new_col_name]] <- df[[col]] - mean(df[[col]], na.rm = TRUE)
    } else if (trans == "boxcox") {
      if (all(df[[col]] > 0)) {
        bc <- boxcox(df[[col]] ~ 1, plotit = FALSE)
        lambda <- bc$x[which.max(bc$y)]
        df[[new_col_name]] <- if (abs(lambda) < 1e-3) log(df[[col]]) else ((df[[col]]^lambda - 1)/lambda)
      } else {
        df[[new_col_name]] <- NA
      }
    } else if (trans == "yeojohnson") {
      yj <- yeojohnson(df[[col]])
      df[[new_col_name]] <- yj$x.t
    } else if (trans == "minmax") {
      df[[new_col_name]] <- (df[[col]] - min(df[[col]], na.rm = TRUE)) / (max(df[[col]], na.rm = TRUE) - min(df[[col]], na.rm = TRUE))
    }
    
    engineered_data(df)
  })
  
  # Apply categorical transformation and update engineered_data
  observeEvent(input$apply_categorical, {
    req(engineered_data())
    df <- engineered_data()
    col <- input$fe_cat_column
    trans <- input$cat_transformation
    prefix <- input$cat_new_prefix
    req(col, trans, prefix)
    
    if (trans == "onehot") {
      # One-hot encoding using model.matrix (creates a column for each level)
      dummies <- as.data.frame(model.matrix(~ . - 1, data = df[, col, drop = FALSE]))
      colnames(dummies) <- paste(prefix, colnames(dummies), sep = "_")
      df <- cbind(df, dummies)
    } else if (trans == "dummy") {
      # Dummy encoding: drop one column (c-1 encoding)
      dummies <- as.data.frame(model.matrix(~ . - 1, data = df[, col, drop = FALSE]))
      if (ncol(dummies) > 1) {
        dummies <- dummies[,-1, drop = FALSE]
      }
      colnames(dummies) <- paste(prefix, colnames(dummies), sep = "_")
      df <- cbind(df, dummies)
    }
    
    engineered_data(df)
  })
  
  # Render the table showing the updated engineered data
  output$fe_table <- renderDT({
    req(engineered_data())
    datatable(engineered_data(), options = list(autoWidth = TRUE))
  })
  
  # --------------
  #  EDA Controls
  # --------------
  output$eda_controls <- renderUI({
    req(engineered_data())
    df <- engineered_data()
    
    fluidRow(
      column(4,
             selectInput("eda_var_x", "Select X Variable", choices = names(df)),
             selectInput("eda_var_y", "Select Y Variable (Optional)", choices = c("None", names(df))),
             selectInput("eda_plot_type", "Select Plot Type", 
                         choices = c("Histogram", "Boxplot", "Scatter Plot", "Bar Plot"))
      ),
      column(4,
             checkboxInput("eda_add_filter", "Enable Filter?", value = FALSE),
             conditionalPanel(
               condition = "input.eda_add_filter == true",
               selectInput("eda_filter_col", "Filter Column", choices = names(df)),
               uiOutput("eda_filter_values")
             )
      )
    )
  })
  
  # For dynamic filter options
  output$eda_filter_values <- renderUI({
    req(input$eda_filter_col)
    df <- engineered_data()
    
    vals <- unique(df[[input$eda_filter_col]])
    
    if (is.numeric(df[[input$eda_filter_col]])) {
      sliderInput("eda_filter_range", "Select Range", 
                  min = min(vals, na.rm = TRUE), max = max(vals, na.rm = TRUE),
                  value = c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
    } else {
      selectInput("eda_filter_vals", "Select Values", choices = vals, multiple = TRUE)
    }
  })
  
  # Apply filtered data
  eda_filtered_data <- reactive({
    df <- engineered_data()
    
    if (input$eda_add_filter) {
      col <- input$eda_filter_col
      if (is.numeric(df[[col]])) {
        rng <- input$eda_filter_range
        df <- df %>% filter(df[[col]] >= rng[1], df[[col]] <= rng[2])
      } else {
        vals <- input$eda_filter_vals
        df <- df %>% filter(df[[col]] %in% vals)
      }
    }
    
    df
  })
  
  # Render plot from user selection
  output$eda_plot <- renderPlot({
    req(eda_filtered_data())
    df <- eda_filtered_data()
    x <- input$eda_var_x
    y <- input$eda_var_y
    plot_type <- input$eda_plot_type
    
    if (plot_type == "Histogram") {
      hist(df[[x]], main = paste("Histogram of", x), xlab = x, col = "skyblue", border = "white")
    }
    
    if (plot_type == "Boxplot") {
      boxplot(df[[x]], main = paste("Boxplot of", x), xlab = x, col = "lightgreen")
    }
    
    if (plot_type == "Scatter Plot" && y != "None") {
      plot(df[[x]], df[[y]], main = paste("Scatter Plot of", x, "vs", y), xlab = x, ylab = y, pch = 19, col = "tomato")
    }
    
    if (plot_type == "Bar Plot") {
      barplot(table(df[[x]]), main = paste("Bar Plot of", x), col = "steelblue")
    }
  })

  # Interactive plots
  output$eda_plot <- renderPlotly({
    req(eda_filtered_data())
    df <- eda_filtered_data()
    x <- input$eda_var_x
    y <- input$eda_var_y
    plot_type <- input$eda_plot_type
    
    if (plot_type == "Histogram") {
      plot_ly(df, x = ~get(x), type = "histogram")
    }
    
    if (plot_type == "Boxplot") {
      plot_ly(df, y = ~get(x), type = "box")
    }
    
    if (plot_type == "Scatter Plot" && y != "None") {
      plot_ly(df, x = ~get(x), y = ~get(y), type = "scatter", mode = "markers")
    }
    
    if (plot_type == "Bar Plot") {
      plot_ly(df, x = ~get(x), type = "bar")
    }
  })
  
  # Render summary statistics
  output$summary_stats <- renderPrint({
    req(eda_filtered_data())
    df <- eda_filtered_data()
    
    summary(df)
  })

  # --- End of EDA Controls ---

}

# Run the application 
shinyApp(ui = ui, server = server)
