# README: R Shiny Data Processing Application
## Overview
This R Shiny application allows users to upload, clean, engineer features, and perform exploratory data analysis (EDA) on datasets. Users can upload files in multiple formats, apply data cleaning operations, transform numerical and categorical variables, and visualize data interactively.
## Features
- User Guide: A built-in guide provides clear instructions and tooltips to assist users in navigating the app.
- Upload Datasets: Supports CSV, TXT, TSV, Excel, JSON, and RDF file formats, or select built-in datasets (`mtcars`, `iris`).
- Data Cleaning: Removes duplicates, imputes missing values, standardizes column names, encodes categorical variables, and scales numerical features.
- Feature Engineering:

  Numeric Transformations: Logarithm, Square Root, Square, Difference from Mean, Box-Cox, Yeo-Johnson, Min-Max Normalization.

  Categorical Transformations: One-Hot Encoding, Dummy Encoding.
- Exploratory Data Analysis (EDA): Generates interactive plots (histograms, boxplots, scatter plots, bar plots) and displays summary statistics.
## Prerequisites
Ensure you have R and the following R packages installed:

`install.packages(c("shiny", "dplyr", "tidyr", "DT", "readxl", "jsonlite", "rdflib", "MASS", "bestNormalize", "ggplot2", "plotly", "shinyBS"))`
## How to Run the Application
### Clone the Repository
`git clone <repository-url>`

`cd <repository-folder>`
### Open R or RStudio
### Run the App
`library(shiny)`

`runApp("app.R")`
### Access the App
- Once the app is running, it will open in your default web browser.
- Follow the User Guide tab for instructions on using the app.
## File Upload Instructions
- The maximum file size for uploads is 100MB.
- Ensure the dataset is properly formatted and does not contain corrupted values.
## Troubleshooting
- If dependencies are missing, reinstall them using:
  `install.packages("package-name")`
- If the app does not start, check for any error messages in the R console
