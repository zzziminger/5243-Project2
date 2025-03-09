# 5243-Project2

To use the app, users can either upload a file by clicking on the "Choose a Data File" button, which supports multiple formats such as CSV, TXT, TSV, Excel, JSON, and RDF, or select one of the built-in datasets, such as "mtcars" or "iris", from the dropdown menu. Once a file is uploaded or a dataset is selected, the app will display a preview of the first 10 rows of the dataset in a table format. (The maximum file size for uploads is 100MB)

Data cleaning: This section of the R Shiny app handles data cleaning and preprocessing by allowing users to upload datasets in various formats or select built-in ones. The cleaning process includes removing duplicate rows, imputing missing values (median for numeric variables and mode for categorical ones), standardizing column names, encoding categorical variables as factors, and scaling numerical features. An interactive interface enables users to preview both raw and cleaned data dynamically, ensuring consistency and readiness for further analysis.

Exploratory Data Analysis: This section of the R Shiny app provides input options to explore and visualize data interactively. After cleaning and preprocessing a dataset, users can navigate to the "Exploratory Data Analysis" tab to generate visual summaries. The EDA functionality includes the ability to select variables and generate different types of plots such as histograms, boxplots, scatter plots, and bar plots. Users can specify an X variable (required) and an optional Y variable for certain plot types. The app supports interactive filtering, allowing users to focus on specific subsets of the data by selecting ranges for numeric variables or categories for categorical variables. Visualizations are rendered using Plotly, providing interactive features such as zooming and tooltips. In addition to the plots, users can view summary statistics for the filtered data, including measures like mean, median, and frequency counts. This interactive interface allows users to explore their data and analyze trends, patterns, and potential outliers.
