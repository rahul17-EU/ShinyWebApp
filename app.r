# Use the port 3838 to connect to the app via the following link:
# http://15.237.116.218:3838/final_project/

## Red Wine Quality - Shiny App
# Load the package ----
library(shiny)
library(shinythemes)
library(shinyjs)
library(here)
library(tidymodels)
library(factoextra)


# Load data and models ----
data <- read.csv(here("DATA", "winequality-red.csv"))

# scaled data for cluster analysis
data_scaled <- data %>% mutate(across(where(is.numeric), scale))

# Remove the last column of 'data'
data_without_quality <- subset(data, select = -ncol(data))


lm <- readRDS(here("lm_fit_model.rds"))
dt <- readRDS(here("final_tree_fit.rds"))
rf <- readRDS(here("final_rf_fit.rds"))
xgb <- readRDS(here("final_xgb_fit.rds"))

# Preload images
addResourcePath("images", here("www"))

# Create quality as a global variable
quality <- 5


# UI ----
ui <- navbarPage(id = "navbarItem", "Red Wine Quality - Data & Machine Learning", # set navbar title

    # Data Visualization tab ----
    tabPanel("Data Visualization", # set tab title

        fluidPage(
        # javascript, html, css ----
        useShinyjs(),
        theme = shinytheme("paper"),  # set theme
        tags$head( 
            tags$style(HTML( # set custom css
                "
                .container-fluid {
                background-color: #f8f9fa;
                padding-top: 20px;
                }
                
                .well {
                    background-color: #ffffff;
                    border: 1px solid #dee2e6;
                    border-radius: 5px;
                    box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
                    padding: 20px;
                }
                
                .btn-primary {
                    background-color: #007bff;
                    border-color: #007bff;
                }
                
                .btn-primary:hover {
                    background-color: #0069d9;
                    border-color: #0062cc;
                }
                
                .btn-primary:focus, .btn-primary.focus {
                    box-shadow: 0 0 0 0.2rem rgba(0, 123, 255, 0.5);
                }
                
                .plot-output {
                    background-color: #ffffff;
                    border: 1px solid #dee2e6;
                    border-radius: 5px;
                    box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
                    padding: 20px;
                    margin-bottom: 20px;
                }"
            ))
        ),
        titlePanel("Visualizing Wine Chemical Data"), # set title

        sidebarLayout(
            sidebarPanel(
                selectInput("x_var", "Select X-axis Variable:",
                                        choices = c("fixed.acidity", "volatile.acidity", "citric.acid",
                                                                "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                                                "total.sulfur.dioxide", "density", "pH", "sulphates",
                                                                "alcohol")),
                uiOutput("x_slider_ui"),
                selectInput("y_var", "Select Y-axis Variable:",
                                        choices = c("fixed.acidity", "volatile.acidity", "citric.acid",
                                                                "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                                                "total.sulfur.dioxide", "density", "pH", "sulphates",
                                                                "alcohol")),
                uiOutput("y_slider_ui")
            ),
            
            mainPanel(
                # Tool Explanation
                p(HTML("<span style='font-style: italic; color: black;'>This tool provides a comprehensive visualization of the red wine chemical dataset, enabling users to gain a deeper understanding of the target variable 'quality' and its relationship with the input variables. You can select two input variables and their range using the sidebar on the left. The scatterplot illustrates the relationship between these two selected variables. The boxplot displays the distribution of the selected y variable in relation to the target variable 'quality'. The histogram represents the frequency distribution of the selected y variable, with different hues indicating different wine quality levels. This interactive visualization tool allows you to explore and understand the data in a more intuitive and insightful way.</span>")),
                plotOutput("scatter_plot"),
                p("The scatterplot shows the relationship between the selected input variables. For example, when comparing fixed acidity and density, we find that they have a somewhat linear relation, in which higher density generally corresponds to higher fixed acidity."),
                plotOutput("boxplot"),
                p("The boxplots shows the relationship between the selected y axis variable and the wine quality. For example, when looking at the relationship between volatile acidity and quality, we see that lower volatile acidity generally corresponds to higher wine quality. We can also see that the variance in volatile acidity drops with higher wine quality."),
                plotOutput("histogram"),
                p("The histogram shows the frequency distribution of the selected x axis variable and shows the corresponding quality frequency using hues. For example, when looking at density, we find that the distribution of density roughly follows a normal distribution. However, if we look at the hues, we see that higher quality wines, those with darker hue, form their own normal distribution centered around a lower density than the average wine.")
            )
        )
        ),
    ),

    # Cluster Analysis tab ----
    tabPanel("Cluster Analysis", # set tab title

        fluidPage(
        # javascript, html, css ----
        useShinyjs(),
        theme = shinytheme("paper"),  # set theme
        tags$head( 
            tags$style(HTML( # set custom css
                "
                .container-fluid {
                background-color: #f8f9fa;
                padding-top: 20px;
                }
                
                .well {
                    background-color: #ffffff;
                    border: 1px solid #dee2e6;
                    border-radius: 5px;
                    box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
                    padding: 20px;
                }
                
                .btn-primary {
                    background-color: #007bff;
                    border-color: #007bff;
                }
                
                .btn-primary:hover {
                    background-color: #0069d9;
                    border-color: #0062cc;
                }
                
                .btn-primary:focus, .btn-primary.focus {
                    box-shadow: 0 0 0 0.2rem rgba(0, 123, 255, 0.5);
                }
                
                .plot-output {
                    background-color: #ffffff;
                    border: 1px solid #dee2e6;
                    border-radius: 5px;
                    box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
                    padding: 20px;
                    margin-bottom: 20px;
                }"
            ))
        ),
        titlePanel("Wine Quality Analysis with Clustering"), # set title

        sidebarLayout(
        sidebarPanel(
            sliderInput("k_value", "Select Number of Clusters:", min = 2, max = 6, value = 3),
            actionButton("refresh_btn", "Refresh Plot")
        ),
        
        mainPanel(
            p(HTML("<span style='font-style: italic; color: black;'>This tool facilitates a deeper understanding of the red wine chemical dataset through a k-means cluster analysis. On the left, you can select the number of clusters to be formed. The Clustering Visualization graph simplifies the clusters to two dimensions for easy interpretation and visualization. The histogram displays the distribution of wine quality, with different hues indicating the various clusters. This cluster analysis tool allows you to explore and understand the underlying patterns and groupings within the data in a more intuitive and insightful way. K-means clusters are generate with an element of randomness, thus, no clustering will be identical, even if the same number of clusters are generated.</span>")),
            plotOutput("cluster_plot"),
            p("The Clustering Visualization shows the the different clusters by projecting the multi-dimensional data onto a 2D plane. Though some information is lost in this process, it allows the clusters to be more humanly understandable. The different colors and circles allow us to see the different clusters and their rough boundaries and overlaps."),
            plotOutput("quality_plot"),
            p("The histogram shows the distribution of wine quality while indication their associated cluster using hues. For example, when creating 4 or more clusters, we can usually find cluster with a higher average quality than the rest. This tool allows us to see how the different clusters are distributed in terms of wine quality. This gives us an inclination that there are, in fact, patterns in the data that can be exploited to predict wine quality, for example by using machine learning.")
        )
    )
        ),
    ),

    # Machine Learning tab ----
    tabPanel("Visualizing ML Models", # set tab title
        
        fluidPage(
            # javascript, html, css ----
            useShinyjs(),
            theme = shinytheme("paper"),  # set theme
            tags$head( 
                tags$style(HTML( # set custom css
                    "
                    .nav-pills > li.active > a, .nav-pills > li.active > a:focus, .nav-pills > li.active > a:hover {
                        color: #404040;
                    }"
                ))
            ),
            titlePanel("Visualizing Machine Learning Models"), # set title


        # Sidebar layout with input and output definitions ----
        sidebarLayout(

            # Sidebar panel for inputs ----
            sidebarPanel(

                # Input: Selector for choosing model ----
                selectInput(inputId = "model",
                            label = "Choose a ML model:",
                            choices = c("Linear Regression", "Decision Tree", 
                                "Random Forest", "Xgboost")),

                            # Text
                tags$p("Roll a random Wine from the Dataset"),


                # Buttons ----
                actionButton(inputId = "random_wine", label = "Random Wine"),
                actionButton(inputId = "qual3", label = "Quality 3 Wine"),
                actionButton(inputId = "qual4", label = "Quality 4 Wine"),
                actionButton(inputId = "qual5", label = "Quality 5 Wine"),
                actionButton(inputId = "qual6", label = "Quality 6 Wine"),
                actionButton(inputId = "qual7", label = "Quality 7 Wine"),
                actionButton(inputId = "qual8", label = "Quality 8 Wine"),

                # Input: Reset button
                actionButton(inputId = "reset", label = "Reset Values to Median"),


                # Sliders for variable values ----
                # Input: Slider for fixed acidity
                sliderInput(inputId = "fixedacidity",
                            label = "Fixed Acidity:",
                            min = 0,
                            max = 20,
                            value = 7.9,
                            step = 0.01),

                # Input: Slider for volatile acidity
                sliderInput(inputId = "volatileacidity",
                            label = "Volatile Acidity:",
                            min = 0,
                            max = 2,
                            value = 0.52,
                            step = 0.01),

                # Input: Slider for citric acid
                sliderInput(inputId = "citricacid",
                            label = "Citric Acid:",
                            min = 0,
                            max = 1,
                            value = 0.26,
                            step = 0.01),

                # Input: Slider for residual sugar
                sliderInput(inputId = "residualsugar",
                            label = "Residiual Sugar:",
                            min = 0,
                            max = 20,
                            value = 2.2,
                            step = 0.01),

                # Input: Slider for chlorides
                sliderInput(inputId = "chlorides",
                            label = "Chlorides:",
                            min = 0,
                            max = 1,
                            value = 0.08,
                            step = 0.01),

                # Input: Slider for free sulfur dioxide
                sliderInput(inputId = "freesulfurdioxide",
                            label = "Free Sulfur Dioxide:",
                            min = 0,
                            max = 100,
                            value = 14,
                            step = 0.01),

                # Input: Slider for total sulfur dioxide
                sliderInput(inputId = "totalsulfurdioxide",
                            label = "Total Sulfur Dioxide:",
                            min = 0,
                            max = 300,
                            value = 38,
                            step = 0.01),

                # Input: Slider for density
                sliderInput(inputId = "density",
                            label = "Density:",
                            min = 0.98,
                            max = 1.02,
                            value = 0.9968,
                            step = 0.0001),

                # Input: Slider for pH
                sliderInput(inputId = "ph",
                            label = "pH:",
                            min = 2,
                            max = 5,
                            value = 3.31,
                            step = 0.01),

                # Input: Slider for sulphates
                sliderInput(inputId = "sulphates",
                            label = "Sulphates:",
                            min = 0,
                            max = 2.5,
                            value = 0.62,
                            step = 0.01),

                # Input: Slider for alcohol
                sliderInput(inputId = "alcohol",
                            label = "Alcohol:",
                            min = 7,
                            max = 16,
                            value = 10.2,
                            step = 0.01),


            ),


            # Main panel for displaying outputs ----
            mainPanel(

                # Output: Heading ----

                # Tool Explanation
                p(HTML("<span style='font-style: italic; color: black;'>This tool compares the quality of machine learning models for the task of predicting red wine quality based on its chemical properties. You can choose a Machine Learning model from the dropdown menu and input the wine's chemical properties using the sliders. The tool will then predict the quality of the wine using the selected model. You can also roll a random wine from the dataset or select a wine of a specific quality to see how the model performs. Furthermore, we provide brief explanations of the different models, their evaluations, and how they make their predictions.</span>")),
                
                # Output: Selected Model ----
                h3(uiOutput("selected_model")),

                # Output: Plot of the predicted quality ----
                plotOutput("prediction_plot", height = "100px"),

                # Output: Model Description Paragraph ----
                uiOutput("model_description_paragraph"),

                # Output: Title for model explanation ----
                h4("How the Model makes its Predictions"),

                # Output: Conditional Image ----
                uiOutput("explainability_img"),

                # Output: Conditional Paragraph ----
                uiOutput("model_explainability_paragraph")

                )
            )
        )
    )
)


# Server ----
server <- function(input, output, session) {

    # Show a popup message when the app is loaded
    showModal(modalDialog(
        title = "Welcome to our Red Wine Quality App",
        tags$div(
            "This app is perfect for you if you're interested in red wine, data visualization, chemical data, or machine learning. We've combined all these elements to create an interactive tool that starts with a dataset on the chemical properties of red wines and their quality ratings.", 
            tags$br(),
            tags$br(),
            "You can explore this data visually through various graphs and a cluster analysis. If you're interested in machine learning, you can use the tool to predict wine quality based on its chemical properties using various models.", 
            tags$br(),
            tags$br(),
            "We provide explanations on how these models work, their performance metrics, and their suitability for predicting wine quality. We hope you find this tool informative and enjoyable!"
        ),
        easyClose = TRUE
    ))

    observeEvent(input$navbarItem, {

        # Data Visualization tab ----
        if (input$navbarItem == "Data Visualization") {
            # Dynamically update x_slider
            output$x_slider_ui <- renderUI({
                column(12, sliderInput("x_slider", "Select X-axis Values:", 
                                                            min = min(data[[input$x_var]]), 
                                                            max = max(data[[input$x_var]]), 
                                                            value = c(min(data[[input$x_var]]), max(data[[input$x_var]]))))
            })
            
            # Dynamically update y_slider
            output$y_slider_ui <- renderUI({
                column(12, sliderInput("y_slider", "Select Y-axis Values:", 
                                                            min = min(data[[input$y_var]]), 
                                                            max = max(data[[input$y_var]]), 
                                                            value = c(min(data[[input$y_var]]), max(data[[input$y_var]]))))
            })
            
            # Scatter plot
            output$scatter_plot <- renderPlot({
                filtered_data <- data[data[[input$x_var]] >= input$x_slider[1] & data[[input$x_var]] <= input$x_slider[2] &
                                                                data[[input$y_var]] >= input$y_slider[1] & data[[input$y_var]] <= input$y_slider[2], ]
                ggplot(filtered_data, aes_string(x = input$x_var, y = input$y_var)) +
                    geom_point(color = "#007bff") +
                    labs(x = input$x_var, y = input$y_var, title = "Scatter Plot") +
                    theme_minimal()
            })
            
            # Boxplot
            output$boxplot <- renderPlot({
                filtered_data <- data[data[[input$x_var]] >= input$x_slider[1] & data[[input$x_var]] <= input$x_slider[2] &
                                                                data[[input$y_var]] >= input$y_slider[1] & data[[input$y_var]] <= input$y_slider[2], ]
                filtered_data$quality <- as.factor(filtered_data$quality)
                ggplot(filtered_data, aes_string(x = "quality", y = input$y_var)) +
                    geom_boxplot(fill = "#007bff", color = "#007bff", alpha = 0.5) +
                    labs(x = "Quality", y = input$y_var, title = "Boxplot by Quality") +
                    theme_minimal()
            })
            
            # Histogram
            output$histogram <- renderPlot({
                filtered_data <- data[data[[input$x_var]] >= input$x_slider[1] & data[[input$x_var]] <= input$x_slider[2] &
                                                                data[[input$y_var]] >= input$y_slider[1] & data[[input$y_var]] <= input$y_slider[2], ]
                filtered_data$quality <- as.factor(filtered_data$quality)
                ggplot(filtered_data, aes_string(x = input$x_var, fill = "quality")) +
                    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
                    labs(x = input$x_var, y = "Frequency", title = "Histogram") +
                    theme_minimal()
            })
        }


        # Cluster Analysis ----
        else if (input$navbarItem == "Cluster Analysis") {
            # Perform clustering when the button is clicked or when the app is first loaded
            clustering <- eventReactive(input$refresh_btn, {
                kmeans(data_scaled, centers = input$k_value, nstart = 10)
            }, ignoreNULL = FALSE)
            
            # Plot cluster visualization
            output$cluster_plot <- renderPlot({
                fviz_cluster(clustering(), data_scaled, ellipse.type = "norm") +
                    ggtitle("Clustering Visualization")
            })
            
            # Plot quality by cluster
            output$quality_plot <- renderPlot({
                data$cluster <- as.factor(clustering()$cluster)
                ggplot(data, aes(x = quality, fill = cluster)) +
                    geom_bar() +
                    theme_light() +
                    labs(title = "Quality of wine by cluster", x = "Quality", y = "Count") +
                    scale_fill_brewer(palette = "Set1")
            })
        }


        # Visualizing ML Models tab ----
        else if (input$navbarItem == "Visualizing ML Models") {
            # create reactive values ----
            # Create a reactive value that will store whether the user has selected a wine from the dataset
            is_wine_from_dataset <- reactiveVal(FALSE)

            # reactive input data for the modles
            inputData <- reactive({
                data.frame(
                    fixed.acidity = input$fixedacidity,
                    volatile.acidity = input$volatileacidity,
                    citric.acid = input$citricacid,
                    residual.sugar = input$residualsugar,
                    chlorides = input$chlorides,
                    free.sulfur.dioxide = input$freesulfurdioxide,
                    total.sulfur.dioxide = input$totalsulfurdioxide,
                    density = input$density,
                    pH = input$ph,
                    sulphates = input$sulphates,
                    alcohol = input$alcohol
                )
            })


            # adaptive heading and caption ----
            output$selected_model <- renderUI({ 
                h3(paste("Selected Model: ", input$model))
            })
            output$caption <- renderText({
                input$caption
            })


            # Reset button
            # Observe the reset button
            observeEvent(input$reset, {
                # Reset the slider values
                updateSliderInput(session, "fixedacidity", value = 7.9)
                updateSliderInput(session, "volatileacidity", value = 0.52)
                updateSliderInput(session, "citricacid", value = 0.26)
                updateSliderInput(session, "residualsugar", value = 2.2)
                updateSliderInput(session, "chlorides", value = 0.08)
                updateSliderInput(session, "freesulfurdioxide", value = 14)
                updateSliderInput(session, "totalsulfurdioxide", value = 38)
                updateSliderInput(session, "density", value = 0.9968)
                updateSliderInput(session, "ph", value = 3.31)
                updateSliderInput(session, "sulphates", value = 0.62)
                updateSliderInput(session, "alcohol", value = 10.2)

                # is_wine_from_dataset(FALSE)
            })


            # Random Wine and Resst Buttons ----
            # Observe the 'Random Wine' button
            observeEvent(input$random_wine, {
                # Draw a random wine from the dataset
                random_wine <- data[sample(nrow(data), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_wine$density)
                updateSliderInput(session, "ph", value = random_wine$pH)
                updateSliderInput(session, "sulphates", value = random_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_wine$quality
            })

            # Observe the 'Quality 3 Wine' button
            observeEvent(input$qual3, {
                # Draw a random wine of quality 3 from the dataset
                quality_wine <- data[data$quality == 3, ]
                random_quality_wine <- quality_wine[sample(nrow(quality_wine), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_quality_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_quality_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_quality_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_quality_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_quality_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_quality_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_quality_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_quality_wine$density)
                updateSliderInput(session, "ph", value = random_quality_wine$pH)
                updateSliderInput(session, "sulphates", value = random_quality_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_quality_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_quality_wine$quality
            })

            # Observe the 'Quality 4 Wine' button
            observeEvent(input$qual4, {
                # Draw a random wine of quality 4 from the dataset
                quality_wine <- data[data$quality == 4, ]
                random_quality_wine <- quality_wine[sample(nrow(quality_wine), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_quality_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_quality_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_quality_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_quality_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_quality_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_quality_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_quality_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_quality_wine$density)
                updateSliderInput(session, "ph", value = random_quality_wine$pH)
                updateSliderInput(session, "sulphates", value = random_quality_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_quality_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_quality_wine$quality
            })

            # Observe the 'Quality 5 Wine' button
            observeEvent(input$qual5, {
                # Draw a random wine of quality 5 from the dataset
                quality_wine <- data[data$quality == 5, ]
                random_quality_wine <- quality_wine[sample(nrow(quality_wine), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_quality_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_quality_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_quality_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_quality_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_quality_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_quality_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_quality_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_quality_wine$density)
                updateSliderInput(session, "ph", value = random_quality_wine$pH)
                updateSliderInput(session, "sulphates", value = random_quality_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_quality_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_quality_wine$quality
            })

            # Observe the 'Quality 6 Wine' button
            observeEvent(input$qual6, {
                # Draw a random wine of quality 6 from the dataset
                quality_wine <- data[data$quality == 6, ]
                random_quality_wine <- quality_wine[sample(nrow(quality_wine), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_quality_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_quality_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_quality_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_quality_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_quality_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_quality_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_quality_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_quality_wine$density)
                updateSliderInput(session, "ph", value = random_quality_wine$pH)
                updateSliderInput(session, "sulphates", value = random_quality_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_quality_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_quality_wine$quality
            })

            # Observe the 'Quality 7 Wine' button
            observeEvent(input$qual7, {
                # Draw a random wine of quality 7 from the dataset
                quality_wine <- data[data$quality == 7, ]
                random_quality_wine <- quality_wine[sample(nrow(quality_wine), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_quality_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_quality_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_quality_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_quality_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_quality_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_quality_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_quality_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_quality_wine$density)
                updateSliderInput(session, "ph", value = random_quality_wine$pH)
                updateSliderInput(session, "sulphates", value = random_quality_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_quality_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_quality_wine$quality
            })

            # Observe the 'Quality 8 Wine' button
            observeEvent(input$qual8, {
                # Draw a random wine of quality 8 from the dataset
                quality_wine <- data[data$quality == 8, ]
                random_quality_wine <- quality_wine[sample(nrow(quality_wine), 1), ]
                
                # Set the slider values
                updateSliderInput(session, "fixedacidity", value = random_quality_wine$fixed.acidity)
                updateSliderInput(session, "volatileacidity", value = random_quality_wine$volatile.acidity)
                updateSliderInput(session, "citricacid", value = random_quality_wine$citric.acid)
                updateSliderInput(session, "residualsugar", value = random_quality_wine$residual.sugar)
                updateSliderInput(session, "chlorides", value = random_quality_wine$chlorides)
                updateSliderInput(session, "freesulfurdioxide", value = random_quality_wine$free.sulfur.dioxide)
                updateSliderInput(session, "totalsulfurdioxide", value = random_quality_wine$total.sulfur.dioxide)
                updateSliderInput(session, "density", value = random_quality_wine$density)
                updateSliderInput(session, "ph", value = random_quality_wine$pH)
                updateSliderInput(session, "sulphates", value = random_quality_wine$sulphates)
                updateSliderInput(session, "alcohol", value = random_quality_wine$alcohol)

                # is_wine_from_dataset(TRUE)

                # Save the quality of the wine
                quality <<- random_quality_wine$quality
            })


            # reactive prediction and plot ----
            prediction <- reactive({ 
                switch(input$model,
                    "Linear Regression" = predict(lm, new_data = inputData()),
                    "Decision Tree" = predict(dt, new_data = inputData()),
                    "Random Forest" = predict(rf, new_data = inputData()),
                    "Xgboost" = predict(xgb, new_data = inputData())
                )
            })

            observeEvent(inputData(), {
                # Get the current slider values
                current_values <- inputData()[1, ]

                # Compare current_values with each row in data_without_quality
                matches <- apply(data_without_quality, 1, function(row) {
                    all(abs(row - current_values) <= 0.1 * abs(current_values))
                })

                # If all rows in data_without_quality are identical to current_values, set is_wine_from_dataset to TRUE
                # Otherwise, set it to FALSE
                is_wine_from_dataset(any(matches))
            })

            output$prediction_plot <- renderPlot({
                pred_df <- data.frame(
                    pred = prediction()$.pred,
                    quality = quality
                )

                # Create a data frame for the gradient background
                gradient_df <- data.frame(x = seq(1, 8, by = 0.1), y = 1)

                if (pred_df$pred > 8 || pred_df$pred < 1) {
                    if (pred_df$pred > 8) {
                        ggplot(gradient_df, aes(x = x, y = y)) +
                            geom_tile(aes(fill = x), width = 0.1, height = 2) +
                            scale_fill_gradient(low = "blue", high = "orange") +
                            annotate("text", x = 4.5, y = 1, label = "Predicted value exceeds the quality cap") +
                            labs(title = "Quality predicted by the selected Model. Shows Actual Quality if similar Wine in Dataset.") +
                            scale_x_continuous(breaks = 1:8, limits = c(1, 8), expand = c(0, 0)) +
                            theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank()) +
                            guides(fill=FALSE)
                    }
                    else {
                        ggplot(gradient_df, aes(x = x, y = y)) +
                            geom_tile(aes(fill = x), width = 0.1, height = 2) +
                            scale_fill_gradient(low = "blue", high = "orange") +
                            annotate("text", x = 4.5, y = 1, label = "Predicted value is below the quality cap") +
                            labs(title = "Quality predicted by the selected Model. Shows Actual Quality if similar Wine in Dataset.") +
                            scale_x_continuous(breaks = 1:8, limits = c(1, 8), expand = c(0, 0)) +
                            theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank()) +
                            guides(fill=FALSE)
                    }
                } else if (is_wine_from_dataset()) {
                    ggplot(gradient_df, aes(x = x, y = y)) +
                        geom_tile(aes(fill = x), width = 0.1, height = 2) +
                        scale_fill_gradient(low = "blue", high = "orange") +
                        geom_point(data = pred_df, aes(y = 1, x = pred), size = 4) +
                        geom_text(data = pred_df, aes(y = 1, x = pred, label = paste("Predicted Quality:", round(pred, 2))), vjust = -1) +
                        geom_point(data = pred_df, aes(y = 1, x = quality), size = 4, color = "darkred") +
                        geom_text(data = pred_df, aes(y = 1, x = quality, label = paste("Actual Quality:", quality)), vjust = 2, color = "darkred") +
                        geom_text(data = pred_df, aes(y = 1, x = (pred + quality) / 2, label = paste("Error: ", round(pred - quality, 2))), vjust = 3.2, color = "darkred") +
                        labs(title = "Quality predicted by the selected Model. Shows Actual Quality if similar Wine in Dataset.") +
                        scale_x_continuous(breaks = 1:8, limits = c(1, 8), expand = c(0, 0)) +
                        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank()) +
                        guides(fill=FALSE)
                } else {
                    ggplot(gradient_df, aes(x = x, y = y)) +
                        geom_tile(aes(fill = x), width = 0.1, height = 2) +
                        scale_fill_gradient(low = "blue", high = "orange") +
                        geom_point(data = pred_df, aes(y = 1, x = pred), size = 4) +
                        geom_text(data = pred_df, aes(y = 1, x = pred, label = paste("Predicted Quality:", round(pred, 2))), vjust = -1) +
                        labs(title = "Quality predicted by the selected Model.  Shows Actual Quality if similar Wine in Dataset.") +
                        scale_x_continuous(breaks = 1:8, limits = c(1, 8), expand = c(0, 0)) +
                        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank()) +
                        guides(fill=FALSE)
                }
            })


            # Model Description and Explainability ----
            output$model_description_paragraph <- renderUI({ # model description ----
                switch(input$model,
                    "Linear Regression" = {
                        list(
                            p("Linear Regression is a statistical model that attempts to predict a dependent variable using one or more independent variables. It assumes a linear relationship between the input variables and the single output variable (in our case the wine quality). More specifically, that target output variable can be calculated from a combination of the input variables."),
                            tableOutput("linear_regression_metrics"),
                            p("The table above shows the Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and R-squared (Rsq) values for the Linear Regression model. It shows that on average, the model's predictions are off by 0.504 quality points. RMSE is a measure of how spread out these residuals are. In other words, it tells you how concentrated the data is around the line of best fit. The closer to 0, the better. Rsq is a measure of how well the model fits the data. It ranges from 0 to 1, with 1 being a perfect fit. An Rsq of 0.355 means that the model explains 35.5% of the variance in the data.")
                        )
                    },
                    "Decision Tree" = {
                        list(
                            p("A Decision Tree is a flowchart-like structure in which each internal node represents a feature (or attribute), each branch represents a decision rule, and each leaf node represents an outcome. The topmost node in a decision tree is known as the root node. The model learns to partition on the basis of the attribute value. It partitions recursively and creates a flowchart-like structure which mimics human level thinking. That is why decision trees are easy to understand and interpret."),
                            tableOutput("decision_tree_metrics"),
                            p("The table above shows the Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and R-squared (Rsq) values for the Decision Tree model. The MAE and RMSE values are higher than those of the Linear Regression model, indicating that the Decision Tree model's predictions are less accurate. The Rsq value is NaN, which indicates that the model fails to explain any of the variation in wine quality. This could be due to overfitting, underfitting, or other issues that we will discuss later.")
                        )
                    },
                    "Random Forest" = {
                        list(
                            p("Random Forest is a popular machine learning algorithm that belongs to the supervised learning technique. It can be used for both Classification and Regression problems in ML. It is based on the concept of ensemble learning, which is a process of combining multiple algorithms to solve a particular problem. Random Forest algorithm creates decision trees on data samples and then gets the prediction from each of them and finally selects the best solution by means of voting. It is an ensemble method which is better than a single decision tree because it reduces the over-fitting by averaging the result."),
                            tableOutput("random_forest_metrics"),
                            p("The table above shows the Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and R-squared (Rsq) values for the Random Forest model. The MAE and RMSE values are lower than those of the other models, indicating that the Random Forest model's predictions are more accurate. The Rsq value is 0.507, which means that the model explains 50.7% of the variance in the data. This is the highest Rsq value among all the models, indicating that the Random Forest model is the best fit for our data.")
                        )
                    },
                    "Xgboost" = {
                        list(
                            p("XGBoost stands for eXtreme Gradient Boosting. It is a decision-tree-based ensemble Machine Learning algorithm that uses a gradient boosting framework. Rather than training all the models in isolation of one another, boosting trains models in succession, with each new model being trained to correct the errors made by the previous ones. Models are added sequentially until no further improvements can be made."),
                            tableOutput("xgboost_metrics"),
                            p("The table above shows the Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and R-squared (Rsq) values for the XGBoost model. The MAE and RMSE values are simliar to those of the Random Forest model, indicating that the XGBoost model's predictions are similarly accurate. The Rsq value is 0.499, which means that the model explains 49.9% of the variance in the data.")
                        )
                    }
                )
            })

            output$linear_regression_metrics <- renderTable({
                data.frame(
                    Metric = c("MAE", "RMSE", "Rsq"),
                    Value = c(0.504, 0.650, 0.355),
                    stringsAsFactors = FALSE
                )
            })

            output$decision_tree_metrics <- renderTable({
                data.frame(
                    Metric = c("MAE", "RMSE", "Rsq"),
                    Value = c(0.683, 0.807, "NaN"),
                    stringsAsFactors = FALSE
                )
            })

            output$random_forest_metrics <- renderTable({
                data.frame(
                    Metric = c("MAE", "RMSE", "Rsq"),
                    Value = c(0.418, 0.573, 0.507),
                    stringsAsFactors = FALSE
                )
            })

            output$xgboost_metrics <- renderTable({
                data.frame(
                    Metric = c("MAE", "RMSE", "Rsq"),
                    Value = c(0.421, 0.578, 0.499),
                    stringsAsFactors = FALSE
                )
            })

            # Server-side element for the conditional image
            output$explainability_img <- renderUI({
                # Check the selected model
                switch(input$model,
                    "Linear Regression" = tags$figure(
                        img(src = "images/lm_explainability.png", alt = "Linear Regression Explainability", width = "100%"),
                        tags$figcaption(style = "font-style: italic; color: black;", "Linear Function created by Linear Regression Model")
                    ),
                    "Decision Tree" = tags$figure(
                        img(src = "images/dt_explainability.png", alt = "Decision Tree Explainability", width = "100%"),
                        tags$figcaption(style = "font-style: italic; color: black;", "Decision 'Stump' created by Decision Tree Model")
                    ),
                    "Random Forest" = tags$figure(
                        img(src = "images/rf_explainability.png", alt = "Random Forest Explainability", width = "100%"),
                        tags$figcaption(style = "font-style: italic; color: black;", "Feature Importance Plot for Random Forest Model")
                    ),
                    "Xgboost" = tags$figure(
                        img(src = "images/xgboost_explainability.png", alt = "XGBoost Explainability", width = "100%"),
                        tags$figcaption(style = "font-style: italic; color: black;", "Feature Importance Plot for XGBoost Model")
                    )
                )
            })

            # Server-side element for the conditional paragraph
            output$model_explainability_paragraph <- renderUI({
                # Check the selected model
                switch(input$model,
                    "Linear Regression" = {
                        p("The Linear Regression model presumes a linear relationship between the predictors and the target variable. In this case, the largest weights were given to the factors of Alcohol, Volatile Acidity, and Sulphates, indicating their perceived importance in predicting wine quality.", tags$br(), tags$br(),
                        "However, applying basic understanding of wine quickly leads us to discard this model. For example, due to its simple linear assumptions, the model believes higher alcohol content to strictly correlate with higher quality, which is obviously not the case. Similarly, we can assume that the relationships of the other predictors to wine quality are also non-linear. This false ground assumption explains the inaccuracy of the model.")
                    },
                    "Decision Tree" = {
                        p("Decision Tree models operate by partitioning the data into subsets based on feature values. These partitions are made in a hierarchical manner, where each decision node in the tree corresponds to a feature that best splits the data at that level. This process continues until a stopping criterion is met, resulting in a set of terminal nodes or 'leaves'. Each leaf represents a decision outcome, which is the mean target value for the observations within that leaf.", tags$br(), tags$br(),
                        "In our case, however, the optimal Decision Tree model turned out to be a 'decision stump'—a tree with a depth of one. This means that the model makes its predictions based solely on a single feature split. Despite running a grid search to optimize the model parameters, the best performing model was one that always predicted the average quality of the dataset, which is 5.6. This is reflected in the missing R-squared value, indicating that the model does not explain any of the variance in wine quality. Therefore, while Decision Trees can be powerful predictive models, in this instance it appears that a more complex model may be required to accurately predict wine quality.")
                    },
                    "Random Forest" = p("Random Forest models are an ensemble learning method that operates by constructing a multitude of decision trees at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees. Random forests correct for decision trees' habit of overfitting to their training set. They are more complex models and can be more difficult to interpret compared to simpler models like Linear Regression or Decision Trees.", tags$br(), tags$br(),
                                        "From the feature importance graph above, we can see that the Random Forest model considers Alcohol, Volatile Acidity, and Sulphates to be the most important features in predicting wine quality. However, it's important to note that feature importance in a Random Forest model is a relative measure. It shows the importance of each feature in relation to the other features in the model, rather than an absolute measure of their importance. Therefore, while these features are considered important by the model, via the feature importance method, we cannot know how exactly the model uses these features to make its predictions, rendering the model more of a black box than the simpler models."),
                    "Xgboost" = p("XGBoost, short for 'Extreme Gradient Boosting', is a powerful machine learning algorithm that is based on the gradient boosting framework. XGBoost builds upon the concept of boosting weak learners, and it does so in an additive and sequential manner. This makes XGBoost a more complex and sophisticated model, which can be more challenging to interpret compared to simpler models.", tags$br(), tags$br(),
                                "Looking at the feature importance graph above, we can see that the XGBoost model assigns a very high importance to Fixed Acidity and almost no importance to Alcohol. This is in stark contrast to the Random Forest model, which considered Alcohol to be one of the most important features. However, interpreting feature importance in XGBoost can be complex due to the way the model is constructed. The model's predictions are based on a multitude of decision trees, where each tree is dependent on the errors of the previous one. This sequential nature makes it difficult to isolate the effect of a single feature on the target variable, as its impact is intertwined with the effects of other features across multiple trees. Thus, this model too, is more of a black box than the simpler models.")
                )
            })

        }

        # Add more conditions for additional navbar items
    })
}


shinyApp(ui = ui, server = server)