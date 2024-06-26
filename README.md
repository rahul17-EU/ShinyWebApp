# Data Science with R 

The project is a Shiny app for analyzing the quality of red wine using various machine learning models.

## Viewing the Application

The application has been uploaded to an EC2 instance and is available at the following URL: `http://15.237.116.218:3838/final_project/`

Alternatively, you can run the application locally by running the app.R file, for example by opening the file using an R IDE and running the application from there.

## Repository Structure

The repository contains the following files and directories:

- `app.R`: The main R script for the Shiny application.
- `DATA/`: A directory containing the dataset used in the project.
- `Preprocessing and ML model creation/`: This directory contains the R script used for preprocessing the data. It also contains the R script used to create the machine learning models. These machine learning models have been saved as .RDS files and have been saved in the base directory. The shiny app loads these pre-trained models instead of training them again.
- `www/`: A directory containing the images used in the Shiny application. 
