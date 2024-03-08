setwd("C:/Users/rcappaw/Desktop/R/Workflow/igraphEpi-New/Empirical-Network-Classifier")
 # future::plan("multisession", workers = 4) # do parallel
 # options(future.globals.maxSize = 1000 * 1024^2)
# Load required libraries
#options("install.lock"=FALSE)
library(shiny)
library(rsconnect)
library(shinydashboard)
library(randomForest)  
library(xgboost)
library(igraph)
library(dplyr)
library(tidyr)
library(modelr)
library(janitor)
library(shinyFiles)
library(tidymodels)
library(shapviz)
library(patchwork)
library(tidyverse)
library(hstats)
library(ggplot2)
library(future.apply)
library(future)


ui <- source("ui.R")$value
server <- source("server.R")$value
shinyApp(ui = ui, server = server)

