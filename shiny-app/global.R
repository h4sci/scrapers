## global script for libraries, functions, and data manipulation

library(shiny)
library(shinydashboard)
library(leaflet)
library(spData)
library(dplyr)
library(geometa)
library(ows4R)
library(rgdal)

# Read scraped data
scrape <- read.csv("scraped_data_format.csv", encoding="UTF-8")

# Set owner type and profession as factors
scrape$domino_tipo <- as.factor(scrape$domino_tipo)
scrape$cadastrante_Profissao <- as.factor(scrape$cadastrante_Profissao)