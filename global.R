library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)

library(tidyverse)

library(cvpiaHabitat)
library(cvpiaFlow)
library(cvpiaTemperature)
library(cvpiaData)

source('modules/about.R')
source('modules/home.R')

metadata_lookup <- read_csv('data/metadata-lookup.csv')

# category data frames
habitat <- read_rds('data/habitat.rds')
flows <- read_rds('data/flows.rds')
temperatures <- read_rds('data/temperatures.rds')
