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

df <- cvpiaTemperature::delta_temps %>% 
  filter(watershed == 'North Delta')

# category data frames
habitat <- read_rds('data/habitat.rds')