library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(magrittr)
library(lubridate)

library(tidyverse)

library(cvpiaHabitat)
library(cvpiaFlow)
library(cvpiaTemperature)
library(cvpiaData)

source('modules/about.R')
source('modules/home.R')

metadata_lookup <- read_csv('data/metadata-lookup.csv')

# category data frames
# habitat <- read_rds('data/habitat.rds')
flows <- read_rds('data/flows.rds') %>% 
  filter(year(date) %in% 1980:1999)
temperatures <- read_rds('data/temperatures.rds') %>% 
  filter(year(date) %in% 1980:1999)
habitat <- read_rds("data/habitat_with_scales.rds") %>% 
  filter(year(date) %in% 1980:1999)

pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}

pal <- c("#377eb8", "#1b7837")

