library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)

library(tidyverse)

library(cvpiaHabitat)
library(cvpiaFlow)
library(cvpiaTemperature)

source('modules/about.R')
source('modules/home.R')

metadata_lookup <- expand.grid(region = letters[1:8], 
                               category = c('Flow', 'Temperature', 'Habitat'), 
                               data_type = c(1, 2, 4, 5, 8, 9), 
                               metadata_link = c('cat.com', 'dog.com'), 
                               metadata_description = c('blah 1', 
                                                        'blah 2'))

df <- cvpiaTemperature::delta_temps %>% 
  filter(watershed == 'North Delta')


