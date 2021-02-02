#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a proxy global file that is used for dev purposes
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines libraries
#-------- script defines custom functions
#-------- everything in this should make it's way to proper golem files for prod
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#project file performs this task - section is not required

#source golem-helpers===========================================================
# source("./R/golem_utils_server.R")
# source("./R/golem_utils_ui.R")

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(magrittr)
library(readxl) #needed for reading sheets for data
library(janitor) #needed for cleaning names and other munging

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets) #only prettyCheckboxGroup
library(shinycssloaders) #only with_spinner
library(DT)

library(timevis) #makes phase timeimput

library(lubridate)


#probably need to better align me verbage better to industry 




