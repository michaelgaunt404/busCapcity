#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for cleaning and organizing data
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script pulls from multiple data streams
#-------- script performs applies filtering to keep or drop files
#-------- script performs all mapping operations
#-------- should make all indexes or derivative data objects here
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SECTION NAME===================================================================
#use this header to make demarcations/section in code [delete this line]
#short description

#description
#description
#description
function_name <- function(input_1, input_2) {
}

#variable_list===================================================================
#use this header to make demarcations/section in code [delete this line]
#short description
variable_list = read.csv("./data_dev/variables_list.csv")

variable_list_glos = variable_list %>%  
  set_names(c("index", "Simulation Module", "raw_variable_name", 
              "variable_name", "long_name",  "units", "exclude", "description"))

index_resultPlot = variable_list %>%  
  filter(Exclude != "X") 

#module list===================================================================
#use this header to make demarcations/section in code [delete this line]
#short description
simulation_modules = read.csv("./data_dev/simulation_modules.csv") 
colnames(simulation_modules) = c("Simulation Modules", "Details")


#script end=====================================================================
