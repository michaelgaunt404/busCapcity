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

index_resultPlot = variable_list %>%  
  filter(Exclude != "X") 





#script end=====================================================================
