#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for mapping.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script pulls from multiple data streams
#-------- script performs applies filtering to keep or drop files
#-------- script performs all mapping operations
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(magrittr)
# if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
# setwd("~/")
# rstudioapi::getSourceEditorContext()$path %>%
#   as.character() %>%
#   gsub("/R.*","\\1", .) %>%
#   path.expand() %>%
#   setwd()
# }
# getwd()
#sourcing utility script~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source("global.R")

#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#tabular sources================================================================
#===============================================================================
path = "./data/MasterDeliverablesList.xlsx"
del = lapply(excel_sheets(path), read_excel, path = path)[1:8]  %>%  
  rbindlist %>%  
  data.table() %>%  
  .[,`:=`(class = str_remove_all(Code, "[:punct:]") %>%  
            str_remove_all("[:digit:]") %>%  
            as.factor())]

# del = del[,-c("Description")]

del_main = del[level == 1]
del_not_main = del[level != 1]
del_group = del[level == 2]
del_deliverable = del[level == 3 & 
                        `level description` != "Milestone",]
del_milestone = del[level == 3 & 
                        `level description` == "Milestone"]

hey = del_main[, .(Title, class)] %>%  
  merge.data.table(., del_group, by = "class") %>% 
  .[, .(Title.x, Title.y)]

listter = split(hey$Title.y, hey$Title.x)

phase = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "phases") %>%
  remove_empty("rows") %>%  
  mutate(input_index = str_glue("{Phase_name} ({Phase_percent})"), 
         `Notes (Phase)` = str_glue('<a href="#" onclick="alert(\'{`Notes (Phase)`}\');">Click for Description</a>'),
         `Notes (Milestone)` = str_glue('<a href="#" onclick="alert(\'{`Notes (Milestone)`}\');">Click for Description</a>'), 
         button = str_glue('<button onclick="alert(\`{`Notes (Milestone)`}\');" >Click for Description</button>')) %>%  
  data.table()

deliverables = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "deliverables", skip = 1) %>%
  remove_empty("rows") %>%  
  select(-starts_with("DM")) %>% 
  mutate(Note = str_glue('<a href="#" onclick="alert(\`{Notes}\');">Click for Description</a>')) %>% 
  data.table()

task_list = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "tasks", skip = 2) %>%
  remove_empty(c("rows", "cols"))

task_list_lookup = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "tasks") %>%
  head(2) %>% 
  remove_empty(c("rows", "cols")) 

task_list_lookup = data.frame(Percent = task_list_lookup[1, -1] %>%  as.numeric(),
                              Phase = task_list_lookup[2, -1] %>%  as.character())

task_list_data = task_list %>%  
  pivot_longer(cols = -Work_Type, names_to = "Phase" ) %>%  
  na.omit() %>% 
  merge(., task_list_lookup, by = "Phase") %>% 
  arrange(Work_Type, Percent) %>%  
  data.table()
































# input = unique(phase$input_index)[-1]
# dflt_timeline_data(input)

dflt_timeline_data = function(input){
  timevisDataGroups <- data.frame(
    id = c("phases", "milestones", "deliverable"),
    content = c("Design Phases", "Milestones", "Deliverables")
  )
  
  tmp_data = phase %>% 
    .[input_index %in% input, 1] %>% 
    unique() %>% 
    rbind(data.frame(Phase_name = "Task Order Development"),.) %>%
    mutate(id = rownames(.) %>% as.numeric(),
           phase_start = Sys.Date() + months(id+1),
           phase_end = phase_start + months(1) - days(1)) %>%  
    merge(., phase[,c(1,3,5,6,7)] %>% 
            select(!starts_with("Not")), by = "Phase_name") %>% 
    rename(phase_content = "Phase_name", 
           milestone_content = "Milestone") %>% 
    mutate(milestone_start = phase_start+((phase_end-phase_start)*(Due/Phase_percent_end)),
           milestone_end = NA) %>%  
    select(contains("phase_"),  contains("milestone_")) %>%  
    select(-Phase_percent_end) %>%  
    merge(., deliverables[, c("Milestone", "Deliverable")], by.x = "milestone_content", by.y = "Milestone") %>%  
    rename(deliverable_content = "Deliverable") %>%  
    mutate(deliverable_start = milestone_start,
           deliverable_end = NA)
  
  tmp_data %>%  
    select(contains("phase_")) %>%  
    rename_all(.funs = list(~gsub(".*_", "\\1", .))) %>% 
    mutate(group = "phases", 
           type = "range") %>% 
    bind_rows(
      tmp_data %>%  
        select(contains("milestone")) %>%
        rename_all(.funs = list(~gsub(".*_", "\\1", .))) %>% 
        mutate(group = "milestones", 
               type = "point")) %>% 
    bind_rows(
      tmp_data %>%  
        select(contains("deliverable")) %>%
        rename_all(.funs = list(~gsub(".*_", "\\1", .))) %>% 
        mutate(group = "deliverable", 
               type = "point")) %>% 
    unique() %>% 
    mutate(id = rownames(.) %>% 
             as.numeric()) %>%  
    select(id, everything()) %>%  
    filter(!is.na(start)) %>% str()
    timevis::timevis(data = ., groups = timevisDataGroups,
            options = list(editable = TRUE, 
              editable = TRUE,
              align = "center",
              orientation = "top",
              snap = NULL,
              margin = list(item = 30, axis = 50)))
  
}
