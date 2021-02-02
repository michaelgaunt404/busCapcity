setwd("~/")
rstudioapi::getSourceEditorContext()$path %>%
  as.character() %>%
  gsub("/R.*","\\1", .) %>%
  path.expand() %>%
  setwd()

task_list = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "tasks", skip = 2) %>%
  remove_empty(c("rows", "cols"))

task_list_lookup = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "tasks") %>%
  head(2) %>% 
  remove_empty(c("rows", "cols")) 
  

task_list_lookup = data.frame(Percent = task_list_lookup[1, -c(1,2)] %>%  as.numeric(),
                              Phase = task_list_lookup[2, -c(1,2)] %>%  as.character())

task_list %>%  
  pivot_longer(cols = -Work_Type, names_to = "Phase" ) %>%  
  na.omit() %>% 
  merge(., task_list_lookup, by = "Phase") %>% 
  arrange(Work_Type, Percent) %>%  
  data.table()

data <- data.frame(
  id = 1:3,
  start = c("2015-04-04", "2015-04-05 11:00:00", "2015-04-06 15:00:00"),
  end = c("2015-04-08", NA, NA),
  content = c("<h2>Vacation!!!</h2>", "Acupuncture", "Massage"),
  style = c("background-color: red;", NA, NA), 
  type = c("background", "point", "point"),
  group = c(NA, "Tasks",  "Tasks"),
  subgroup = c("yolo", "Butt", "Butt2")
) %>%  
  mutate(title = start)

data_groups = data$group %>%  
  unique() %>%  
  data.frame(content = ., 
             id = .)

data_sub_groups = data$subgroup %>%  
  unique() %>%  
  data.frame(content = ., 
             id = .)


  timevis(data, groups = data_groups, 
    
    options = list(subgroupVisibility = "{'yolo': false, 'Butt': true, 'Butt2': true}",
                   showNested = T, snap = NULL, editable = TRUE, multiselect = TRUE, 
                   align = "center", 
                   maxHeight = 800)
  )




  timedata <- data.frame(
    id = 1:6, 
    start = Sys.Date() + c(1, - 10, 4, 20, -10, 10),
    end = c(rep(as.Date(NA), 4), Sys.Date(), Sys.Date() + 20),
    group = c(1,1,1,2,2,2),
    content = c("event 1", "event 2", "event 2", "event 1", "range 1",     "range 1"),
    subgroup = c("1.1", "1.2", "1.2", "2.1", "2.2", "2.2")
  )
  
  groups <- data.frame(id = c(1,2), content = c("g1", "g2"), title = c("YOLO", "HSHSH"))
  timevis::timevis(data =timedata, groups = groups, options = list(stack = FALSE))









#sankey============
  library(tidyverse)
  library(viridis)
  library(patchwork)
  library(hrbrthemes)
  library(circlize)
  library(textclean)
  library(janitor)
  library(dtpl)
  library(tidygraph)
  library(ggraph)
  library(networkD3)
  library(igraph)
  
  # Load dataset from github
  data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
  # Package

  
  # I need a long format
  data_long <- data %>%
    rownames_to_column %>%
    gather(key = 'key', value = 'value', -rowname) %>%
    filter(value > 0)
  colnames(data_long) <- c("source", "target", "value")
  data_long$target <- paste(data_long$target, " ", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  data_long$IDsource=match(data_long$source, nodes$name)-1 
  data_long$IDtarget=match(data_long$target, nodes$name)-1
  
  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  # Make the Network
  sankeyNetwork(Links = data_long, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

  
  
  
  u
  
  
  
  
  #deliverables dataframe that has been filtered
  del_raw = readxl::read_excel('./data/DA_PDA_DDP_Checklist.xlsx', 
                                  sheet = "DM_CKLST_X") %>% 
    remove_empty(c("rows", "cols")) %>%  
    na_if("-") %>%  
    clean_names() %>%  
    filter(is.na(remove))
  
  del_raw_pivot = del_raw %>% 
    pivot_longer(cols = c(bod:ddp),
                 names_to = "milestone",
                 values_to = "keep") %>%  
    filter(!is.na(keep))
  
  chklst_raw = readxl::read_excel('./data/DA_PDA_DDP_Checklist.xlsx', 
                               sheet = "Task_X") %>% 
    remove_empty(c("rows", "cols")) %>%  
    na_if("-") %>%  
    clean_names() %>%  
    filter(is.na(remove)) %>% 
    pivot_longer(cols = planning_review:final_design_90, 
                 names_to = "assoc_phase", 
                 values_to = "element_name") %>% 
    filter(!is.na(element_name))
  
  del_work_df = del_raw_pivot %>%
    select(deliverable_id, milestone) %>% 
    set_names(c('deliverable_id', 'element_id')) %>%  
    bind_rows(., chklst_raw %>%  
                select(deliverable_id, element_id) %>%  
                na.omit()) %>% 
    na.omit() 
  
  del_work_df %>% 
    filter(element_id != "ddp") %>% 
    simpleNetwork(zoom = T)
  
  
  del_work_df %>%  arrange(deliverable_id) %>%  data.frame()
  
  del_work_df_nodes = data.frame(items = c(del_work_df$deliverable_id, del_work_df$element_id) %>%  
                             unique()) %>%  
    mutate(number = row.names(.) %>%  
             as.numeric()-1)
  
  del_work_df %>%  
    mutate(value = 1) %>% 
    sankeyNetwork(Links = ., Nodes = del_work_df_nodes,
                  Source = "deliverable_id", Target = "element_id",
                  Value = "value", NodeID = "items")

  del_work_df_tbl_grph = del_work_df %>%  
    mutate(value = 1) %>% 
    merge(numbered_df, by.x = "deliverable_id", by.y = "items") %>% 
    merge(numbered_df, by.x = "element_id", by.y = "items") %>%  
    select(number.x, number.y, everything()) %>%
    # set_names(c("from", "to", "weight")) %>% 
    as_tbl_graph() 
  
  del_work_df_tbl_grph %>% 
    # mutate(community = as.factor(group_infomap())) %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() +
    geom_node_point(aes(colour = community), size = 7) +
  
  tbl_graph(nodes = del_work_df_nodes, edges = del_work_df) %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() +
    geom_node_point(size = 7) +
    theme_graph()
  
  iris_clust <- hclust(dist(iris[1:4]))
  iris_tree <- as_tbl_graph(iris_clust)
  iris_tree %>% 
    ggraph(layout = 'dendrogram') + 
    geom_edge_bend()
  
  iris_tree %>% 
    ggraph(layout = 'linear', circular = T) + 
    geom_edge_arc() 
    
  
  rstat_nodes <- data.frame(name = c("Hadley", "David", "Romain", "Julia"), 
                            gender = c("b", "b", "b", "g"))
  rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
                            to = c(2, 3, 4, 1, 1, 2, 1, 2, 3)) %>%  
    tail()
  play_islands(5, 10, 0.8, 3)
  tbl_graph(nodes = rstat_nodes, edges = rstat_edges) %>% 
    ggraph(layout = 'linear', circular = F) + 
    geom_edge_arc(arrow = arrow()) +
    geom_node_point(aes(color = gender), size = 7)
  
  tbl_graph(nodes = rstat_nodes, edges = rstat_edges) %>% 
    # mutate(leaf = node_is_leaf(), root = node_is_root()) %>% 
    ggraph(layout = 'tree') + 
    geom_edge_diagonal(arrow = arrow()) +
    geom_node_point(aes(color = gender), size = 7) +
    # geom_node_point(aes(filter = leaf), colour = 'forestgreen', size = 10) +
    # geom_node_point(aes(filter = root), colour = 'firebrick', size = 10) +
    theme_graph()

  create_tree(20, 3) %>%  mutate(leaf = node_is_leaf(), root = node_is_root()) 
  del_work_df %>% 
    simpleNetwork(aes(), zoom = T)
  
  chklst_raw %>%  
    select(deliverable_id, element_id,assoc_phase, element_name) %>%  
    filter(!is.na(element_name)) %>% head(20)
    na.omit() %>% 
    simpleNetwork(zoom = T)
  
    create_tree(20, 3) %>% 
      mutate(leaf = node_is_leaf(), root = node_is_root())
  
  
  
  numbered_df = data.frame(items = c(del_raw_pivot$deliverable_id, del_raw_pivot$milestone) %>%  
               unique()) %>%  
    mutate(number = row.names(.) %>%  
             as.numeric()-1)
    
  chklst_raw_sank = chklst_raw_pivot %>%  
    merge(numbered_df, by.x = "item", by.y = "items") %>% 
    merge(numbered_df, by.x = "milestone", by.y = "items") %>%  
    mutate(value = runif(min = .5, max = 2.5, nrow(.)))

  chklst_raw_sank %>%  
    filter(!is.na(include)) %>%
    select(milestone, item, number.x, number.y, value) %>%  
    set_names(c('target', 'source', 'IDsource', 'IDtarget', 'value')) %>% 
    sankeyNetwork(Links = ., Nodes = numbered_df,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "items")
                sinksRight=FALSE, colourScale=NA, nodeWidth=40, fontSize=13, nodePadding=20)
chklst_raw_sank %>%  str()
  library(tidygraph)
devtools::install_github("r-lib/ellipsis")
install.packages("pkgload")
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
5cd9514mr0 <- jsonlite::fromJSON(URL)

edges = chklst_raw_sank %>%  
  select(number.x, number.y, value) %>%
  set_names(c("from", "to", "weight")) 

# Create graph of highschool friendships
graph <- as_tbl_graph(highschool) %>% 
  mutate(Popularity = centrality_degree(mode = 'in'))

edges %>%  
  as_tbl_graph() %>%  
  activate(nodes) %>% 
  left_join(numbered_df %>%  
              mutate(number = as.character(number)), by = c("name" = "number")) %>% 
  ggraph(layout = 'linear', circular = F) + 
  geom_edge_arc() + 
  geom_node_text(aes(label = items))
  geom_edge_link() + 
  geom_node_point()
  
  circular = TRUE
# plot using ggraph
yolo = ggraph(graph, layout = 'linear') + 
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(size = Popularity)) + 
  facet_edges(~year) 
  # theme_graph(foreground = 'steelblue', fg_text_colour = 'white')





phase = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "phases") %>%
  clean_names() %>% 
  remove_empty("rows") %>%  
  mutate(input_index = str_glue("{phase_name} ({phase_percent})"), 
         `notes_phase` = str_glue('<a href="#" onclick="alert(\'{`notes_phase`}\');">Click for Description</a>'),
         `notes_milestone` = str_glue('<a href="#" onclick="alert(\'{`notes_milestone`}\');">Click for Description</a>'), 
         button = str_glue('<button onclick="alert(\`{`notes_milestone`}\');" >Click for Description</button>')) %>%  
  data.table()


onclick_quick = function(column){
  str_glue('<a href="#" onclick="alert(\'{`column`}\');">Click for Description</a>')
}


phases$milestone %>%  
  onclick_quick()

phases %>%  
  mutate(notes_phase = onclick_quick(notes_phase)) %>%  
  DT::datatable(escape = F)


