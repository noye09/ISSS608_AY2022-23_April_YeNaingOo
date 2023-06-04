pacman::p_load(jsonlite, tidygraph, ggraph, visNetwork, lubridate, tidyverse, fontawesome, plotly)

mc2_data <- fromJSON("data/MC2/mc2_challenge_graph.json")

mc2_nodes <- as_tibble(mc2_data$nodes) %>%
  select(id, shpcountry, rcvcountry)

mc2_edges <- as_tibble(mc2_data$links) %>%
  mutate(ArrivalDate = ymd(arrivaldate)) %>%
  mutate(Year = year(ArrivalDate)) %>%
  mutate(Mth = month(ArrivalDate)) %>%
  mutate(parent_hscode = substr(hscode, 1, 3)) %>%
  select(source, target, ArrivalDate, Year,Mth, hscode, parent_hscode, valueofgoods_omu, 
         volumeteu, weightkg, valueofgoodsusd) %>% 
  distinct()


#write.csv(mc2_edges, "data/MC2/mc2_edges.csv", row.names=FALSE)
#write.csv(mc2_nodes, "data/MC2/mc2_nodes.csv", row.names=FALSE)

#Convert the data type of the 'parent_hscode' column in the Parent HS code table to character. Perform an inner join with the HS code table based on the 'parent_hscode' column, filtering away any unrelated hscode.(https://connect2india.com/hs-codes-directory/hs-code-3)
HS_code_3_India <- read.csv("data/MC2/HS_code_3_India.csv",colClasses=c("parent_hscode" ="character"))

mc2_edges_parent_hscode <- inner_join (mc2_edges,HS_code_3_India,
                                    join_by(parent_hscode   == parent_hscode))

#############################################
# Filter the data for the year 2028
# Group the data by source, target, Year, parent_hscode, and HS_Name
# Calculate the count of weights and the sum of Totalkg within each group
# Filter out rows where source is equal to target
# Filter out rows where weights is greater than 20
# Remove the grouping

mc2_edges_aggregated <- mc2_edges_parent_hscode %>%
  #filter(Year == "2028") %>% 
  group_by(source, target, Year, parent_hscode, HS_Name) %>% 
  summarise(weights = n(), Totalkg = sum(weightkg)) %>% 
  filter(source != target) %>%
  filter(weights > 20) %>% 
  ungroup() 




id1 <- mc2_edges_aggregated %>%
  select(source) %>%
  rename(id = source)
id2 <- mc2_edges_aggregated %>%
  select(target) %>%
  rename(id = target)
mc2_nodes_extracted <- rbind(id1, id2) %>%
  distinct()


####test
mc2_edges_aggregated_bin <- mc2_edges_parent_hscode %>%
  group_by(source, target, Year, parent_hscode,HS_Name ) %>%
  summarise(weights = n(),Totalkg = sum(weightkg)) %>%
  filter(source!=target) %>%
  ungroup()


# Define the percentage breakpoints for binning
percentages <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
# Calculate the quantiles based on the defined percentages
breakpoints_Totalkg <- quantile(mc2_edges_aggregated_bin$Totalkg, percentages)

breakpoints_Totalkg <- round(breakpoints_Totalkg, digits = 2)

bin_labels <- c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")

# Bin the 'values' column using the cut() function
mc2_edges_aggregated_bin$Totalkg_bin <- cut(mc2_edges_aggregated_bin$Totalkg, breakpoints_Totalkg, labels = bin_labels)



#############################################

plot_ly(
  data = mc2_edges_aggregated,
  x = ~Year,
  y = ~Totalkg,
  color = ~HS_Name,
  type = "bar"
) %>% 
  layout( title="Yearly total Weight of Seafood by category (Parent HS code)",barmode = "stack")


top_20_sources <- mc2_edges_aggregated %>%
  group_by(source) %>%
  summarise(Total_Totalkg = sum(Totalkg)) %>%
  top_n(20, Total_Totalkg) %>%
  inner_join(mc2_edges_aggregated, by = "source") 

plot_ly(
  data = top_20_sources,
  x = ~Totalkg,
  y = ~reorder(source, Total_Totalkg),
  color = ~HS_Name,
  type = "bar"
) %>% 
  layout(
    title = "Top 20 Exporters by weight",
    barmode = "stack"
  )

#############################################

mc2_graph <- tbl_graph(nodes = mc2_nodes_id,
                       edges = mc2_edges_aggregated,
                       directed = TRUE)


ggraph(mc2_graph,
       layout = "fr") +
  geom_edge_link(aes()) +
  geom_node_point(aes(colour = rcvcountry,size = 3))+
  theme_graph()

  
  
#############################################
mc2_nodes_id <- left_join(mc2_nodes_extracted, mc2_nodes, by = c("id" = "id")) %>%
  mutate(node_id = 1:nrow(.)) %>%
  mutate(shpcountry = ifelse(is.na(shpcountry), "unknown_shipTo", shpcountry)) %>%
  mutate(rcvcountry  = ifelse(is.na(rcvcountry ), "unknown_shipFrom", rcvcountry))

mc2_edges_aggregated_id <- mc2_edges_aggregated %>%
  left_join(mc2_nodes_id, by = c("source" = "id")) %>%
  rename(from = node_id) %>%
  select(-shpcountry,-rcvcountry) %>%
  left_join(mc2_nodes_id, by = c("target" = "id")) %>%
  rename(to = node_id) %>%
  select(-shpcountry,-rcvcountry)  %>%
  select(from,to,parent_hscode,HS_Name,Year,weights,Totalkg,source,target)


mc2_graph <- tbl_graph(nodes = mc2_nodes_id,
                       edges = mc2_edges_aggregated_id,
                       directed = TRUE)

edges_df <- mc2_graph %>%
  activate(edges) %>%
  as.tibble()

nodes_df <- mc2_graph %>%
  activate(nodes) %>%
  as.tibble() %>%
  rename(label = id) %>%
  mutate(group = rcvcountry) %>%
  mutate(id=row_number())


visNetwork(nodes_df,edges_df, width = "100%",main = "A really simple example") %>%
  #visNodes(shape = "square") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>%
  visEdges(arrows = "from",smooth = list(enabled = TRUE, type = "curvedCW")) %>%
  visGroups(groupname = "Oceanus",shape = "icon", icon = list(code = "f005", size = 75)) %>% 
  visLegend() %>%
  visOptions(highlightNearest = TRUE) %>%
  visOptions(selectedBy = "rcvcountry") %>%
  visHierarchicalLayout() %>%
  addFontAwesome()


#############################################


