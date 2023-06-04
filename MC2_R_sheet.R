pacman::p_load(jsonlite, tidygraph, ggraph, visNetwork, lubridate, tidyverse, fontawesome, plotly, igraph)

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

mc2_edges_aggregated_all <- mc2_edges_parent_hscode %>%
  group_by(source, target, Year, parent_hscode, HS_Name) %>% 
  summarise(weights = n(), Totalkg = sum(weightkg)) %>% 
  filter(source != target) %>%
  filter(weights > 20) %>% 
  ungroup() 

mc2_edges_aggregated <- mc2_edges_aggregated_all %>% filter(Year == "2034")


#############################################

id1 <- mc2_edges_aggregated %>%
  select(source) %>%
  rename(id = source)
id2 <- mc2_edges_aggregated %>%
  select(target) %>%
  rename(id = target)
mc2_nodes_extracted <- rbind(id1, id2) %>%
  distinct()

#############################################

plot_ly(
  data = mc2_edges_aggregated_all,
  x = ~Year,
  y = ~Totalkg,
  color = ~HS_Name,
  type = "bar"
) %>% 
  layout( title="Yearly total Weight of Seafood by category (Parent HS code)",barmode = "stack")

#############################################
top_X_sources <- mc2_edges_aggregated %>%
  group_by(source) %>%
  summarise(Total_Totalkg = sum(Totalkg)) %>%
  top_n(10, Total_Totalkg) %>%
  inner_join(mc2_edges_aggregated, by = "source") 

plot_ly(
  data = top_x_sources,
  x = ~Totalkg,
  y = ~reorder(source, Total_Totalkg),
  color = ~HS_Name,
  type = "bar"
) %>% 
  layout(
    title = "Top 10 Exporters by weight (2034)",
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



#############################################


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



# Calculate degree centrality
degree_centrality <- degree(mc2_graph)

# Calculate betweenness centrality
betweenness_centrality <- betweenness(mc2_graph)

# Calculate closeness centrality
closeness_centrality <- closeness(mc2_graph)

# Calculate eigenvector centrality
eigenvector_centrality <- eigen_centrality(mc2_graph)$vector

# Add centrality measures to the nodes data frame
nodes_df$degree_centrality <- degree_centrality
nodes_df$betweenness_centrality <- betweenness_centrality

nodes_df$closeness_centrality <- closeness_centrality
nodes_df$closeness_centrality <- round(nodes_df$closeness_centrality, digits = 1)

nodes_df$eigenvector_centrality <- eigenvector_centrality

# Set threshold values for centrality measures
degree_threshold <- 5
betweenness_threshold <- 0.1
closeness_threshold <- 0.3
eigenvector_threshold <- 0.2

# Filter nodes based on centrality thresholds
filtered_nodes <- nodes_df[degree_centrality >= degree_threshold , ]

# Create the network visualization with filtered nodes
visNetwork(filtered_nodes, edges_df, width = "100%") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visEdges(arrows = "from", smooth = list(enabled = TRUE, type = "curvedCW")) %>%
  visNodes() %>%
  visLegend() %>%
  visOptions(highlightNearest = TRUE)




