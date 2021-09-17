test1 <- read.csv("C:/Users/User/Desktop/tools/R/data/eksternal/Berita/Berita Neraca Pengeluaran (Q1-Q2 2021).csv",
                  header = T, fill = T, sep = ";", encoding="UTF-8")
View(head(test1))
str(test1)

## network
library(network)
library(tidyverse)

sources <- test1 %>%
  distinct(sumber) %>%
  rename(label = sumber)


destinations <- test1 %>%
  distinct(kategori) %>%
  rename(label = kategori)

sentiments <- test1 %>%
  distinct(sentimen_kutipan) %>%
  rename(label = sentimen_kutipan)

nodes <- full_join(sentiments, destinations, by = "label")
nodes

'nodes <- left_join(sources, destinations, by = "label") %>% 
                     left_join(., sentiments, by = "label")'
'nodes'

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route <- test1 %>%  
  group_by(sumber, kategori) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("sumber" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("kategori" = "label")) %>% 
  rename(to = id)

edges <- edges %>% 
  left_join(nodes, by = c("sentimen_kutipan" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

routes_network <- network(edges, vertex.attr = nodes,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)
class(routes_network)

routes_network

plot(routes_network, vertex.cex = 3)

plot(routes_network, vertex.cex = 3, mode = "circle")

detach(package:network)
rm(routes_network)
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
routes_igraph

plot(routes_igraph, edge.arrow.size = 0.2)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))
routes_tidy

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

library(visNetwork)
library(networkD3)
visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/5 + 1)
visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

#Sankey network another version
library(ggplot2)
library(ggalluvial)

cb1 <- test1 %>%  
  group_by(sumber, kategori, sentimen_berita) %>%
  summarise(bobot = n()) %>% 
  ungroup()
head(cb1)

is_alluvia_form(as.data.frame(cb1), axes = 1:3, silent = TRUE)

ggplot(as.data.frame(cb1),
       aes(y = bobot, axis1 = sumber, axis2 = kategori)) +
  geom_alluvium(aes(fill = sentimen_berita), width = 1/12) +
  geom_stratum(width = 1/9, fill = "orange", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Sumber", "Kategori"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "colour") +
  theme_minimal() +
  ggtitle("Berita Neraca Pengeluaran (Q1-Q2 2021)")

