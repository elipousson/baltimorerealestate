# Created 2021-05-10 ----

# Network analysis attempts

library(tidygraph)
library(ggraph)

listing_agent_count <- mls_analysis %>%
  filter(neighborhood_name %in% c("Ellwood Park", "Baltimore Highlands")) %>%
  count(list_agent_name, sort = TRUE, name = "num_listings") %>%
  rename(agent = list_agent_name)

selling_agent_count <- mls_analysis %>%
  filter(neighborhood_name %in% c("Ellwood Park", "Baltimore Highlands")) %>%
  count(selling_agent, sort = TRUE, name = "num_sales") %>%
  rename(agent = selling_agent)

agent_count <- full_join(selling_agent_count, listing_agent_count) %>%
  filter(num_sales > 2)

list_agents <- mls_analysis %>%
  filter(settled_year >= 2018) %>%
  distinct(list_agent_code, list_agent_name) %>%
  rename(code = list_agent_code, label = list_agent_name)

selling_agents <- mls_analysis %>%
  filter(settled_year >= 2018) %>%
  distinct(selling_agent_code, selling_agent) %>%
  rename(code = selling_agent_code, label = selling_agent)

agents <- full_join(list_agents, selling_agents) %>%
  rowid_to_column("id")

num_sold <- mls_analysis %>%
  filter(settled_year >= 2018) %>%
  group_by(list_agent_code, selling_agent_code) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  filter(weight > 1) %>%
  left_join(agents, by = c("list_agent_code" = "code")) %>%
  rename(listing_agent = id) %>%
  left_join(agents, by = c("selling_agent_code" = "code")) %>%
  rename(selling_agent = id) %>%
  select(listing_agent, selling_agent, weight)

agents <- filter(agents,
                 (code %in% num_sold$listing_agent) |  (code %in% num_sold$selling_agent))

routes_tidy <- tbl_graph(nodes = agents, edges = num_sold, directed = TRUE)

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Sales") +
  theme_graph()
