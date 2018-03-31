library(readr)
library(tidygraph)
library(dplyr)
library(ggplot2)
library(ggraph)

# we want a nicely labeled DAG, given from, to, and a value label for the edge
# Strategy: create a synthetic node that is the "value" of the edge, and use
# whatever layout to place all nodes (actual and synthetic). And then spend
# 90% of the time messing around with aesthetics

# read in the data
tib <- read_csv("./data/data.csv")

# create an edge from the "from" to a new synthetic node
# that is the created from the "value". These edges should
# not have arrows
fromToValue <- tib %>%
  mutate(from_value = paste0(from, "->", value, "->", to)) %>%
  select(from, from_value) %>%
  rename(to = from_value) %>%
  mutate(showArrow = FALSE)

# create an edge from the synthetic node to the
# "to". These edges should have arrows
valueToTo <- tib %>%
  mutate(from_value = paste0(from, "->", value, "->", to)) %>%
  select(from_value, to) %>%
  rename(from = from_value) %>%
  mutate(showArrow = TRUE)

# this tibble represents our edges
tib2 <- bind_rows(fromToValue, valueToTo)

# the valueNodes have a name that is the same as the
# synthetic nodes above and a label that is the 'value'
# from the original data
valueNodes <- tib %>%
  mutate(name = paste0(from, "->", value, "->", to),
         label = as.character(value)) %>%
  select(name, label)

# the data nodes are the nodes in the original data
dataNodes <- tibble(name = unique(c(tib$from, tib$to)),
                    label = unique(c(tib$from, tib$to)))

# all nodes are the concatenation of data nodes and the
# synthetic value nodes
theNodes <- bind_rows(dataNodes, valueNodes) %>% distinct()

# create the graph from the edge info
g <- as_tbl_graph(tib2)

# annotate the nodes in the graph so that we have the "label" attribute
g <- g %>% activate(nodes) %>% 
  inner_join(theNodes, by = c("name" = "name"))

# plot it. pay attention to the "filter" attribute of the aesthetic function ('aes')
ggraph(g, layout = "sugiyama") +
  geom_node_point(aes(color = name, filter = nchar(name) == 1), 
                  size = 10) +
  geom_edge_link(
    aes(filter = showArrow),
    alpha = 0.5,
    edge_width = 1,
    arrow = arrow(length = unit(4, 'mm'), type = 'closed'),
    end_cap = circle(4, 'mm'),
    start_cap = circle(4, 'mm')
  ) +
  geom_edge_link(
    aes(filter = !showArrow),
    alpha = 0.5,
    edge_width = 1,
    end_cap = circle(4, 'mm'),
    start_cap = circle(4, 'mm')
  ) +
  geom_node_text(aes(label = label)) + ggthemes::theme_few() +
  guides(color = FALSE) + theme(
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

ggsave("./proto.png",
       width = 5,
       height = 5,
       dpi = 100)
