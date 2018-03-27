library(readr)
library(tidygraph)
library(dplyr)
library(ggplot2)
library(ggraph)

tib <- read_csv("./data/data.csv")

g <- as_tbl_graph(tib)


ggraph(g) + geom_node_point(aes(color=name),size=10) + 
  geom_edge_link(aes(label = value), alpha = 0.5,
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm'),
    arrow = arrow(length = unit(4,'mm'), type='closed'),
                 end_cap = circle(4,'mm'),
                 start_cap = circle(4,'mm')) +
  geom_node_text(aes(label = name)) + ggthemes::theme_few()
