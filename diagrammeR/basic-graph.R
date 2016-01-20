#------------------------------------------------
# https://github.com/rich-iannone/DiagrammeR
#------------------------------------------------

library(DiagrammeR)

# So, with Graphviz:
grViz("
      digraph DAG {
      
      # Intialization of graph attributes
      graph [overlap = true]
      
      # Initialization of node attributes
      node [shape = box,
      fontname = Helvetica,
      color = blue,
      type = box,
      fixedsize = true]
      
      # Initialization of edge attributes
      edge [color = green,
      rel = yields]
      
      # Node statements
      1; 2; 3; 4; 8; 9; 10; 11
      
      # Revision to node attributes
      node [shape = circle]
      
      # Node statements
      5; 6; 7
      
      # Edge statements
      1->5; 2->6; 3->9; 4->7; 5->8; 5->10; 7->11
      
      # Revision to edge attributes
      edge [color = red]
      
      # Edge statements
      1->8; 3->6; 3->11; 3->7; 5->9; 6->10
      }
      ")



# With magrittr and DiagrammeR's graph functions:
library(DiagrammeR)
library(magrittr)

graph <-
  create_graph() %>%
  set_graph_name("DAG") %>%
  set_global_graph_attr("graph", "overlap", "true") %>%
  set_global_graph_attr("graph", "fixedsize", "true") %>%
  set_global_graph_attr("node", "color", "blue") %>%
  set_global_graph_attr("node", "fontname", "Helvetica") %>%
  add_n_nodes(11) %>%
  select_nodes_by_id(1:4) %>% 
  set_node_attr_with_selection("shape", "box") %>%
  set_node_attr_with_selection("type", "box") %>%
  clear_selection %>%
  select_nodes_by_id(5:7) %>% 
  set_node_attr_with_selection("shape", "circle") %>%
  set_node_attr_with_selection("type", "circle") %>%
  clear_selection %>%
  select_nodes_by_id(8:11) %>% 
  set_node_attr_with_selection("shape", "box") %>%
  set_node_attr_with_selection("type", "box") %>%
  clear_selection %>%
  add_edge(1, 5) %>% add_edge(2, 6) %>%
  add_edge(3, 9) %>% add_edge(4, 7) %>%
  add_edge(5, 8) %>% add_edge(5, 10) %>%
  add_edge(7, 11) %>% 
  select_edges %>%
  set_edge_attr_with_selection("color", "green") %>%
  add_edge(1, 8) %>% add_edge(3, 6) %>%
  add_edge(3, 11) %>% add_edge(3, 7) %>%
  add_edge(5, 9) %>% add_edge(6, 10) %>%
  select_edges("color", "^$") %>%
  set_edge_attr_with_selection("color", "red") %>%
  clear_selection

render_graph(graph)