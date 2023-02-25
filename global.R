library(igraph)
library(dplyr)

generate_graph_tables <- function(graph) {
  # Calculate various network properties, adding them as attributes to each
  # node/vertex
  if(!(is.directed(graph))) {
    # Community detection algorithms that can only work on undirected graphs
    print("Fastgreedy communities...")
    V(graph)$fastgreedy_comm <- membership(fastgreedy.community(graph))
    print("Multilevel communities...")
    V(graph)$multilevel_comm <- membership(multilevel.community(graph, weights = E(graph)$weight))
  }

  print("Walktrap communities...")
  V(graph)$walktrap_comm <- membership(walktrap.community(graph))
  print("Edge betweenness communities...")
  V(graph)$edge_comm <- membership(edge.betweenness.community(graph))

  if(vcount(graph) < 80) {
    # optimal.community has exponential time complexity, and should not be run
    # on large graphs
    print("Optimal communities...")
    V(graph)$optimal_comm <- membership(optimal.community(graph))
  }

  if(is.connected(graph)) {
    # Spinglass will only work on highly connected graphs
    print("Spinglass commmunities")
    V(graph)$spinglass_comm <- membership(spinglass.community(graph))
  }

  print("Other node statistics...")
  V(graph)$degree <- degree(graph)
  V(graph)$closeness <- centralization.closeness(graph)$res
  V(graph)$betweenness <- centralization.betweenness(graph)$res
  V(graph)$eigen <- centralization.evcent(graph)$vector

  # Re-generate dataframes for both nodes and edges, now containing
  # calculated network attributes
  #node_list <- get.data.frame(graph, what = "vertices")
  
  node_list <- get.data.frame(graph, what = "vertices")
  edge_list <- get.data.frame(graph, what = "edges")

  if(is.directed(graph)) {
    return(list(nodes = node_list, edges = edge_list))
  } else {
  # If a graph is undirected, it is necessary to "double" the edges, such that
  # A -> B has the complement B -> A. This will keep the adjacency plot
  # fully populated
    edge_list <- rbind(edge_list, edge_list %>% rename(from = to, to = from))
    return(list(nodes = node_list, edges = edge_list))
  }
}

# Load csv's from file
load_export <- function(edge_filename, node_filename = NULL, directed = TRUE) {
  edgelist <- read.csv(edge_filename, stringsAsFactors = FALSE)
  if(!(is.null(node_filename))) {
    nodelist <- read.csv(node_filename, stringsAsFactors = FALSE)
    graph <- graph.data.frame(edgelist, directed = directed, vertices = nodelist)
  } else {
    graph <- graph.data.frame(edgelist, directed = directed)
  }
  tables <- generate_graph_tables(graph)
  return(tables)
}

if(file.exists("data/igneous.RData")) {
  load("data/igneous.RData")
} else {
  graph <- load_export("data/igneous/igneous_mineral_edges_2.csv", "data/igneous/igneous_mineral_nodes_2.csv", directed = TRUE)
  igneous_node_list <- graph$nodes
  igneous_edge_list <- graph$edges
  save(igneous_node_list, igneous_edge_list, file = "data/igneous.RData")
}


if(file.exists("data/mag_o_spinel.RData")) {
  load("data/mag_o_spinel.RData")
} else {
  graph <- load_export("data/magnetite_oxidized_minerals/loc_edges_mo_spinel.csv", "data/magnetite_oxidized_minerals/loc_nodes_mo_spinel.csv", directed = TRUE)
  mag_o_spinel_node_list <- graph$nodes
  mag_o_spinel_edge_list <- graph$edges
  save(mag_o_spinel_node_list, mag_o_spinel_edge_list, file = "data/mag_o_spinel.RData")
}

if(file.exists("data/em_all.RData")) {
  load("data/em_all.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/72_elements_edges_total.csv", "data/mineral_quantity_of_73_elm_ab/72_elements_nodes_total.csv", directed = TRUE)
  em_all_node_list <- graph$nodes
  em_all_edge_list <- graph$edges
  save(em_all_node_list,em_all_edge_list, file = "data/em_all.RData")
}



if(file.exists("data/em_H.RData")) {
  load("data/em_H.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/H_edges.csv", "data/mineral_quantity_of_73_elm/H_nodes.csv", directed = TRUE)
  em_H_node_list <- graph$nodes
  em_H_edge_list <- graph$edges
  save(em_H_edge_list, em_H_node_list, file = "data/em_H.RData")
}

if(file.exists("data/em_Li.RData")) {
  load("data/em_Li.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Li_edges.csv", "data/mineral_quantity_of_73_elm/Li_nodes.csv", directed = TRUE)
  em_Li_node_list <- graph$nodes
  em_Li_edge_list <- graph$edges
  save(em_Li_node_list, em_Li_edge_list, file = "data/em_Li.RData")
}


if(file.exists("data/em_Be.RData")) {
  load("data/em_Be.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Be_edges.csv", "data/mineral_quantity_of_73_elm/Be_nodes.csv", directed = TRUE)
  em_Be_node_list <- graph$nodes
  em_Be_edge_list <- graph$edges
  save(em_Be_node_list, em_Be_edge_list, file = "data/em_Be.RData")
}

if(file.exists("data/em_B.RData")) {
  load("data/em_B.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/B_edges.csv", "data/mineral_quantity_of_73_elm/B_nodes.csv", directed = TRUE)
  em_B_node_list <- graph$nodes
  em_B_edge_list <- graph$edges
  save(em_B_node_list, em_B_edge_list, file = "data/em_B.RData")
}

if(file.exists("data/em_C.RData")) {
  load("data/em_C.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/C_edges.csv", "data/mineral_quantity_of_73_elm/C_nodes.csv", directed = TRUE)
  em_C_node_list <- graph$nodes
  em_C_edge_list <- graph$edges
  save(em_C_node_list, em_C_edge_list, file = "data/em_C.RData")
}

if(file.exists("data/em_N.RData")) {
  load("data/em_N.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/N_edges.csv", "data/mineral_quantity_of_73_elm/N_nodes.csv", directed = TRUE)
  em_N_node_list <- graph$nodes
  em_N_edge_list <- graph$edges
  save(em_N_node_list, em_N_edge_list, file = "data/em_N.RData")
}

if(file.exists("data/em_O.RData")) {
  load("data/em_O.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/O_edges.csv", "data/mineral_quantity_of_73_elm/O_nodes.csv", directed = TRUE)
  em_O_node_list <- graph$nodes
  em_O_edge_list <- graph$edges
  save(em_O_node_list, em_O_edge_list, file = "data/em_O.RData")
}

if(file.exists("data/em_F.RData")) {
  load("data/em_F.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/F_edges.csv", "data/mineral_quantity_of_73_elm/F_nodes.csv", directed = TRUE)
  em_F_node_list <- graph$nodes
  em_F_edge_list <- graph$edges
  save(em_F_node_list, em_F_edge_list, file = "data/em_F.RData")
}

if(file.exists("data/em_Na.RData")) {
  load("data/em_Na.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Na_edges.csv", "data/mineral_quantity_of_73_elm/Na_nodes.csv", directed = TRUE)
  em_Na_node_list <- graph$nodes
  em_Na_edge_list <- graph$edges
  save(em_Na_node_list, em_Na_edge_list, file = "data/em_Na.RData")
}

if(file.exists("data/em_Mg.RData")) {
  load("data/em_Mg.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Mg_edges.csv", "data/mineral_quantity_of_73_elm/Mg_nodes.csv", directed = TRUE)
  em_Mg_node_list <- graph$nodes
  em_Mg_edge_list <- graph$edges
  save(em_Mg_node_list, em_Mg_edge_list, file = "data/em_Mg.RData")
}

if(file.exists("data/em_Al.RData")) {
  load("data/em_Al.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Al_edges.csv", "data/mineral_quantity_of_73_elm/Al_nodes.csv", directed = TRUE)
  em_Al_node_list <- graph$nodes
  em_Al_edge_list <- graph$edges
  save(em_Al_node_list, em_Al_edge_list, file = "data/em_Al.RData")
}

if(file.exists("data/em_Si.RData")) {
  load("data/em_Si.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Si_edges.csv", "data/mineral_quantity_of_73_elm/Si_nodes.csv", directed = TRUE)
  em_Si_node_list <- graph$nodes
  em_Si_edge_list <- graph$edges
  save(em_Si_node_list, em_Si_edge_list, file = "data/em_Si.RData")
}

if(file.exists("data/em_P.RData")) {
  load("data/em_P.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/P_edges.csv", "data/mineral_quantity_of_73_elm/P_nodes.csv", directed = TRUE)
  em_P_node_list <- graph$nodes
  em_P_edge_list <- graph$edges
  save(em_P_node_list, em_P_edge_list, file = "data/em_P.RData")
}

if(file.exists("data/em_S.RData")) {
  load("data/em_S.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/S_edges.csv", "data/mineral_quantity_of_73_elm/S_nodes.csv", directed = TRUE)
  em_S_node_list <- graph$nodes
  em_S_edge_list <- graph$edges
  save(em_S_node_list, em_S_edge_list, file = "data/em_S.RData")
}

if(file.exists("data/em_Cl.RData")) {
  load("data/em_Cl.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Cl_edges.csv", "data/mineral_quantity_of_73_elm/Cl_nodes.csv", directed = TRUE)
  em_Cl_node_list <- graph$nodes
  em_Cl_edge_list <- graph$edges
  save(em_Cl_node_list, em_Cl_edge_list, file = "data/em_Cl.RData")
}

if(file.exists("data/em_K.RData")) {
  load("data/em_K.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/K_edges.csv", "data/mineral_quantity_of_73_elm/K_nodes.csv", directed = TRUE)
  em_K_node_list <- graph$nodes
  em_K_edge_list <- graph$edges
  save(em_K_node_list, em_K_edge_list, file = "data/em_K.RData")
}

if(file.exists("data/em_Ca.RData")) {
  load("data/em_Ca.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ca_edges.csv", "data/mineral_quantity_of_73_elm/Ca_nodes.csv", directed = TRUE)
  em_Ca_node_list <- graph$nodes
  em_Ca_edge_list <- graph$edges
  save(em_Ca_node_list, em_Ca_edge_list, file = "data/em_Ca.RData")
}

if(file.exists("data/em_Sc.RData")) {
  load("data/em_Sc.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Sc_edges.csv", "data/mineral_quantity_of_73_elm/Sc_nodes.csv", directed = TRUE)
  em_Sc_node_list <- graph$nodes
  em_Sc_edge_list <- graph$edges
  save(em_Sc_node_list, em_Sc_edge_list, file = "data/em_Sc.RData")
}

if(file.exists("data/em_Ti.RData")) {
  load("data/em_Ti.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ti_edges.csv", "data/mineral_quantity_of_73_elm/Ti_nodes.csv", directed = TRUE)
  em_Ti_node_list <- graph$nodes
  em_Ti_edge_list <- graph$edges
  save(em_Ti_node_list, em_Ti_edge_list, file = "data/em_Ti.RData")
}

if(file.exists("data/em_V.RData")) {
  load("data/em_V.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/V_edges.csv", "data/mineral_quantity_of_73_elm/V_nodes.csv", directed = TRUE)
  em_V_node_list <- graph$nodes
  em_V_edge_list <- graph$edges
  save(em_V_node_list, em_V_edge_list, file = "data/em_V.RData")
}

if(file.exists("data/em_Cr.RData")) {
  load("data/em_Cr.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Cr_edges.csv", "data/mineral_quantity_of_73_elm/Cr_nodes.csv", directed = TRUE)
  em_Cr_node_list <- graph$nodes
  em_Cr_edge_list <- graph$edges
  save(em_Cr_node_list, em_Cr_edge_list, file = "data/em_Cr.RData")
}

if(file.exists("data/em_Mn.RData")) {
  load("data/em_Mn.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Mn_edges.csv", "data/mineral_quantity_of_73_elm/Mn_nodes.csv", directed = TRUE)
  em_Mn_node_list <- graph$nodes
  em_Mn_edge_list <- graph$edges
  save(em_Mn_node_list, em_Mn_edge_list, file = "data/em_Mn.RData")
}

if(file.exists("data/em_Fe.RData")) {
  load("data/em_Fe.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Fe_edges.csv", "data/mineral_quantity_of_73_elm/Fe_nodes.csv", directed = TRUE)
  em_Fe_node_list <- graph$nodes
  em_Fe_edge_list <- graph$edges
  save(em_Fe_node_list, em_Fe_edge_list, file = "data/em_Fe.RData")
}

if(file.exists("data/em_Co.RData")) {
  load("data/em_Co.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Co_edges.csv", "data/mineral_quantity_of_73_elm/Co_nodes.csv", directed = TRUE)
  em_Co_node_list <- graph$nodes
  em_Co_edge_list <- graph$edges
  save(em_Co_node_list, em_Co_edge_list, file = "data/em_Co.RData")
}


if(file.exists("data/em_Ni.RData")) {
  load("data/em_Ni.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ni_edges.csv", "data/mineral_quantity_of_73_elm/Ni_nodes.csv", directed = TRUE)
  em_Ni_node_list <- graph$nodes
  em_Ni_edge_list <- graph$edges
  save(em_Ni_node_list, em_Ni_edge_list, file = "data/em_Ni.RData")
}

if(file.exists("data/em_Cu.RData")) {
  load("data/em_Cu.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Cu_edges.csv", "data/mineral_quantity_of_73_elm/Cu_nodes.csv", directed = TRUE)
  em_Cu_node_list <- graph$nodes
  em_Cu_edge_list <- graph$edges
  save(em_Cu_node_list, em_Cu_edge_list, file = "data/em_Cu.RData")
}

if(file.exists("data/em_Zn.RData")) {
  load("data/em_Zn.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Zn_edges.csv", "data/mineral_quantity_of_73_elm/Zn_nodes.csv", directed = TRUE)
  em_Zn_node_list <- graph$nodes
  em_Zn_edge_list <- graph$edges
  save(em_Zn_node_list, em_Zn_edge_list, file = "data/em_Zn.RData")
}

if(file.exists("data/em_Ga.RData")) {
  load("data/em_Ga.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ga_edges.csv", "data/mineral_quantity_of_73_elm/Ga_nodes.csv", directed = TRUE)
  em_Ga_node_list <- graph$nodes
  em_Ga_edge_list <- graph$edges
  save(em_Ga_node_list, em_Ga_edge_list, file = "data/em_Ga.RData")
}

if(file.exists("data/em_Ge.RData")) {
  load("data/em_Ge.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ge_edges.csv", "data/mineral_quantity_of_73_elm/Ge_nodes.csv", directed = TRUE)
  em_Ge_node_list <- graph$nodes
  em_Ge_edge_list <- graph$edges
  save(em_Ge_node_list, em_Ge_edge_list, file = "data/em_Ge.RData")
}

if(file.exists("data/em_As.RData")) {
  load("data/em_As.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/As_edges.csv", "data/mineral_quantity_of_73_elm/As_nodes.csv", directed = TRUE)
  em_As_node_list <- graph$nodes
  em_As_edge_list <- graph$edges
  save(em_As_node_list, em_As_edge_list, file = "data/em_As.RData")
}

if(file.exists("data/em_Se.RData")) {
  load("data/em_Se.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Se_edges.csv", "data/mineral_quantity_of_73_elm/Se_nodes.csv", directed = TRUE)
  em_Se_node_list <- graph$nodes
  em_Se_edge_list <- graph$edges
  save(em_Se_node_list, em_Se_edge_list, file = "data/em_Se.RData")
}

if(file.exists("data/em_Br.RData")) {
  load("data/em_Br.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Br_edges.csv", "data/mineral_quantity_of_73_elm/Br_nodes.csv", directed = TRUE)
  em_Br_node_list <- graph$nodes
  em_Br_edge_list <- graph$edges
  save(em_Br_node_list, em_Br_edge_list, file = "data/em_Br.RData")
}

if(file.exists("data/em_Rb.RData")) {
  load("data/em_Rb.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Rb_edges.csv", "data/mineral_quantity_of_73_elm/Rb_nodes.csv", directed = TRUE)
  em_Rb_node_list <- graph$nodes
  em_Rb_edge_list <- graph$edges
  save(em_Rb_node_list, em_Rb_edge_list, file = "data/em_Rb.RData")
}

if(file.exists("data/em_Sr.RData")) {
  load("data/em_Sr.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Sr_edges.csv", "data/mineral_quantity_of_73_elm/Sr_nodes.csv", directed = TRUE)
  em_Sr_node_list <- graph$nodes
  em_Sr_edge_list <- graph$edges
  save(em_Sr_node_list, em_Sr_edge_list, file = "data/em_Sr.RData")
}

if(file.exists("data/em_Y.RData")) {
  load("data/em_Y.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Y_edges.csv", "data/mineral_quantity_of_73_elm/Y_nodes.csv", directed = TRUE)
  em_Y_node_list <- graph$nodes
  em_Y_edge_list <- graph$edges
  save(em_Y_node_list, em_Y_edge_list, file = "data/em_Y.RData")
}

if(file.exists("data/em_Zr.RData")) {
  load("data/em_Zr.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Zr_edges.csv", "data/mineral_quantity_of_73_elm/Zr_nodes.csv", directed = TRUE)
  em_Zr_node_list <- graph$nodes
  em_Zr_edge_list <- graph$edges
  save(em_Zr_node_list, em_Zr_edge_list, file = "data/em_Zr.RData")
}

if(file.exists("data/em_Nb.RData")) {
  load("data/em_Nb.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Nb_edges.csv", "data/mineral_quantity_of_73_elm/Nb_nodes.csv", directed = TRUE)
  em_Nb_node_list <- graph$nodes
  em_Nb_edge_list <- graph$edges
  save(em_Nb_node_list, em_Nb_edge_list, file = "data/em_Nb.RData")
}

if(file.exists("data/em_Mo.RData")) {
  load("data/em_Mo.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Mo_edges.csv", "data/mineral_quantity_of_73_elm/Mo_nodes.csv", directed = TRUE)
  em_Mo_node_list <- graph$nodes
  em_Mo_edge_list <- graph$edges
  save(em_Mo_node_list, em_Mo_edge_list, file = "data/em_Mo.RData")
}

if(file.exists("data/em_Ru.RData")) {
  load("data/em_Ru.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ru_edges.csv", "data/mineral_quantity_of_73_elm/Ru_nodes.csv", directed = TRUE)
  em_Ru_node_list <- graph$nodes
  em_Ru_edge_list <- graph$edges
  save(em_Ru_node_list, em_Ru_edge_list, file = "data/em_Ru.RData")
}

if(file.exists("data/em_Rh.RData")) {
  load("data/em_Rh.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Rh_edges.csv", "data/mineral_quantity_of_73_elm/Rh_nodes.csv", directed = TRUE)
  em_Rh_node_list <- graph$nodes
  em_Rh_edge_list <- graph$edges
  save(em_Rh_node_list, em_Rh_edge_list, file = "data/em_Rh.RData")
}

if(file.exists("data/em_Pd.RData")) {
  load("data/em_Pd.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Pd_edges.csv", "data/mineral_quantity_of_73_elm/Pd_nodes.csv", directed = TRUE)
  em_Pd_node_list <- graph$nodes
  em_Pd_edge_list <- graph$edges
  save(em_Pd_node_list, em_Pd_edge_list, file = "data/em_Pd.RData")
}

if(file.exists("data/em_Ag.RData")) {
  load("data/em_Ag.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ag_edges.csv", "data/mineral_quantity_of_73_elm/Ag_nodes.csv", directed = TRUE)
  em_Ag_node_list <- graph$nodes
  em_Ag_edge_list <- graph$edges
  save(em_Ag_node_list, em_Ag_edge_list, file = "data/em_Ag.RData")
}

if(file.exists("data/em_Cd.RData")) {
  load("data/em_Cd.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Cd_edges.csv", "data/mineral_quantity_of_73_elm/Cd_nodes.csv", directed = TRUE)
  em_Cd_node_list <- graph$nodes
  em_Cd_edge_list <- graph$edges
  save(em_Cd_node_list, em_Cd_edge_list, file = "data/em_Cd.RData")
}

if(file.exists("data/em_In.RData")) {
  load("data/em_In.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/In_edges.csv", "data/mineral_quantity_of_73_elm/In_nodes.csv", directed = TRUE)
  em_In_node_list <- graph$nodes
  em_In_edge_list <- graph$edges
  save(em_In_node_list, em_In_edge_list, file = "data/em_In.RData")
}

if(file.exists("data/em_Sn.RData")) {
  load("data/em_Sn.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Sn_edges.csv", "data/mineral_quantity_of_73_elm/Sn_nodes.csv", directed = TRUE)
  em_Sn_node_list <- graph$nodes
  em_Sn_edge_list <- graph$edges
  save(em_Sn_node_list, em_Sn_edge_list, file = "data/em_Sn.RData")
}

if(file.exists("data/em_Sb.RData")) {
  load("data/em_Sb.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Sb_edges.csv", "data/mineral_quantity_of_73_elm/Sb_nodes.csv", directed = TRUE)
  em_Sb_node_list <- graph$nodes
  em_Sb_edge_list <- graph$edges
  save(em_Sb_node_list, em_Sb_edge_list, file = "data/em_Sb.RData")
}

if(file.exists("data/em_Te.RData")) {
  load("data/em_Te.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Te_edges.csv", "data/mineral_quantity_of_73_elm/Te_nodes.csv", directed = TRUE)
  em_Te_node_list <- graph$nodes
  em_Te_edge_list <- graph$edges
  save(em_Te_node_list, em_Te_edge_list, file = "data/em_Te.RData")
}

if(file.exists("data/em_I.RData")) {
  load("data/em_I.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/I_edges.csv", "data/mineral_quantity_of_73_elm/I_nodes.csv", directed = TRUE)
  em_I_node_list <- graph$nodes
  em_I_edge_list <- graph$edges
  save(em_I_node_list, em_I_edge_list, file = "data/em_I.RData")
}

if(file.exists("data/em_Cs.RData")) {
  load("data/em_Cs.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Cs_edges.csv", "data/mineral_quantity_of_73_elm/Cs_nodes.csv", directed = TRUE)
  em_Cs_node_list <- graph$nodes
  em_Cs_edge_list <- graph$edges
  save(em_Cs_node_list, em_Cs_edge_list, file = "data/em_Cs.RData")
}

if(file.exists("data/em_Ba.RData")) {
  load("data/em_Ba.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ba_edges.csv", "data/mineral_quantity_of_73_elm/Ba_nodes.csv", directed = TRUE)
  em_Ba_node_list <- graph$nodes
  em_Ba_edge_list <- graph$edges
  save(em_Ba_node_list, em_Ba_edge_list, file = "data/em_Ba.RData")
}

if(file.exists("data/em_La.RData")) {
  load("data/em_La.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/La_edges.csv", "data/mineral_quantity_of_73_elm/La_nodes.csv", directed = TRUE)
  em_La_node_list <- graph$nodes
  em_La_edge_list <- graph$edges
  save(em_La_node_list, em_La_edge_list, file = "data/em_La.RData")
}

if(file.exists("data/em_Ce.RData")) {
  load("data/em_Ce.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ce_edges.csv", "data/mineral_quantity_of_73_elm/Ce_nodes.csv", directed = TRUE)
  em_Ce_node_list <- graph$nodes
  em_Ce_edge_list <- graph$edges
  save(em_Ce_node_list, em_Ce_edge_list, file = "data/em_Ce.RData")
}

if(file.exists("data/em_Pr.RData")) {
  load("data/em_Pr.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Pr_edges.csv", "data/mineral_quantity_of_73_elm/Pr_nodes.csv", directed = TRUE)
  em_Pr_node_list <- graph$nodes
  em_Pr_edge_list <- graph$edges
  save(em_Pr_node_list, em_Pr_edge_list, file = "data/em_Pr.RData")
}

if(file.exists("data/em_Nd.RData")) {
  load("data/em_Nd.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Nd_edges.csv", "data/mineral_quantity_of_73_elm/Nd_nodes.csv", directed = TRUE)
  em_Nd_node_list <- graph$nodes
  em_Nd_edge_list <- graph$edges
  save(em_Nd_node_list, em_Nd_edge_list, file = "data/em_Nd.RData")
}

if(file.exists("data/em_Sm.RData")) {
  load("data/em_Sm.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Sm_edges.csv", "data/mineral_quantity_of_73_elm/Sm_nodes.csv", directed = TRUE)
  em_Sm_node_list <- graph$nodes
  em_Sm_edge_list <- graph$edges
  save(em_Sm_node_list, em_Sm_edge_list, file = "data/em_Sm.RData")
}

if(file.exists("data/em_Gd.RData")) {
  load("data/em_Gd.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Gd_edges.csv", "data/mineral_quantity_of_73_elm/Gd_nodes.csv", directed = TRUE)
  em_Gd_node_list <- graph$nodes
  em_Gd_edge_list <- graph$edges
  save(em_Gd_node_list, em_Gd_edge_list, file = "data/em_Gd.RData")
}

if(file.exists("data/em_Dy.RData")) {
  load("data/em_Dy.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Dy_edges.csv", "data/mineral_quantity_of_73_elm/Dy_nodes.csv", directed = TRUE)
  em_Dy_node_list <- graph$nodes
  em_Dy_edge_list <- graph$edges
  save(em_Dy_node_list, em_Dy_edge_list, file = "data/em_Dy.RData")
}

if(file.exists("data/em_Er.RData")) {
  load("data/em_Er.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Er_edges.csv", "data/mineral_quantity_of_73_elm/Er_nodes.csv", directed = TRUE)
  em_Er_node_list <- graph$nodes
  em_Er_edge_list <- graph$edges
  save(em_Er_node_list, em_Er_edge_list, file = "data/em_Er.RData")
}

if(file.exists("data/em_Yb.RData")) {
  load("data/em_Yb.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Yb_edges.csv", "data/mineral_quantity_of_73_elm/Yb_nodes.csv", directed = TRUE)
  em_Yb_node_list <- graph$nodes
  em_Yb_edge_list <- graph$edges
  save(em_Yb_node_list, em_Yb_edge_list, file = "data/em_Yb.RData")
}

if(file.exists("data/em_Hf.RData")) {
  load("data/em_Hf.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Hf_edges.csv", "data/mineral_quantity_of_73_elm/Hf_nodes.csv", directed = TRUE)
  em_Hf_node_list <- graph$nodes
  em_Hf_edge_list <- graph$edges
  save(em_Hf_node_list, em_Hf_edge_list, file = "data/em_Hf.RData")
}

if(file.exists("data/em_Ta.RData")) {
  load("data/em_Ta.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ta_edges.csv", "data/mineral_quantity_of_73_elm/Ta_nodes.csv", directed = TRUE)
  em_Ta_node_list <- graph$nodes
  em_Ta_edge_list <- graph$edges
  save(em_Ta_node_list, em_Ta_edge_list, file = "data/em_Ta.RData")
}

if(file.exists("data/em_W.RData")) {
  load("data/em_W.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/W_edges.csv", "data/mineral_quantity_of_73_elm/W_nodes.csv", directed = TRUE)
  em_W_node_list <- graph$nodes
  em_W_edge_list <- graph$edges
  save(em_W_node_list, em_W_edge_list, file = "data/em_W.RData")
}

if(file.exists("data/em_Re.RData")) {
  load("data/em_Re.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Re_edges.csv", "data/mineral_quantity_of_73_elm/Re_nodes.csv", directed = TRUE)
  em_Re_node_list <- graph$nodes
  em_Re_edge_list <- graph$edges
  save(em_Re_node_list, em_Re_edge_list, file = "data/em_Re.RData")
}

if(file.exists("data/em_Os.RData")) {
  load("data/em_Os.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Os_edges.csv", "data/mineral_quantity_of_73_elm/Os_nodes.csv", directed = TRUE)
  em_Os_node_list <- graph$nodes
  em_Os_edge_list <- graph$edges
  save(em_Os_node_list, em_Os_edge_list, file = "data/em_Os.RData")
}


if(file.exists("data/em_Ir.RData")) {
  load("data/em_Ir.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Ir_edges.csv", "data/mineral_quantity_of_73_elm/Ir_nodes.csv", directed = TRUE)
  em_Ir_node_list <- graph$nodes
  em_Ir_edge_list <- graph$edges
  save(em_Ir_node_list, em_Ir_edge_list, file = "data/em_Ir.RData")
}


if(file.exists("data/em_Pt.RData")) {
  load("data/em_Pt.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Pt_edges.csv", "data/mineral_quantity_of_73_elm/Pt_nodes.csv", directed = TRUE)
  em_Pt_node_list <- graph$nodes
  em_Pt_edge_list <- graph$edges
  save(em_Pt_node_list, em_Pt_edge_list, file = "data/em_Pt.RData")
}

if(file.exists("data/em_Au.RData")) {
  load("data/em_Au.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Au_edges.csv", "data/mineral_quantity_of_73_elm/Au_nodes.csv", directed = TRUE)
  em_Au_node_list <- graph$nodes
  em_Au_edge_list <- graph$edges
  save(em_Au_node_list, em_Au_edge_list, file = "data/em_Au.RData")
}


if(file.exists("data/em_Hg.RData")) {
  load("data/em_Hg.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Hg_edges.csv", "data/mineral_quantity_of_73_elm/Hg_nodes.csv", directed = TRUE)
  em_Hg_node_list <- graph$nodes
  em_Hg_edge_list <- graph$edges
  save(em_Hg_node_list, em_Hg_edge_list, file = "data/em_Hg.RData")
}


if(file.exists("data/em_Tl.RData")) {
  load("data/em_Tl.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Tl_edges.csv", "data/mineral_quantity_of_73_elm/Tl_nodes.csv", directed = TRUE)
  em_Tl_node_list <- graph$nodes
  em_Tl_edge_list <- graph$edges
  save(em_Tl_node_list, em_Tl_edge_list, file = "data/em_Tl.RData")
}

if(file.exists("data/em_Pb.RData")) {
  load("data/em_Pb.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Pb_edges.csv", "data/mineral_quantity_of_73_elm/Pb_nodes.csv", directed = TRUE)
  em_Pb_node_list <- graph$nodes
  em_Pb_edge_list <- graph$edges
  save(em_Pb_node_list, em_Pb_edge_list, file = "data/em_Pb.RData")
}

if(file.exists("data/em_Bi.RData")) {
  load("data/em_Bi.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Bi_edges.csv", "data/mineral_quantity_of_73_elm/Bi_nodes.csv", directed = TRUE)
  em_Bi_node_list <- graph$nodes
  em_Bi_edge_list <- graph$edges
  save(em_Bi_node_list, em_Bi_edge_list, file = "data/em_Bi.RData")
}

if(file.exists("data/em_Th.RData")) {
  load("data/em_Th.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/Th_edges.csv", "data/mineral_quantity_of_73_elm/Th_nodes.csv", directed = TRUE)
  em_Th_node_list <- graph$nodes
  em_Th_edge_list <- graph$edges
  save(em_Th_node_list, em_Th_edge_list, file = "data/em_Th.RData")
}

if(file.exists("data/em_U.RData")) {
  load("data/em_U.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm/U_edges.csv", "data/mineral_quantity_of_73_elm/U_nodes.csv", directed = TRUE)
  em_U_node_list <- graph$nodes
  em_U_edge_list <- graph$edges
  save(em_U_node_list, em_U_edge_list, file = "data/em_U.RData")
}
################################################################################################################################################################
if(file.exists("data/em_all_ab.RData")) {
  load("data/em_all_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/72_elements_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/72_elements_nodes_ab.csv", directed = TRUE)
  em_all_ab_node_list <- graph$nodes
  em_all_ab_edge_list <- graph$edges
  save(em_all_ab_node_list,em_all_ab_edge_list, file = "data/em_all_ab.RData")
}

if(file.exists("data/em_H_ab.RData")) {
  load("data/em_H_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/H_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/H_nodes_ab.csv", directed = TRUE)
  em_H_ab_node_list <- graph$nodes
  em_H_ab_edge_list <- graph$edges
  save(em_H_ab_edge_list, em_H_ab_node_list, file = "data/em_H_ab.RData")
}

if(file.exists("data/em_Li_ab.RData")) {
  load("data/em_Li_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Li_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Li_nodes_ab.csv", directed = TRUE)
  em_Li_ab_node_list <- graph$nodes
  em_Li_ab_edge_list <- graph$edges
  save(em_Li_ab_node_list, em_Li_ab_edge_list, file = "data/em_Li_ab.RData")
}


if(file.exists("data/em_Be_ab.RData")) {
  load("data/em_Be_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Be_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Be_nodes_ab.csv", directed = TRUE)
  em_Be_ab_node_list <- graph$nodes
  em_Be_ab_edge_list <- graph$edges
  save(em_Be_ab_node_list, em_Be_ab_edge_list, file = "data/em_Be_ab.RData")
}

if(file.exists("data/em_B_ab.RData")) {
  load("data/em_B_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/B_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/B_nodes_ab.csv", directed = TRUE)
  em_B_ab_node_list <- graph$nodes
  em_B_ab_edge_list <- graph$edges
  save(em_B_ab_node_list, em_B_ab_edge_list, file = "data/em_B_ab.RData")
}

if(file.exists("data/em_C_ab.RData")) {
  load("data/em_C_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/C_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/C_nodes_ab.csv", directed = TRUE)
  em_C_ab_node_list <- graph$nodes
  em_C_ab_edge_list <- graph$edges
  save(em_C_ab_node_list, em_C_ab_edge_list, file = "data/em_C_ab.RData")
}

if(file.exists("data/em_N_ab.RData")) {
  load("data/em_N_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/N_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/N_nodes_ab.csv", directed = TRUE)
  em_N_ab_node_list <- graph$nodes
  em_N_ab_edge_list <- graph$edges
  save(em_N_ab_node_list, em_N_ab_edge_list, file = "data/em_N_ab.RData")
}

if(file.exists("data/em_O_ab.RData")) {
  load("data/em_O_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/O_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/O_nodes_ab.csv", directed = TRUE)
  em_O_ab_node_list <- graph$nodes
  em_O_ab_edge_list <- graph$edges
  save(em_O_ab_node_list, em_O_ab_edge_list, file = "data/em_O_ab.RData")
}

if(file.exists("data/em_F_ab.RData")) {
  load("data/em_F_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/F_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/F_nodes_ab.csv", directed = TRUE)
  em_F_ab_node_list <- graph$nodes
  em_F_ab_edge_list <- graph$edges
  save(em_F_ab_node_list, em_F_ab_edge_list, file = "data/em_F_ab.RData")
}

if(file.exists("data/em_Na_ab.RData")) {
  load("data/em_Na_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Na_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Na_nodes_ab.csv", directed = TRUE)
  em_Na_ab_node_list <- graph$nodes
  em_Na_ab_edge_list <- graph$edges
  save(em_Na_ab_node_list, em_Na_ab_edge_list, file = "data/em_Na_ab.RData")
}

if(file.exists("data/em_Mg_ab.RData")) {
  load("data/em_Mg_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Mg_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Mg_nodes_ab.csv", directed = TRUE)
  em_Mg_ab_node_list <- graph$nodes
  em_Mg_ab_edge_list <- graph$edges
  save(em_Mg_ab_node_list, em_Mg_ab_edge_list, file = "data/em_Mg_ab.RData")
}

if(file.exists("data/em_Al_ab.RData")) {
  load("data/em_Al_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Al_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Al_nodes_ab.csv", directed = TRUE)
  em_Al_ab_node_list <- graph$nodes
  em_Al_ab_edge_list <- graph$edges
  save(em_Al_ab_node_list, em_Al_ab_edge_list, file = "data/em_Al_ab.RData")
}

if(file.exists("data/em_Si_ab.RData")) {
  load("data/em_Si_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Si_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Si_nodes_ab.csv", directed = TRUE)
  em_Si_ab_node_list <- graph$nodes
  em_Si_ab_edge_list <- graph$edges
  save(em_Si_ab_node_list, em_Si_ab_edge_list, file = "data/em_Si_ab.RData")
}

if(file.exists("data/em_P_ab.RData")) {
  load("data/em_P_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/P_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/P_nodes_ab.csv", directed = TRUE)
  em_P_ab_node_list <- graph$nodes
  em_P_ab_edge_list <- graph$edges
  save(em_P_ab_node_list, em_P_ab_edge_list, file = "data/em_P_ab.RData")
}

if(file.exists("data/em_S_ab.RData")) {
  load("data/em_S_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/S_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/S_nodes_ab.csv", directed = TRUE)
  em_S_ab_node_list <- graph$nodes
  em_S_ab_edge_list <- graph$edges
  save(em_S_ab_node_list, em_S_ab_edge_list, file = "data/em_S_ab.RData")
}

if(file.exists("data/em_Cl_ab.RData")) {
  load("data/em_Cl_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Cl_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Cl_nodes_ab.csv", directed = TRUE)
  em_Cl_ab_node_list <- graph$nodes
  em_Cl_ab_edge_list <- graph$edges
  save(em_Cl_ab_node_list, em_Cl_ab_edge_list, file = "data/em_Cl_ab.RData")
}

if(file.exists("data/em_K_ab.RData")) {
  load("data/em_K_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/K_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/K_nodes_ab.csv", directed = TRUE)
  em_K_ab_node_list <- graph$nodes
  em_K_ab_edge_list <- graph$edges
  save(em_K_ab_node_list, em_K_ab_edge_list, file = "data/em_K_ab.RData")
}

if(file.exists("data/em_Ca_ab.RData")) {
  load("data/em_Ca_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ca_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ca_nodes_ab.csv", directed = TRUE)
  em_Ca_ab_node_list <- graph$nodes
  em_Ca_ab_edge_list <- graph$edges
  save(em_Ca_ab_node_list, em_Ca_ab_edge_list, file = "data/em_Ca_ab.RData")
}

if(file.exists("data/em_Sc_ab.RData")) {
  load("data/em_Sc_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Sc_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Sc_nodes_ab.csv", directed = TRUE)
  em_Sc_ab_node_list <- graph$nodes
  em_Sc_ab_edge_list <- graph$edges
  save(em_Sc_ab_node_list, em_Sc_ab_edge_list, file = "data/em_Sc_ab.RData")
}

if(file.exists("data/em_Ti_ab.RData")) {
  load("data/em_Ti_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ti_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ti_nodes_ab.csv", directed = TRUE)
  em_Ti_ab_node_list <- graph$nodes
  em_Ti_ab_edge_list <- graph$edges
  save(em_Ti_ab_node_list, em_Ti_ab_edge_list, file = "data/em_Ti_ab.RData")
}

if(file.exists("data/em_V_ab.RData")) {
  load("data/em_V_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/V_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/V_nodes_ab.csv", directed = TRUE)
  em_V_ab_node_list <- graph$nodes
  em_V_ab_edge_list <- graph$edges
  save(em_V_ab_node_list, em_V_ab_edge_list, file = "data/em_V_ab.RData")
}

if(file.exists("data/em_Cr_ab.RData")) {
  load("data/em_Cr_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Cr_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Cr_nodes_ab.csv", directed = TRUE)
  em_Cr_ab_node_list <- graph$nodes
  em_Cr_ab_edge_list <- graph$edges
  save(em_Cr_ab_node_list, em_Cr_ab_edge_list, file = "data/em_Cr_ab.RData")
}

if(file.exists("data/em_Mn_ab.RData")) {
  load("data/em_Mn_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Mn_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Mn_nodes_ab.csv", directed = TRUE)
  em_Mn_node_list <- graph$nodes
  em_Mn_edge_list <- graph$edges
  save(em_Mn_node_list, em_Mn_edge_list, file = "data/em_Mn_ab.RData")
}

if(file.exists("data/em_Fe_ab.RData")) {
  load("data/em_Fe_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Fe_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Fe_nodes_ab.csv", directed = TRUE)
  em_Fe_ab_node_list <- graph$nodes
  em_Fe_ab_edge_list <- graph$edges
  save(em_Fe_ab_node_list, em_Fe_ab_edge_list, file = "data/em_Fe_ab.RData")
}

if(file.exists("data/em_Co_ab.RData")) {
  load("data/em_Co_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Co_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Co_nodes_ab.csv", directed = TRUE)
  em_Co_ab_node_list <- graph$nodes
  em_Co_ab_edge_list <- graph$edges
  save(em_Co_ab_node_list, em_Co_ab_edge_list, file = "data/em_Co_ab.RData")
}


if(file.exists("data/em_Ni_ab.RData")) {
  load("data/em_Ni_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ni_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ni_nodes_ab.csv", directed = TRUE)
  em_Ni_ab_node_list <- graph$nodes
  em_Ni_ab_edge_list <- graph$edges
  save(em_Ni_ab_node_list, em_Ni_ab_edge_list, file = "data/em_Ni_ab.RData")
}

if(file.exists("data/em_Cu_ab.RData")) {
  load("data/em_Cu_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Cu_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Cu_nodes_ab.csv", directed = TRUE)
  em_Cu_ab_node_list <- graph$nodes
  em_Cu_ab_edge_list <- graph$edges
  save(em_Cu_ab_node_list, em_Cu_ab_edge_list, file = "data/em_Cu_ab.RData")
}

if(file.exists("data/em_Zn_ab.RData")) {
  load("data/em_Zn_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Zn_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Zn_nodes_ab.csv", directed = TRUE)
  em_Zn_ab_node_list <- graph$nodes
  em_Zn_ab_edge_list <- graph$edges
  save(em_Zn_ab_node_list, em_Zn_ab_edge_list, file = "data/em_Zn_ab.RData")
}

if(file.exists("data/em_Ga_ab.RData")) {
  load("data/em_Ga_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ga_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ga_nodes_ab.csv", directed = TRUE)
  em_Ga_ab_node_list <- graph$nodes
  em_Ga_ab_edge_list <- graph$edges
  save(em_Ga_ab_node_list, em_Ga_ab_edge_list, file = "data/em_Ga_ab.RData")
}

if(file.exists("data/em_Ge_ab.RData")) {
  load("data/em_Ge_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ge_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ge_nodes_ab.csv", directed = TRUE)
  em_Ge_ab_node_list <- graph$nodes
  em_Ge_ab_edge_list <- graph$edges
  save(em_Ge_ab_node_list, em_Ge_ab_edge_list, file = "data/em_Ge_ab.RData")
}

if(file.exists("data/em_As_ab.RData")) {
  load("data/em_As_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/As_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/As_nodes_ab.csv", directed = TRUE)
  em_As_ab_node_list <- graph$nodes
  em_As_ab_edge_list <- graph$edges
  save(em_As_ab_node_list, em_As_ab_edge_list, file = "data/em_As_ab.RData")
}

if(file.exists("data/em_Se_ab.RData")) {
  load("data/em_Se_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Se_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Se_nodes_ab.csv", directed = TRUE)
  em_Se_ab_node_list <- graph$nodes
  em_Se_ab_edge_list <- graph$edges
  save(em_Se_ab_node_list, em_Se_ab_edge_list, file = "data/em_Se_ab.RData")
}

if(file.exists("data/em_Br_ab.RData")) {
  load("data/em_Br_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Br_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Br_nodes_ab.csv", directed = TRUE)
  em_Br_ab_node_list <- graph$nodes
  em_Br_ab_edge_list <- graph$edges
  save(em_Br_ab_node_list, em_Br_ab_edge_list, file = "data/em_Br_ab.RData")
}

if(file.exists("data/em_Rb_ab.RData")) {
  load("data/em_Rb_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Rb_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Rb_nodes_ab.csv", directed = TRUE)
  em_Rb_ab_node_list <- graph$nodes
  em_Rb_ab_edge_list <- graph$edges
  save(em_Rb_ab_node_list, em_Rb_ab_edge_list, file = "data/em_Rb_ab.RData")
}

if(file.exists("data/em_Sr_ab.RData")) {
  load("data/em_Sr_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Sr_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Sr_nodes_ab.csv", directed = TRUE)
  em_Sr_ab_node_list <- graph$nodes
  em_Sr_ab_edge_list <- graph$edges
  save(em_Sr_ab_node_list, em_Sr_ab_edge_list, file = "data/em_Sr_ab.RData")
}

if(file.exists("data/em_Y_ab.RData")) {
  load("data/em_Y_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Y_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Y_nodes_ab.csv", directed = TRUE)
  em_Y_ab_node_list <- graph$nodes
  em_Y_ab_edge_list <- graph$edges
  save(em_Y_ab_node_list, em_Y_ab_edge_list, file = "data/em_Y_ab.RData")
}

if(file.exists("data/em_Zr_ab.RData")) {
  load("data/em_Zr_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Zr_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Zr_nodes_ab.csv", directed = TRUE)
  em_Zr_ab_node_list <- graph$nodes
  em_Zr_ab_edge_list <- graph$edges
  save(em_Zr_ab_node_list, em_Zr_ab_edge_list, file = "data/em_Zr_ab.RData")
}

if(file.exists("data/em_Nb_ab.RData")) {
  load("data/em_Nb_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Nb_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Nb_nodes_ab.csv", directed = TRUE)
  em_Nb_ab_node_list <- graph$nodes
  em_Nb_ab_edge_list <- graph$edges
  save(em_Nb_ab_node_list, em_Nb_ab_edge_list, file = "data/em_Nb_ab.RData")
}

if(file.exists("data/em_Mo_ab.RData")) {
  load("data/em_Mo_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Mo_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Mo_nodes_ab.csv", directed = TRUE)
  em_Mo_ab_node_list <- graph$nodes
  em_Mo_ab_edge_list <- graph$edges
  save(em_Mo_ab_node_list, em_Mo_ab_edge_list, file = "data/em_Mo_ab.RData")
}

if(file.exists("data/em_Ru_ab.RData")) {
  load("data/em_Ru_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ru_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ru_nodes_ab.csv", directed = TRUE)
  em_Ru_ab_node_list <- graph$nodes
  em_Ru_ab_edge_list <- graph$edges
  save(em_Ru_ab_node_list, em_Ru_ab_edge_list, file = "data/em_Ru_ab.RData")
}

if(file.exists("data/em_Rh_ab.RData")) {
  load("data/em_Rh_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Rh_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Rh_nodes_ab.csv", directed = TRUE)
  em_Rh_ab_node_list <- graph$nodes
  em_Rh_ab_edge_list <- graph$edges
  save(em_Rh_ab_node_list, em_Rh_ab_edge_list, file = "data/em_Rh_ab.RData")
}

if(file.exists("data/em_ab_Pd.RData")) {
  load("data/em_ab_Pd.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Pd_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Pd_nodes_ab.csv", directed = TRUE)
  em_Pd_ab_node_list <- graph$nodes
  em_Pd_ab_edge_list <- graph$edges
  save(em_Pd_ab_node_list, em_Pd_ab_edge_list, file = "data/em_ab_Pd.RData")
}

if(file.exists("data/em_Ag_ab.RData")) {
  load("data/em_Ag_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ag_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ag_nodes_ab.csv", directed = TRUE)
  em_Ag_ab_node_list <- graph$nodes
  em_Ag_ab_edge_list <- graph$edges
  save(em_Ag_ab_node_list, em_Ag_ab_edge_list, file = "data/em_Ag_ab.RData")
}

if(file.exists("data/em_Cd_ab.RData")) {
  load("data/em_Cd_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Cd_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Cd_nodes_ab.csv", directed = TRUE)
  em_Cd_ab_node_list <- graph$nodes
  em_Cd_ab_edge_list <- graph$edges
  save(em_Cd_ab_node_list, em_Cd_ab_edge_list, file = "data/em_Cd_ab.RData")
}

if(file.exists("data/em_In_ab.RData")) {
  load("data/em_In_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/In_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/In_nodes_ab.csv", directed = TRUE)
  em_In_ab_node_list <- graph$nodes
  em_In_ab_edge_list <- graph$edges
  save(em_In_ab_node_list, em_In_ab_edge_list, file = "data/em_In_ab.RData")
}

if(file.exists("data/em_Sn_ab.RData")) {
  load("data/em_Sn_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Sn_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Sn_nodes_ab.csv", directed = TRUE)
  em_Sn_ab_node_list <- graph$nodes
  em_Sn_ab_edge_list <- graph$edges
  save(em_Sn_ab_node_list, em_Sn_ab_edge_list, file = "data/em_Sn_ab.RData")
}

if(file.exists("data/em_Sb_ab.RData")) {
  load("data/em_Sb_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Sb_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Sb_nodes_ab.csv", directed = TRUE)
  em_Sb_ab_node_list <- graph$nodes
  em_Sb_ab_edge_list <- graph$edges
  save(em_Sb_ab_node_list, em_Sb_ab_edge_list, file = "data/em_Sb_ab.RData")
}

if(file.exists("data/em_Te_ab.RData")) {
  load("data/em_Te_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Te_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Te_nodes_ab.csv", directed = TRUE)
  em_Te_ab_node_list <- graph$nodes
  em_Te_ab_edge_list <- graph$edges
  save(em_Te_ab_node_list, em_Te_ab_edge_list, file = "data/em_Te_ab.RData")
}

if(file.exists("data/em_I_ab.RData")) {
  load("data/em_I_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/I_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/I_nodes_ab.csv", directed = TRUE)
  em_I_ab_node_list <- graph$nodes
  em_I_ab_edge_list <- graph$edges
  save(em_I_ab_node_list, em_I_ab_edge_list, file = "data/em_I_ab.RData")
}

if(file.exists("data/em_Cs_ab.RData")) {
  load("data/em_Cs_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Cs_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Cs_nodes_ab.csv", directed = TRUE)
  em_Cs_ab_node_list <- graph$nodes
  em_Cs_ab_edge_list <- graph$edges
  save(em_Cs_ab_node_list, em_Cs_ab_edge_list, file = "data/em_Cs_ab.RData")
}

if(file.exists("data/em_Ba_ab.RData")) {
  load("data/em_Ba_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ba_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ba_nodes_ab.csv", directed = TRUE)
  em_Ba_ab_node_list <- graph$nodes
  em_Ba_ab_edge_list <- graph$edges
  save(em_Ba_ab_node_list, em_Ba_ab_edge_list, file = "data/em_Ba_ab.RData")
}

if(file.exists("data/em_La_ab.RData")) {
  load("data/em_La_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/La_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/La_nodes_ab.csv", directed = TRUE)
  em_La_ab_node_list <- graph$nodes
  em_La_ab_edge_list <- graph$edges
  save(em_La_ab_node_list, em_La_ab_edge_list, file = "data/em_La_ab.RData")
}

if(file.exists("data/em_Ce_ab.RData")) {
  load("data/em_Ce_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ce_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ce_nodes_ab.csv", directed = TRUE)
  em_Ce_ab_node_list <- graph$nodes
  em_Ce_ab_edge_list <- graph$edges
  save(em_Ce_ab_node_list, em_Ce_ab_edge_list, file = "data/em_Ce_ab.RData")
}



if(file.exists("data/em_Pr_ab.RData")) {
  load("data/em_Pr_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Pr_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Pr_nodes_ab.csv", directed = TRUE)
  em_Pr_ab_node_list <- graph$nodes
  em_Pr_ab_edge_list <- graph$edges
  save(em_Pr_ab_node_list, em_Pr_ab_edge_list, file = "data/em_Pr_ab.RData")
}


if(file.exists("data/em_Nd_ab.RData")) {
  load("data/em_Nd_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab//Nd_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Nd_nodes_ab.csv", directed = TRUE)
  em_Nd_ab_node_list <- graph$nodes
  em_Nd_ab_edge_list <- graph$edges
  save(em_Nd_ab_node_list, em_Nd_ab_edge_list, file = "data/em_Nd_ab.RData")
}

if(file.exists("data/em_Sm_ab.RData")) {
  load("data/em_Sm_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Sm_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Sm_nodes_ab.csv", directed = TRUE)
  em_Sm_ab_node_list <- graph$nodes
  em_Sm_ab_edge_list <- graph$edges
  save(em_Sm_ab_node_list, em_Sm_ab_edge_list, file = "data/em_Sm_ab.RData")
}

if(file.exists("data/em_Gd_ab.RData")) {
  load("data/em_Gd_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Gd_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Gd_nodes_ab.csv", directed = TRUE)
  em_Gd_ab_node_list <- graph$nodes
  em_Gd_ab_edge_list <- graph$edges
  save(em_Gd_ab_node_list, em_Gd_ab_edge_list, file = "data/em_Gd_ab.RData")
}

if(file.exists("data/em_Dy_ab.RData")) {
  load("data/em_Dy_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Dy_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Dy_nodes_ab.csv", directed = TRUE)
  em_Dy_ab_node_list <- graph$nodes
  em_Dy_ab_edge_list <- graph$edges
  save(em_Dy_ab_node_list, em_Dy_ab_edge_list, file = "data/em_Dy_ab.RData")
}

if(file.exists("data/em_Er_ab.RData")) {
  load("data/em_Er_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Er_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Er_nodes_ab.csv", directed = TRUE)
  em_Er_ab_node_list <- graph$nodes
  em_Er_ab_edge_list <- graph$edges
  save(em_Er_ab_node_list, em_Er_ab_edge_list, file = "data/em_Er_ab.RData")
}

if(file.exists("data/em_Yb_ab.RData")) {
  load("data/em_Yb_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Yb_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Yb_nodes_ab.csv", directed = TRUE)
  em_Yb_ab_node_list <- graph$nodes
  em_Yb_ab_edge_list <- graph$edges
  save(em_Yb_ab_node_list, em_Yb_ab_edge_list, file = "data/em_Yb_ab.RData")
}

if(file.exists("data/em_Hf_ab.RData")) {
  load("data/em_Hf_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Hf_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Hf_nodes_ab.csv", directed = TRUE)
  em_Hf_ab_node_list <- graph$nodes
  em_Hf_ab_edge_list <- graph$edges
  save(em_Hf_ab_node_list, em_Hf_ab_edge_list, file = "data/em_Hf_ab.RData")
}

if(file.exists("data/em_Ta_ab.RData")) {
  load("data/em_Ta_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ta_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ta_nodes_ab.csv", directed = TRUE)
  em_Ta_ab_node_list <- graph$nodes
  em_Ta_ab_edge_list <- graph$edges
  save(em_Ta_ab_node_list, em_Ta_ab_edge_list, file = "data/em_Ta_ab.RData")
}

if(file.exists("data/em_W_ab.RData")) {
  load("data/em_W_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/W_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/W_nodes_ab.csv", directed = TRUE)
  em_W_ab_node_list <- graph$nodes
  em_W_ab_edge_list <- graph$edges
  save(em_W_ab_node_list, em_W_ab_edge_list, file = "data/em_W_ab.RData")
}

if(file.exists("data/em_Re_ab.RData")) {
  load("data/em_Re_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Re_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Re_nodes_ab.csv", directed = TRUE)
  em_Re_ab_node_list <- graph$nodes
  em_Re_ab_edge_list <- graph$edges
  save(em_Re_ab_node_list, em_Re_ab_edge_list, file = "data/em_Re_ab.RData")
}

if(file.exists("data/em_Os_ab.RData")) {
  load("data/em_Os_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Os_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Os_nodes_ab.csv", directed = TRUE)
  em_Os_ab_node_list <- graph$nodes
  em_Os_ab_edge_list <- graph$edges
  save(em_Os_ab_node_list, em_Os_ab_edge_list, file = "data/em_Os_ab.RData")
}


if(file.exists("data/em_Ir_ab.RData")) {
  load("data/em_Ir_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Ir_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Ir_nodes_ab.csv", directed = TRUE)
  em_Ir_ab_node_list <- graph$nodes
  em_Ir_ab_edge_list <- graph$edges
  save(em_Ir_ab_node_list, em_Ir_ab_edge_list, file = "data/em_Ir_ab.RData")
}


if(file.exists("data/em_Pt_ab.RData")) {
  load("data/em_Pt_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Pt_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Pt_nodes_ab.csv", directed = TRUE)
  em_Pt_ab_node_list <- graph$nodes
  em_Pt_ab_edge_list <- graph$edges
  save(em_Pt_ab_node_list, em_Pt_ab_edge_list, file = "data/em_Pt_ab.RData")
}

if(file.exists("data/em_Au_ab.RData")) {
  load("data/em_Au_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Au_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Au_nodes_ab.csv", directed = TRUE)
  em_Au_ab_node_list <- graph$nodes
  em_Au_ab_edge_list <- graph$edges
  save(em_Au_ab_node_list, em_Au_ab_edge_list, file = "data/em_Au_ab.RData")
}


if(file.exists("data/em_Hg_ab.RData")) {
  load("data/em_Hg_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Hg_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Hg_nodes_ab.csv", directed = TRUE)
  em_Hg_ab_node_list <- graph$nodes
  em_Hg_ab_edge_list <- graph$edges
  save(em_Hg_ab_node_list, em_Hg_ab_edge_list, file = "data/em_Hg_ab.RData")
}


if(file.exists("data/em_Tl_ab.RData")) {
  load("data/em_Tl_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Tl_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Tl_nodes_ab.csv", directed = TRUE)
  em_Tl_ab_node_list <- graph$nodes
  em_Tl_ab_edge_list <- graph$edges
  save(em_Tl_ab_node_list, em_Tl_ab_edge_list, file = "data/em_Tl_ab.RData")
}

if(file.exists("data/em_Pb_ab.RData")) {
  load("data/em_Pb_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Pb_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Pb_nodes_ab.csv", directed = TRUE)
  em_Pb_ab_node_list <- graph$nodes
  em_Pb_ab_edge_list <- graph$edges
  save(em_Pb_ab_node_list, em_Pb_ab_edge_list, file = "data/em_Pb_ab.RData")
}

if(file.exists("data/em_Bi_ab.RData")) {
  load("data/em_Bi_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Bi_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Bi_nodes_ab.csv", directed = TRUE)
  em_Bi_ab_node_list <- graph$nodes
  em_Bi_ab_edge_list <- graph$edges
  save(em_Bi_ab_node_list, em_Bi_ab_edge_list, file = "data/em_Bi_ab.RData")
}

if(file.exists("data/em_Th_ab.RData")) {
  load("data/em_Th_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/Th_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/Th_nodes_ab.csv", directed = TRUE)
  em_Th_ab_node_list <- graph$nodes
  em_Th_ab_edge_list <- graph$edges
  save(em_Th_ab_node_list, em_Th_ab_edge_list, file = "data/em_Th_ab.RData")
}

if(file.exists("data/em_U_ab.RData")) {
  load("data/em_U_ab.RData")
} else {
  graph <- load_export("data/mineral_quantity_of_73_elm_ab/U_edges_ab.csv", "data/mineral_quantity_of_73_elm_ab/U_nodes_ab.csv", directed = TRUE)
  em_U_ab_node_list <- graph$nodes
  em_U_ab_edge_list <- graph$edges
  save(em_U_ab_node_list, em_U_ab_edge_list, file = "data/em_U_ab.RData")
}



####################
if(file.exists("data/loc_Li.RData")) {
  load("data/loc_Li.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Li_updated.csv", "data/ele_based_localities/loc_nodes_Li_updated.csv", directed = TRUE)
  loc_Li_node_list <- graph$nodes
  loc_Li_edge_list <- graph$edges
  save(loc_Li_node_list, loc_Li_edge_list, file = "data/loc_Li.RData")
}

if(file.exists("data/loc_Be.RData")) {
  load("data/loc_Be.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Be_updated.csv", "data/ele_based_localities/loc_nodes_Be_updated.csv", directed = TRUE)
  loc_Be_node_list <- graph$nodes
  loc_Be_edge_list <- graph$edges
  save(loc_Be_node_list, loc_Be_edge_list, file = "data/loc_Be.RData")
}

if(file.exists("data/loc_B.RData")) {
  load("data/loc_B.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_B_updated.csv", "data/ele_based_localities/loc_nodes_B_updated.csv", directed = TRUE)
  loc_B_node_list <- graph$nodes
  loc_B_edge_list <- graph$edges
  save(loc_B_node_list, loc_B_edge_list, file = "data/loc_B.RData")
}

if(file.exists("data/loc_C.RData")) {
  load("data/loc_C.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_C_updated.csv", "data/ele_based_localities/loc_nodes_C_updated.csv", directed = TRUE)
  loc_C_node_list <- graph$nodes
  loc_C_edge_list <- graph$edges
  save(loc_C_node_list, loc_C_edge_list, file = "data/loc_C.RData")
}

if(file.exists("data/loc_N.RData")) {
  load("data/loc_N.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_N_updated.csv", "data/ele_based_localities/loc_nodes_N_updated.csv", directed = TRUE)
  loc_N_node_list <- graph$nodes
  loc_N_edge_list <- graph$edges
  save(loc_N_node_list, loc_N_edge_list, file = "data/loc_N.RData")
}

if(file.exists("data/loc_F.RData")) {
  load("data/loc_F.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_F_updated.csv", "data/ele_based_localities/loc_nodes_F_updated.csv", directed = TRUE)
  loc_F_node_list <- graph$nodes
  loc_F_edge_list <- graph$edges
  save(loc_F_node_list, loc_F_edge_list, file = "data/loc_F.RData")
}

if(file.exists("data/loc_Na.RData")) {
  load("data/loc_Na.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Na_updated.csv", "data/ele_based_localities/loc_nodes_Na_updated.csv", directed = TRUE)
  loc_Na_node_list <- graph$nodes
  loc_Na_edge_list <- graph$edges
  save(loc_Na_node_list, loc_Na_edge_list, file = "data/loc_Na.RData")
}

if(file.exists("data/loc_Mg.RData")) {
  load("data/loc_Mg.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Mg_updated.csv", "data/ele_based_localities/loc_nodes_Mg_updated.csv", directed = TRUE)
  loc_Mg_node_list <- graph$nodes
  loc_Mg_edge_list <- graph$edges
  save(loc_Mg_node_list, loc_Mg_edge_list, file = "data/loc_Mg.RData")
}


if(file.exists("data/loc_Al.RData")) {
  load("data/loc_Al.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Al_updated.csv", "data/ele_based_localities/loc_nodes_Al_updated.csv", directed = TRUE)
  loc_Al_node_list <- graph$nodes
  loc_Al_edge_list <- graph$edges
  save(loc_Al_node_list, loc_Al_edge_list, file = "data/loc_Al.RData")
}

if(file.exists("data/loc_Si.RData")) {
  load("data/loc_Si.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Si_updated.csv", "data/ele_based_localities/loc_nodes_Si_updated.csv", directed = TRUE)
  loc_Si_node_list <- graph$nodes
  loc_Si_edge_list <- graph$edges
  save(loc_Si_node_list, loc_Si_edge_list, file = "data/loc_Si.RData")
}

if(file.exists("data/loc_P.RData")) {
  load("data/loc_P.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_P_updated.csv", "data/ele_based_localities/loc_nodes_P_updated.csv", directed = TRUE)
  loc_P_node_list <- graph$nodes
  loc_P_edge_list <- graph$edges
  save(loc_P_node_list, loc_P_edge_list, file = "data/loc_P.RData")
}

if(file.exists("data/loc_S.RData")) {
  load("data/loc_S.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_S_updated.csv", "data/ele_based_localities/loc_nodes_S_updated.csv", directed = TRUE)
  loc_S_node_list <- graph$nodes
  loc_S_edge_list <- graph$edges
  save(loc_S_node_list, loc_S_edge_list, file = "data/loc_S.RData")
}

if(file.exists("data/loc_Cl.RData")) {
  load("data/loc_Cl.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Cl_updated.csv", "data/ele_based_localities/loc_nodes_Cl_updated.csv", directed = TRUE)
  loc_Cl_node_list <- graph$nodes
  loc_Cl_edge_list <- graph$edges
  save(loc_Cl_node_list, loc_Cl_edge_list, file = "data/loc_Cl.RData")
}

if(file.exists("data/loc_K.RData")) {
  load("data/loc_K.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_K_updated.csv", "data/ele_based_localities/loc_nodes_K_updated.csv", directed = TRUE)
  loc_K_node_list <- graph$nodes
  loc_K_edge_list <- graph$edges
  save(loc_K_node_list, loc_K_edge_list, file = "data/loc_K.RData")
}

if(file.exists("data/loc_Ca.RData")) {
  load("data/loc_Ca.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ca_updated.csv", "data/ele_based_localities/loc_nodes_Ca_updated.csv", directed = TRUE)
  loc_Ca_node_list <- graph$nodes
  loc_Ca_edge_list <- graph$edges
  save(loc_Ca_node_list, loc_Ca_edge_list, file = "data/loc_Ca.RData")
}

if(file.exists("data/loc_Sc.RData")) {
  load("data/loc_Sc.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Sc_updated.csv", "data/ele_based_localities/loc_nodes_Sc_updated.csv", directed = TRUE)
  loc_Sc_node_list <- graph$nodes
  loc_Sc_edge_list <- graph$edges
  save(loc_Sc_node_list, loc_Sc_edge_list, file = "data/loc_Sc.RData")
}

if(file.exists("data/loc_Ti.RData")) {
  load("data/loc_Ti.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ti_updated.csv", "data/ele_based_localities/loc_nodes_Ti_updated.csv", directed = TRUE)
  loc_Ti_node_list <- graph$nodes
  loc_Ti_edge_list <- graph$edges
  save(loc_Ti_node_list, loc_Ti_edge_list, file = "data/loc_Ti.RData")
}

if(file.exists("data/loc_V.RData")) {
  load("data/loc_V.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_V_updated.csv", "data/ele_based_localities/loc_nodes_V_updated.csv", directed = TRUE)
  loc_V_node_list <- graph$nodes
  loc_V_edge_list <- graph$edges
  save(loc_V_node_list, loc_V_edge_list, file = "data/loc_V.RData")
}

if(file.exists("data/loc_Cr.RData")) {
  load("data/loc_Cr.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Cr_updated.csv", "data/ele_based_localities/loc_nodes_Cr_updated.csv", directed = TRUE)
  loc_Cr_node_list <- graph$nodes
  loc_Cr_edge_list <- graph$edges
  save(loc_Cr_node_list, loc_Cr_edge_list, file = "data/loc_Cr.RData")
}


if(file.exists("data/loc_Mn.RData")) {
  load("data/loc_Mn.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Mn_updated.csv", "data/ele_based_localities/loc_nodes_Mn_updated.csv", directed = TRUE)
  loc_Mn_node_list <- graph$nodes
  loc_Mn_edge_list <- graph$edges
  save(loc_Mn_node_list, loc_Mn_edge_list, file = "data/loc_Mn.RData")
}

if(file.exists("data/loc_Fe.RData")) {
  load("data/loc_Fe.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Fe_updated.csv", "data/ele_based_localities/loc_nodes_Fe_updated.csv", directed = TRUE)
  loc_Fe_node_list <- graph$nodes
  loc_Fe_edge_list <- graph$edges
  save(loc_Fe_node_list, loc_Fe_edge_list, file = "data/loc_Fe.RData")
}

if(file.exists("data/loc_Co.RData")) {
  load("data/loc_Co.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Co_updated.csv", "data/ele_based_localities/loc_nodes_Co_updated.csv", directed = TRUE)
  loc_Co_node_list <- graph$nodes
  loc_Co_edge_list <- graph$edges
  save(loc_Co_node_list, loc_Co_edge_list, file = "data/loc_Co.RData")
}

if(file.exists("data/loc_Ni.RData")) {
  load("data/loc_Ni.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ni_updated.csv", "data/ele_based_localities/loc_nodes_Ni_updated.csv", directed = TRUE)
  loc_Ni_node_list <- graph$nodes
  loc_Ni_edge_list <- graph$edges
  save(loc_Ni_node_list, loc_Ni_edge_list, file = "data/loc_Ni.RData")
}

if(file.exists("data/loc_Cu.RData")) {
  load("data/loc_Cu.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Cu_updated.csv", "data/ele_based_localities/loc_nodes_Cu_updated.csv", directed = TRUE)
  loc_Cu_node_list <- graph$nodes
  loc_Cu_edge_list <- graph$edges
  save(loc_Cu_node_list, loc_Cu_edge_list, file = "data/loc_Cu.RData")
}

if(file.exists("data/loc_Zn.RData")) {
  load("data/loc_Zn.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Zn_updated.csv", "data/ele_based_localities/loc_nodes_Zn_updated.csv", directed = TRUE)
  loc_Zn_node_list <- graph$nodes
  loc_Zn_edge_list <- graph$edges
  save(loc_Zn_node_list, loc_Zn_edge_list, file = "data/loc_Zn.RData")
}

if(file.exists("data/loc_Ga.RData")) {
  load("data/loc_Ga.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ga_updated.csv", "data/ele_based_localities/loc_nodes_Ga_updated.csv", directed = TRUE)
  loc_Ga_node_list <- graph$nodes
  loc_Ga_edge_list <- graph$edges
  save(loc_Ga_node_list, loc_Ga_edge_list, file = "data/loc_Ga.RData")
}

if(file.exists("data/loc_Ge.RData")) {
  load("data/loc_Ge.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ge_updated.csv", "data/ele_based_localities/loc_nodes_Ge_updated.csv", directed = TRUE)
  loc_Ge_node_list <- graph$nodes
  loc_Ge_edge_list <- graph$edges
  save(loc_Ge_node_list, loc_Ge_edge_list, file = "data/loc_Ge.RData")
}

if(file.exists("data/loc_As.RData")) {
  load("data/loc_As.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_As_updated.csv", "data/ele_based_localities/loc_nodes_As_updated.csv", directed = TRUE)
  loc_As_node_list <- graph$nodes
  loc_As_edge_list <- graph$edges
  save(loc_As_node_list, loc_As_edge_list, file = "data/loc_As.RData")
}

if(file.exists("data/loc_Se.RData")) {
  load("data/loc_Se.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Se_updated.csv", "data/ele_based_localities/loc_nodes_Se_updated.csv", directed = TRUE)
  loc_Se_node_list <- graph$nodes
  loc_Se_edge_list <- graph$edges
  save(loc_Se_node_list, loc_Se_edge_list, file = "data/loc_Se.RData")
}

if(file.exists("data/loc_Br.RData")) {
  load("data/loc_Br.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Br_updated.csv", "data/ele_based_localities/loc_nodes_Br_updated.csv", directed = TRUE)
  loc_Br_node_list <- graph$nodes
  loc_Br_edge_list <- graph$edges
  save(loc_Br_node_list, loc_Br_edge_list, file = "data/loc_Br.RData")
}

if(file.exists("data/loc_Rb.RData")) {
  load("data/loc_Rb.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Rb_updated.csv", "data/ele_based_localities/loc_nodes_Rb_updated.csv", directed = TRUE)
  loc_Rb_node_list <- graph$nodes
  loc_Rb_edge_list <- graph$edges
  save(loc_Rb_node_list, loc_Rb_edge_list, file = "data/loc_Rb.RData")
}

if(file.exists("data/loc_Sr.RData")) {
  load("data/loc_Sr.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Sr_updated.csv", "data/ele_based_localities/loc_nodes_Sr_updated.csv", directed = TRUE)
  loc_Sr_node_list <- graph$nodes
  loc_Sr_edge_list <- graph$edges
  save(loc_Sr_node_list, loc_Sr_edge_list, file = "data/loc_Sr.RData")
}

if(file.exists("data/loc_Y.RData")) {
  load("data/loc_Y.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Y_updated.csv", "data/ele_based_localities/loc_nodes_Y_updated.csv", directed = TRUE)
  loc_Y_node_list <- graph$nodes
  loc_Y_edge_list <- graph$edges
  save(loc_Y_node_list, loc_Y_edge_list, file = "data/loc_Y.RData")
}

if(file.exists("data/loc_Zr.RData")) {
  load("data/loc_Zr.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Zr_updated.csv", "data/ele_based_localities/loc_nodes_Zr_updated.csv", directed = TRUE)
  loc_Zr_node_list <- graph$nodes
  loc_Zr_edge_list <- graph$edges
  save(loc_Zr_node_list, loc_Zr_edge_list, file = "data/loc_Zr.RData")
}

if(file.exists("data/loc_Nb.RData")) {
  load("data/loc_Nb.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Nb_updated.csv", "data/ele_based_localities/loc_nodes_Nb_updated.csv", directed = TRUE)
  loc_Nb_node_list <- graph$nodes
  loc_Nb_edge_list <- graph$edges
  save(loc_Nb_node_list, loc_Nb_edge_list, file = "data/loc_Nb.RData")
}

if(file.exists("data/loc_Mo.RData")) {
  load("data/loc_Mo.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Mo_updated.csv", "data/ele_based_localities/loc_nodes_Mo_updated.csv", directed = TRUE)
  loc_Mo_node_list <- graph$nodes
  loc_Mo_edge_list <- graph$edges
  save(loc_Mo_node_list, loc_Mo_edge_list, file = "data/loc_Mo.RData")
}

if(file.exists("data/loc_Ru.RData")) {
  load("data/loc_Ru.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ru_updated.csv", "data/ele_based_localities/loc_nodes_Ru_updated.csv", directed = TRUE)
  loc_Ru_node_list <- graph$nodes
  loc_Ru_edge_list <- graph$edges
  save(loc_Ru_node_list, loc_Ru_edge_list, file = "data/loc_Ru.RData")
}

if(file.exists("data/loc_Rh.RData")) {
  load("data/loc_Rh.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Rh_updated.csv", "data/ele_based_localities/loc_nodes_Rh_updated.csv", directed = TRUE)
  loc_Rh_node_list <- graph$nodes
  loc_Rh_edge_list <- graph$edges
  save(loc_Rh_node_list, loc_Rh_edge_list, file = "data/loc_Rh.RData")
}

if(file.exists("data/loc_Pd.RData")) {
  load("data/loc_Pd.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Pd_updated.csv", "data/ele_based_localities/loc_nodes_Pd_updated.csv", directed = TRUE)
  loc_Pd_node_list <- graph$nodes
  loc_Pd_edge_list <- graph$edges
  save(loc_Pd_node_list, loc_Pd_edge_list, file = "data/loc_Pd.RData")
}

if(file.exists("data/loc_Ag.RData")) {
  load("data/loc_Ag.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ag_updated.csv", "data/ele_based_localities/loc_nodes_Ag_updated.csv", directed = TRUE)
  loc_Ag_node_list <- graph$nodes
  loc_Ag_edge_list <- graph$edges
  save(loc_Ag_node_list, loc_Ag_edge_list, file = "data/loc_Ag.RData")
}

if(file.exists("data/loc_Cd.RData")) {
  load("data/loc_Cd.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Cd_updated.csv", "data/ele_based_localities/loc_nodes_Cd_updated.csv", directed = TRUE)
  loc_Cd_node_list <- graph$nodes
  loc_Cd_edge_list <- graph$edges
  save(loc_Cd_node_list, loc_Cd_edge_list, file = "data/loc_Cd.RData")
}

if(file.exists("data/loc_In.RData")) {
  load("data/loc_In.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_In_updated.csv", "data/ele_based_localities/loc_nodes_In_updated.csv", directed = TRUE)
  loc_In_node_list <- graph$nodes
  loc_In_edge_list <- graph$edges
  save(loc_In_node_list, loc_In_edge_list, file = "data/loc_In.RData")
}

if(file.exists("data/loc_Sn.RData")) {
  load("data/loc_Sn.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Sn_updated.csv", "data/ele_based_localities/loc_nodes_Sn_updated.csv", directed = TRUE)
  loc_Sn_node_list <- graph$nodes
  loc_Sn_edge_list <- graph$edges
  save(loc_Sn_node_list, loc_Sn_edge_list, file = "data/loc_Sn.RData")
}

if(file.exists("data/loc_Sb.RData")) {
  load("data/loc_Sb.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Sb_updated.csv", "data/ele_based_localities/loc_nodes_Sb_updated.csv", directed = TRUE)
  loc_Sb_node_list <- graph$nodes
  loc_Sb_edge_list <- graph$edges
  save(loc_Sb_node_list, loc_Sb_edge_list, file = "data/loc_Sb.RData")
}

if(file.exists("data/loc_Te.RData")) {
  load("data/loc_Te.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Te_updated.csv", "data/ele_based_localities/loc_nodes_Te_updated.csv", directed = TRUE)
  loc_Te_node_list <- graph$nodes
  loc_Te_edge_list <- graph$edges
  save(loc_Te_node_list, loc_Te_edge_list, file = "data/loc_Te.RData")
}

if(file.exists("data/loc_I.RData")) {
  load("data/loc_I.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_I_updated.csv", "data/ele_based_localities/loc_nodes_I_updated.csv", directed = TRUE)
  loc_I_node_list <- graph$nodes
  loc_I_edge_list <- graph$edges
  save(loc_I_node_list, loc_I_edge_list, file = "data/loc_I.RData")
}

if(file.exists("data/loc_Cs.RData")) {
  load("data/loc_Cs.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Cs_updated.csv", "data/ele_based_localities/loc_nodes_Cs_updated.csv", directed = TRUE)
  loc_Cs_node_list <- graph$nodes
  loc_Cs_edge_list <- graph$edges
  save(loc_Cs_node_list, loc_Cs_edge_list, file = "data/loc_Cs.RData")
}

if(file.exists("data/loc_Ba.RData")) {
  load("data/loc_Ba.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ba_updated.csv", "data/ele_based_localities/loc_nodes_Ba_updated.csv", directed = TRUE)
  loc_Ba_node_list <- graph$nodes
  loc_Ba_edge_list <- graph$edges
  save(loc_Ba_node_list, loc_Ba_edge_list, file = "data/loc_Ba.RData")
}

if(file.exists("data/loc_La.RData")) {
  load("data/loc_La.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_La_updated.csv", "data/ele_based_localities/loc_nodes_La_updated.csv", directed = TRUE)
  loc_La_node_list <- graph$nodes
  loc_La_edge_list <- graph$edges
  save(loc_La_node_list, loc_La_edge_list, file = "data/loc_La.RData")
}

if(file.exists("data/loc_Ce.RData")) {
  load("data/loc_Ce.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ce_updated.csv", "data/ele_based_localities/loc_nodes_Ce_updated.csv", directed = TRUE)
  loc_Ce_node_list <- graph$nodes
  loc_Ce_edge_list <- graph$edges
  save(loc_Ce_node_list, loc_Ce_edge_list, file = "data/loc_Ce.RData")
}

if(file.exists("data/loc_Nd.RData")) {
  load("data/loc_Nd.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Nd_updated.csv", "data/ele_based_localities/loc_nodes_Nd_updated.csv", directed = TRUE)
  loc_Nd_node_list <- graph$nodes
  loc_Nd_edge_list <- graph$edges
  save(loc_Nd_node_list, loc_Nd_edge_list, file = "data/loc_Nd.RData")
}

if(file.exists("data/loc_Sm.RData")) {
  load("data/loc_Sm.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Sm_updated.csv", "data/ele_based_localities/loc_nodes_Sm_updated.csv", directed = TRUE)
  loc_Sm_node_list <- graph$nodes
  loc_Sm_edge_list <- graph$edges
  save(loc_Sm_node_list, loc_Sm_edge_list, file = "data/loc_Sm.RData")
}

if(file.exists("data/loc_Gd.RData")) {
  load("data/loc_Gd.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Gd_updated.csv", "data/ele_based_localities/loc_nodes_Gd_updated.csv", directed = TRUE)
  loc_Gd_node_list <- graph$nodes
  loc_Gd_edge_list <- graph$edges
  save(loc_Gd_node_list, loc_Gd_edge_list, file = "data/loc_Gd.RData")
}

if(file.exists("data/loc_Dy.RData")) {
  load("data/loc_Dy.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Dy_updated.csv", "data/ele_based_localities/loc_nodes_Dy_updated.csv", directed = TRUE)
  loc_Dy_node_list <- graph$nodes
  loc_Dy_edge_list <- graph$edges
  save(loc_Dy_node_list, loc_Dy_edge_list, file = "data/loc_Dy.RData")
}

if(file.exists("data/loc_Er.RData")) {
  load("data/loc_Er.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Er_updated.csv", "data/ele_based_localities/loc_nodes_Er_updated.csv", directed = TRUE)
  loc_Er_node_list <- graph$nodes
  loc_Er_edge_list <- graph$edges
  save(loc_Er_node_list, loc_Er_edge_list, file = "data/loc_Er.RData")
}

if(file.exists("data/loc_Yb.RData")) {
  load("data/loc_Yb.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Yb_updated.csv", "data/ele_based_localities/loc_nodes_Yb_updated.csv", directed = TRUE)
  loc_Yb_node_list <- graph$nodes
  loc_Yb_edge_list <- graph$edges
  save(loc_Yb_node_list, loc_Yb_edge_list, file = "data/loc_Yb.RData")
}

if(file.exists("data/loc_Hf.RData")) {
  load("data/loc_Hf.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Hf_updated.csv", "data/ele_based_localities/loc_nodes_Hf_updated.csv", directed = TRUE)
  loc_Hf_node_list <- graph$nodes
  loc_Hf_edge_list <- graph$edges
  save(loc_Hf_node_list, loc_Hf_edge_list, file = "data/loc_Hf.RData")
}

if(file.exists("data/loc_Ta.RData")) {
  load("data/loc_Ta.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ta_updated.csv", "data/ele_based_localities/loc_nodes_Ta_updated.csv", directed = TRUE)
  loc_Ta_node_list <- graph$nodes
  loc_Ta_edge_list <- graph$edges
  save(loc_Ta_node_list, loc_Ta_edge_list, file = "data/loc_Ta.RData")
}

if(file.exists("data/loc_W.RData")) {
  load("data/loc_W.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_W_updated.csv", "data/ele_based_localities/loc_nodes_W_updated.csv", directed = TRUE)
  loc_W_node_list <- graph$nodes
  loc_W_edge_list <- graph$edges
  save(loc_W_node_list, loc_W_edge_list, file = "data/loc_W.RData")
}

if(file.exists("data/loc_Re.RData")) {
  load("data/loc_Re.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Re_updated.csv", "data/ele_based_localities/loc_nodes_Re_updated.csv", directed = TRUE)
  loc_Re_node_list <- graph$nodes
  loc_Re_edge_list <- graph$edges
  save(loc_Re_node_list, loc_Re_edge_list, file = "data/loc_Re.RData")
}

if(file.exists("data/loc_Os.RData")) {
  load("data/loc_Os.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Os_updated.csv", "data/ele_based_localities/loc_nodes_Os_updated.csv", directed = TRUE)
  loc_Os_node_list <- graph$nodes
  loc_Os_edge_list <- graph$edges
  save(loc_Os_node_list, loc_Os_edge_list, file = "data/loc_Os.RData")
}


if(file.exists("data/loc_Ir.RData")) {
  load("data/loc_Ir.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Ir_updated.csv", "data/ele_based_localities/loc_nodes_Ir_updated.csv", directed = TRUE)
  loc_Ir_node_list <- graph$nodes
  loc_Ir_edge_list <- graph$edges
  save(loc_Ir_node_list, loc_Ir_edge_list, file = "data/loc_Ir.RData")
}

if(file.exists("data/loc_Pt.RData")) {
  load("data/loc_Pt.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Pt_updated.csv", "data/ele_based_localities/loc_nodes_Pt_updated.csv", directed = TRUE)
  loc_Pt_node_list <- graph$nodes
  loc_Pt_edge_list <- graph$edges
  save(loc_Pt_node_list, loc_Pt_edge_list, file = "data/loc_Pt.RData")
}

if(file.exists("data/loc_Au.RData")) {
  load("data/loc_Au.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Au_updated.csv", "data/ele_based_localities/loc_nodes_Au_updated.csv", directed = TRUE)
  loc_Au_node_list <- graph$nodes
  loc_Au_edge_list <- graph$edges
  save(loc_Au_node_list, loc_Au_edge_list, file = "data/loc_Au.RData")
}


if(file.exists("data/loc_Hg.RData")) {
  load("data/loc_Hg.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Hg_updated.csv", "data/ele_based_localities/loc_nodes_Hg_updated.csv", directed = TRUE)
  loc_Hg_node_list <- graph$nodes
  loc_Hg_edge_list <- graph$edges
  save(loc_Hg_node_list, loc_Hg_edge_list, file = "data/loc_Hg.RData")
}

if(file.exists("data/loc_Tl.RData")) {
  load("data/loc_Tl.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Tl_updated.csv", "data/ele_based_localities/loc_nodes_Tl_updated.csv", directed = TRUE)
  loc_Tl_node_list <- graph$nodes
  loc_Tl_edge_list <- graph$edges
  save(loc_Tl_node_list, loc_Tl_edge_list, file = "data/loc_Tl.RData")
}

if(file.exists("data/loc_Pb.RData")) {
  load("data/loc_Pb.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Pb_updated.csv", "data/ele_based_localities/loc_nodes_Pb_updated.csv", directed = TRUE)
  loc_Pb_node_list <- graph$nodes
  loc_Pb_edge_list <- graph$edges
  save(loc_Pb_node_list, loc_Pb_edge_list, file = "data/loc_Pb.RData")
}

if(file.exists("data/loc_Bi.RData")) {
  load("data/loc_Bi.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Bi_updated.csv", "data/ele_based_localities/loc_nodes_Bi_updated.csv", directed = TRUE)
  loc_Bi_node_list <- graph$nodes
  loc_Bi_edge_list <- graph$edges
  save(loc_Bi_node_list, loc_Bi_edge_list, file = "data/loc_Bi.RData")
}

if(file.exists("data/loc_Th.RData")) {
  load("data/loc_Th.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_Th_updated.csv", "data/ele_based_localities/loc_nodes_Th_updated.csv", directed = TRUE)
  loc_Th_node_list <- graph$nodes
  loc_Th_edge_list <- graph$edges
  save(loc_Th_node_list, loc_Th_edge_list, file = "data/loc_Th.RData")
}

if(file.exists("data/loc_U.RData")) {
  load("data/loc_U.RData")
} else {
  graph <- load_export("data/ele_based_localities/loc_edges_U_updated.csv", "data/ele_based_localities/loc_nodes_U_updated.csv", directed = TRUE)
  loc_U_node_list <- graph$nodes
  loc_U_edge_list <- graph$edges
  save(loc_U_node_list, loc_U_edge_list, file = "data/loc_U.RData")
}
