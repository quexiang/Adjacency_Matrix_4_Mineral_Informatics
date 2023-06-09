library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)

elm_minerallist <- {c(
  "element-mineral_All"="em_all_elements",
  "element-mineral_Ag" = "em_Ag",
  "element-mineral_Al" = "em_Al",
  "element-mineral_Au" = "em_Au",
  "element-mineral_As" = "em_As",
  "element-mineral_B" = "em_B",
  "element-mineral_Ba" = "em_Ba",
  "element-mineral_Be" = "em_Be",
  "element-mineral_Bi" = "em_Bi",
  "element-mineral_Br" = "em_Br",
  "element-mineral_C" = "em_C",
  "element-mineral_Ca" = "em_Ca",
  "element-mineral_Cd" = "em_Cd",
  "element-mineral_Ce" = "em_Ce",
  "element-mineral_Co" = "em_Co",
  "element-mineral_Cl" = "em_Cl",
  "element-mineral_Cr" = "em_Cr",
  "element-mineral_Cs" = "em_Cs",
  "element-mineral_Cu" = "em_Cu",
  "element-mineral_Dy" = "em_Dy",
  "element-mineral_Er" = "em_Er",
  "element-mineral_O" = "em_O",
  "element-mineral_Pd" = "em_Pd",
  "element-mineral_Pr" = "em_Pr",
  "element-mineral_Pt" = "em_Pt",
  "element-mineral_Rb" = "em_Rb",
  "element-mineral_Rh" = "em_Rh",
  "element-mineral_Ru" = "em_Ru",
  "element-mineral_F" = "em_F",
  "element-mineral_Fe" = "em_Fe",
  "element-mineral_Ga" = "em_Ga",
  "element-mineral_Ge" = "em_Ge",
  "element-mineral_Gd" = "em_Gd",
  "element-mineral_H" = "em_H",
  "element-mineral_Hf" = "em_Hf",
  "element-mineral_Hg" = "em_Hg",
  "element-mineral_I" = "em_I",
  "element-mineral_In" = "em_In",
  "element-mineral_Ir" = "em_Ir",
  "element-mineral_K" = "em_K",
  "element-mineral_La" = "em_La",
  "element-mineral_Li" = "em_Li",
  "element-mineral_Mg" = "em_Mg",
  "element-mineral_Mn" = "em_Mn",
  "element-mineral_Mo" = "em_Mo",
  "element-mineral_N" = "em_N",
  "element-mineral_Na" = "em_Na",
  "element-mineral_Nb" = "em_Nb",
  "element-mineral_Nd" = "em_Nd",
  "element-mineral_Ni" = "em_Ni",
  "element-mineral_Os" = "em_Os",
  "element-mineral_P" = "em_P",
  "element-mineral_Pt" = "em_Pt",
  "element-mineral_Pb" = "em_Pb",
  "element-mineral_Re" = "em_Re",
  "element-mineral_S" = "em_S",
  "element-mineral_Sc" = "em_Sc",
  "element-mineral_Sb" = "em_Sb",
  "element-mineral_Se" = "em_Se",
  "element-mineral_Si" = "em_Si",
  "element-mineral_Sm" = "em_Sm",
  "element-mineral_Sn" = "em_Sn",
  "element-mineral_Sr" = "em_Sr",
  "element-mineral_Ta" = "em_Ta",
  "element-mineral_Te" = "em_Te",
  "element-mineral_Th" = "em_Th",
  "element-mineral_Ti" = "em_Ti",
  "element-mineral_Tl" = "em_Tl",
  "element-mineral_U" = "em_U",
  "element-mineral_V" = "em_V",
  "element-mineral_W" = "em_W",
  "element-mineral_Y" = "em_Y",
  "element-mineral_Yb" = "em_Yb",
  "element-mineral_Zn" = "em_Zn",
  "element-mineral_Zr" = "em_Zr"
  
)}


elm_ab_minerallist <- {c(
  "element-mineral_ab_All"="em_all_ab_elements",
  "element-mineral_Ag_ab" = "em_Ag_ab",
  "element-mineral_Al_ab" = "em_Al_ab",
  "element-mineral_Au_ab" = "em_Au_ab",
  "element-mineral_As_ab" = "em_As_ab",
  "element-mineral_B_ab" = "em_B_ab",
  "element-mineral_Ba_ab" = "em_Ba_ab",
  "element-mineral_Be_ab" = "em_Be_ab",
  "element-mineral_Bi_ab" = "em_Bi_ab",
  "element-mineral_Br_ab" = "em_Br_ab",
  "element-mineral_C_ab" = "em_C_ab",
  "element-mineral_Ca_ab" = "em_Ca_ab",
  "element-mineral_Cd_ab" = "em_Cd_ab",
  "element-mineral_Ce_ab" = "em_Ce_ab",
  "element-mineral_Co_ab" = "em_Co_ab",
  "element-mineral_Cl_ab" = "em_Cl_ab",
  "element-mineral_Cr_ab" = "em_Cr_ab",
  "element-mineral_Cs_ab" = "em_Cs_ab",
  "element-mineral_Cu_ab" = "em_Cu_ab",
  "element-mineral_Dy_ab" = "em_Dy_ab",
  "element-mineral_Er_ab" = "em_Er_ab",
  "element-mineral_O_ab" = "em_O_ab",
  "element-mineral_Pd_ab" = "em_Pd_ab",
  "element-mineral_Rb_ab" = "em_Rb_ab",
  "element-mineral_Rh_ab" = "em_Rh_ab",
  "element-mineral_Ru_ab" = "em_Ru_ab",
  "element-mineral_F_ab" = "em_F_ab",
  "element-mineral_Fe_ab" = "em_Fe_ab",
  "element-mineral_Ga_ab" = "em_Ga_ab",
  "element-mineral_Ge_ab" = "em_Ge_ab",
  "element-mineral_Gd_ab" = "em_Gd_ab",
  "element-mineral_H_ab" = "em_H_ab",
  "element-mineral_Hf_ab" = "em_Hf_ab",
  "element-mineral_Hg_ab" = "em_Hg_ab",
  "element-mineral_I_ab" = "em_I_ab",
  "element-mineral_In_ab" = "em_In_ab",
  "element-mineral_Ir_ab" = "em_Ir_ab",
  "element-mineral_K_ab" = "em_K_ab",
  "element-mineral_La_ab" = "em_La_ab",
  "element-mineral_Li_ab" = "em_Li_ab",
  "element-mineral_Mg_ab" = "em_Mg_ab",
  "element-mineral_Mn_ab" = "em_Mn_ab",
  "element-mineral_Mo_ab" = "em_Mo_ab",
  "element-mineral_N_ab" = "em_N_ab",
  "element-mineral_Na_ab" = "em_Na_ab",
  "element-mineral_Nb_ab" = "em_Nb_ab",
  "element-mineral_Nd_ab" = "em_Nd_ab",
  "element-mineral_Ni_ab" = "em_Ni_ab",
  "element-mineral_Os_ab" = "em_Os_ab",
  "element-mineral_P_ab" = "em_P_ab",
  "element-mineral_Pb_ab" = "em_Pb_ab",
  "element-mineral_Pr_ab" = "em_Pr_ab",
  "element-mineral_Pt_ab" = "em_Pt_ab",
  "element-mineral_Re_ab" = "em_Re_ab",
  "element-mineral_S_ab" = "em_S_ab",
  "element-mineral_Sc_ab" = "em_Sc_ab",
  "element-mineral_Sb_ab" = "em_Sb_ab",
  "element-mineral_Se_ab" = "em_Se_ab",
  "element-mineral_Si_ab" = "em_Si_ab",
  "element-mineral_Sm_ab" = "em_Sm_ab",
  "element-mineral_Sn_ab" = "em_Sn_ab",
  "element-mineral_Sr_ab" = "em_Sr_ab",
  "element-mineral_Ta_ab" = "em_Ta_ab",
  "element-mineral_Te_ab" = "em_Te_ab",
  "element-mineral_Th_ab" = "em_Th_ab",
  "element-mineral_Ti_ab" = "em_Ti_ab",
  "element-mineral_Tl_ab" = "em_Tl_ab",
  "element-mineral_U_ab" = "em_U_ab",
  "element-mineral_V_ab" = "em_V_ab",
  "element-mineral_W_ab" = "em_W_ab",
  "element-mineral_Y_ab" = "em_Y_ab",
  "element-mineral_Yb_ab" = "em_Yb_ab",
  "element-mineral_Zn_ab" = "em_Zn_ab",
  "element-mineral_Zr_ab" = "em_Zr_ab"
)}


loc_mineral_list <- {c(
  "loc_mineral_Ag" = "loc_Ag",
  "loc_mineral_Al" = "loc_Al",
  "loc_mineral_Au" = "loc_Au",
  "loc_mineral_As" = "loc_As",
  "loc_mineral_B" = "loc_B",
  "loc_mineral_Ba" = "loc_Ba",
  "loc_mineral_Be" = "loc_Be",
  "loc_mineral_Bi" = "loc_Bi",
  "loc_mineral_Br" = "loc_Br",
  "loc_mineral_C" = "loc_C",
  "loc_mineral_Ca" = "loc_Ca",
  "loc_mineral_Cd" = "loc_Cd",
  "loc_mineral_Ce" = "loc_Ce",
  "loc_mineral_Co" = "loc_Co",
  "loc_mineral_Cl" = "loc_Cl",
  "loc_mineral_Cr" = "loc_Cr",
  "loc_mineral_Cs" = "loc_Cs",
  "loc_mineral_Cu" = "loc_Cu",
  "loc_mineral_Dy" = "loc_Dy",
  "loc_mineral_Er" = "loc_Er",
  "loc_mineral_O" = "loc_O",
  "loc_mineral_Pd" = "loc_Pd",
  "loc_mineral_Rb" = "loc_Rb",
  "loc_mineral_Rh" = "loc_Rh",
  "loc_mineral_Ru" = "loc_Ru",
  "loc_mineral_F" = "loc_F",
  "loc_mineral_Fe" = "loc_Fe",
  "loc_mineral_Ga" = "loc_Ga",
  "loc_mineral_Ge" = "loc_Ge",
  "loc_mineral_Gd" = "loc_Gd",
  "loc_mineral_H" = "loc_H",
  "loc_mineral_Hf" = "loc_Hf",
  "loc_mineral_Hg" = "loc_Hg",
  "loc_mineral_I" = "loc_I",
  "loc_mineral_In" = "loc_In",
  "loc_mineral_Ir" = "loc_Ir",
  "loc_mineral_K" = "loc_K",
  "loc_mineral_La" = "loc_La",
  "loc_mineral_Li" = "loc_Li",
  "loc_mineral_Mg" = "loc_Mg",
  "loc_mineral_Mn" = "loc_Mn",
  "loc_mineral_Mo" = "loc_Mo",
  "loc_mineral_N" = "loc_N",
  "loc_mineral_Na" = "loc_Na",
  "loc_mineral_Nb" = "loc_Nb",
  "loc_mineral_Nd" = "loc_Nd",
  "loc_mineral_Ni" = "loc_Ni",
  "loc_mineral_Os" = "loc_Os",
  "loc_mineral_P" = "loc_P",
  "loc_mineral_Pt" = "loc_Pt",
  "loc_mineral_Pb" = "loc_Pb",
  "loc_mineral_Re" = "loc_Re",
  "loc_mineral_S" = "loc_S",
  "loc_mineral_Sc" = "loc_Sc",
  "loc_mineral_Sb" = "loc_Sb",
  "loc_mineral_Se" = "loc_Se",
  "loc_mineral_Si" = "loc_Si",
  "loc_mineral_Sm" = "loc_Sm",
  "loc_mineral_Sn" = "loc_Sn",
  "loc_mineral_Sr" = "loc_Sr",
  "loc_mineral_Ta" = "loc_Ta",
  "loc_mineral_Te" = "loc_Te",
  "loc_mineral_Th" = "loc_Th",
  "loc_mineral_Ti" = "loc_Ti",
  "loc_mineral_Tl" = "loc_Tl",
  "loc_mineral_U" = "loc_U",
  "loc_mineral_V" = "loc_V",
  "loc_mineral_W" = "loc_W",
  "loc_mineral_Y" = "loc_Y",
  "loc_mineral_Yb" = "loc_Yb",
  "loc_mineral_Zn" = "loc_Zn",
  "loc_mineral_Zr" = "loc_Zr"
)}

igneous_mineral_list <- {c(
  "igneous_minerals"="igneous"
)}

o_spinel_mineral_list <- {c(
  "oxygen_spinel"="o_spinel"
)}

shinyServer(function(input, output, session) {
  
  output$dataset_description <- renderUI({
    switch(
      input$cases_source,
      "elm_mineral" = includeMarkdown("data/description/elm_mineral.md"),
      "elm_mineral_ab" = includeMarkdown("data/description/elm_mineral_ab.md"),
      "mineral_loc" = includeMarkdown("data/description/mineral_loc.md"),
      "igneous_minerals" = includeMarkdown("data/description/igneous_minerals.md"),
      "oxygen_spinel" = includeMarkdown("data/description/oxygen_spinel.md")
    )
  })
  
  case_list<-reactive({switch(
    input$cases_source,
    "elm_mineral" = elm_minerallist,
    "elm_mineral_ab" = elm_ab_minerallist,
    "mineral_loc" = loc_mineral_list,
    "igneous_minerals" = igneous_mineral_list,
    "oxygen_spinel" = o_spinel_mineral_list
  )}
  )
  
  #  # Generate a selection menu for community detection choices
  output$graph_set <- renderUI({
    case_namelist <- case_list()
    print(case_namelist)
    return(selectInput(
      "graph_set",
      "Data Set Selection",
      choices = case_namelist, #elm_minerallist,#
      selected = 0 ## "em_Li"
    ))
  })
  
  
  
  node_list <- reactive({
    switch(
      input$graph_set,
      "em_all_elements" = em_all_node_list,
      "em_Ag" = em_Ag_node_list,
      "em_Al" = em_Al_node_list,
      "em_Au" = em_Au_node_list,
      "em_As" = em_As_node_list,
      "em_B" = em_B_node_list,
      "em_Ba" = em_Ba_node_list,
      "em_Be" = em_Be_node_list,
      "em_Bi" = em_Bi_node_list,
      "em_Br" = em_Br_node_list,
      "em_C" = em_C_node_list,
      "em_Ca" = em_Ca_node_list,
      "em_Cd" = em_Cd_node_list,
      "em_Ce" = em_Ce_node_list,
      "em_Cl" = em_Cl_node_list,
      "em_Co" = em_Co_node_list,
      "em_Cu" = em_Cu_node_list,
      "em_Cr" = em_Cr_node_list,
      "em_Cs" = em_Cs_node_list,
      "em_Dy" = em_Dy_node_list,
      "em_Er" = em_Er_node_list,
      "em_F" = em_F_node_list,
      "em_Fe" = em_Fe_node_list,
      "em_Ga" = em_Ga_node_list,
      "em_Gd" = em_Gd_node_list,
      "em_Ge" = em_Ge_node_list,
      "em_H" = em_H_node_list,
      "em_Hg" = em_Hg_node_list,
      "em_Hf" = em_Hf_node_list,
      "em_I" = em_I_node_list,
      "em_In" = em_In_node_list,
      "em_Ir" = em_Ir_node_list,
      "em_K" = em_K_node_list,
      "em_La" = em_La_node_list,
      "em_Li" = em_Li_node_list,
      "em_Mg" = em_Mg_node_list,
      "em_Mn" = em_Mn_node_list,
      "em_Mo" = em_Mo_node_list,
      "em_N" = em_N_node_list,
      "em_Na" = em_Na_node_list,
      "em_Nb" = em_Nb_node_list,
      "em_Nd" = em_Nd_node_list,
      "em_Ni" = em_Ni_node_list,
      "em_O" = em_O_node_list,
      "em_Os" = em_Os_node_list,
      "em_P" = em_P_node_list,
      "em_Pd" = em_Pd_node_list,
      "em_Pb" = em_Pb_node_list,
      "em_Pr" = em_Pr_node_list,
      "em_Pt" = em_Pt_node_list,
      "em_Rb" = em_Rb_node_list,
      "em_Re" = em_Re_node_list,
      "em_Ru" = em_Ru_node_list,
      "em_Rh" = em_Rh_node_list,
      "em_S" = em_S_node_list,
      "em_Sm" = em_Sm_node_list,
      "em_Sb" = em_Sb_node_list,
      "em_Sc" = em_Sc_node_list,
      "em_Se" = em_Se_node_list,
      "em_Si" = em_Si_node_list,
      "em_Sn" = em_Sn_node_list,
      "em_Sr" = em_Sr_node_list,
      "em_Ta" = em_Ta_node_list,
      "em_Te" = em_Te_node_list,
      "em_Th" = em_Th_node_list,
      "em_Ti" = em_Ti_node_list,
      "em_Tl" = em_Tl_node_list,
      "em_U" = em_U_node_list,
      "em_V" = em_V_node_list,
      "em_W" = em_W_node_list,
      "em_Y" = em_Y_node_list,
      "em_Yb" = em_Yb_node_list,
      "em_Zn" = em_Zn_node_list,
      "em_Zr" = em_Zr_node_list,  ######
      "em_all_ab_elements" = em_all_ab_node_list,
      "em_Ag_ab" = em_Ag_ab_node_list,
      "em_Al_ab" = em_Al_ab_node_list,
      "em_Au_ab" = em_Au_ab_node_list,
      "em_As_ab" = em_As_ab_node_list,
      "em_B_ab" = em_B_ab_node_list,
      "em_Ba_ab" = em_Ba_ab_node_list,
      "em_Be_ab" = em_Be_ab_node_list,
      "em_Bi_ab" = em_Bi_ab_node_list,
      "em_Br_ab" = em_Br_ab_node_list,
      "em_C_ab" = em_C_ab_node_list,
      "em_Ca_ab" = em_Ca_ab_node_list,
      "em_Cd_ab" = em_Cd_ab_node_list,
      "em_Ce_ab" = em_Ce_ab_node_list,
      "em_Cl_ab" = em_Cl_ab_node_list,
      "em_Co_ab" = em_Co_ab_node_list,
      "em_Cu_ab" = em_Cu_ab_node_list,
      "em_Cr_ab" = em_Cr_ab_node_list,
      "em_Cs_ab" = em_Cs_ab_node_list,
      "em_Dy_ab" = em_Dy_ab_node_list,
      "em_Er_ab" = em_Er_ab_node_list,
      "em_F_ab" = em_F_ab_node_list,
      "em_Fe_ab" = em_Fe_ab_node_list,
      "em_Ga_ab" = em_Ga_ab_node_list,
      "em_Gd_ab" = em_Gd_ab_node_list,
      "em_Ge_ab" = em_Ge_ab_node_list,
      "em_H_ab" = em_H_ab_node_list,
      "em_Hg_ab" = em_Hg_ab_node_list,
      "em_Hf_ab" = em_Hf_ab_node_list,
      "em_I_ab" = em_I_ab_node_list,
      "em_In_ab" = em_In_ab_node_list,
      "em_Ir_ab" = em_Ir_ab_node_list,
      "em_K_ab" = em_K_ab_node_list,
      "em_La_ab" = em_La_ab_node_list,
      "em_Li_ab" = em_Li_ab_node_list,
      "em_Mg_ab" = em_Mg_ab_node_list,
      "em_Mn_ab" = em_Mn_ab_node_list,
      "em_Mo_ab" = em_Mo_ab_node_list,
      "em_N_ab" = em_N_ab_node_list,
      "em_Na_ab" = em_Na_ab_node_list,
      "em_Nb_ab" = em_Nb_ab_node_list,
      "em_Nd_ab" = em_Nd_ab_node_list,
      "em_Ni_ab" = em_Ni_ab_node_list,
      "em_O_ab" = em_O_ab_node_list,
      "em_Os_ab" = em_Os_ab_node_list,
      "em_P_ab" = em_P_ab_node_list,
      "em_Pd_ab" = em_Pd_ab_node_list,
      "em_Pr_ab" = em_Pr_ab_node_list,
      "em_Pt_ab" = em_Pt_ab_node_list,
      "em_Rb_ab" = em_Rb_ab_node_list,
      "em_Re_ab" = em_Re_ab_node_list,
      "em_Ru_ab" = em_Ru_ab_node_list,
      "em_Rh_ab" = em_Rh_ab_node_list,
      "em_S_ab" = em_S_ab_node_list,
      "em_Sm_ab" = em_Sm_ab_node_list,
      "em_Sb_ab" = em_Sb_ab_node_list,
      "em_Sc_ab" = em_Sc_ab_node_list,
      "em_Se_ab" = em_Se_ab_node_list,
      "em_Si_ab" = em_Si_ab_node_list,
      "em_Sn_ab" = em_Sn_ab_node_list,
      "em_Sr_ab" = em_Sr_ab_node_list,
      "em_Ta_ab" = em_Ta_ab_node_list,
      "em_Te_ab" = em_Te_ab_node_list,
      "em_Th_ab" = em_Th_ab_node_list,
      "em_Ti_ab" = em_Ti_ab_node_list,
      "em_Tl_ab" = em_Tl_ab_node_list,
      "em_U_ab" = em_U_ab_node_list,
      "em_V_ab" = em_V_ab_node_list,
      "em_W_ab" = em_W_ab_node_list,
      "em_Y_ab" = em_Y_ab_node_list,
      "em_Yb_ab" = em_Yb_ab_node_list,
      "em_Zn_ab" = em_Zn_ab_node_list,
      "em_Zr_ab" = em_Zr_ab_node_list,######
      "loc_Ag" = loc_Ag_node_list,
      "loc_Al" = loc_Al_node_list,
      "loc_As" = loc_As_node_list,
      "loc_Au" = loc_Au_node_list,
      "loc_B" = loc_B_node_list,
      "loc_Ba" = loc_Ba_node_list,
      "loc_Be" = loc_Be_node_list,
      "loc_Bi" = loc_Bi_node_list,
      "loc_Br" = loc_Br_node_list,
      "loc_C" = loc_C_node_list,
      "loc_Ca" = loc_Ca_node_list,
      "loc_Cd" = loc_Cd_node_list,
      "loc_Ce" = loc_Ce_node_list,
      "loc_Cl" = loc_Cl_node_list,
      "loc_Co" = loc_Co_node_list,
      "loc_Cu" = loc_Cu_node_list,
      "loc_Cr" = loc_Cr_node_list,
      "loc_Cs" = loc_Cs_node_list,
      "loc_Dy" = loc_Dy_node_list,
      "loc_Er" = loc_Er_node_list,
      "loc_F" = loc_F_node_list,
      "loc_Fe" = loc_Fe_node_list,
      "loc_Ga" = loc_Ga_node_list,
      "loc_Gd" = loc_Gd_node_list,
      "loc_Ge" = loc_Ge_node_list,
      "loc_H" = em_H_node_list,
      "loc_Hg" = loc_Hg_node_list,
      "loc_Hf" = loc_Hf_node_list,
      "loc_I" = loc_I_node_list,
      "loc_In" = loc_In_node_list,
      "loc_Ir" = loc_Ir_node_list,
      "loc_K" = loc_K_node_list,
      "loc_La" = loc_La_node_list,
      "loc_Li" = loc_Li_node_list,
      "loc_Mg" = loc_Mg_node_list,
      "loc_Mn" = loc_Mn_node_list,
      "loc_Mo" = loc_Mo_node_list,
      "loc_N" = loc_N_node_list,
      "loc_Na" = loc_Na_node_list,
      "loc_Nb" = loc_Nb_node_list,
      "loc_Nd" = loc_Nd_node_list,
      "loc_Ni" = loc_Ni_node_list,
      "loc_O" = em_O_node_list,
      "loc_Os" = loc_Os_node_list,
      "loc_P" =  loc_P_node_list,
      "loc_Pb" = loc_Pb_node_list,
      "loc_Pd" = loc_Pd_node_list,
      "loc_Pt" = loc_Pt_node_list,
      "loc_Rb" = loc_Rb_node_list,
      "loc_Re" = loc_Re_node_list,
      "loc_Rh" = loc_Rh_node_list,
      "loc_Ru" = loc_Ru_node_list,
      "loc_S" =  loc_S_node_list,
      "loc_Sm" = loc_Sm_node_list,
      "loc_Sb" = loc_Sb_node_list,
      "loc_Sc" = loc_Sc_node_list,
      "loc_Se" = loc_Se_node_list,
      "loc_Si" = loc_Si_node_list,
      "loc_Sn" = loc_Sn_node_list,
      "loc_Sr" = loc_Sr_node_list,
      "loc_Ta" = loc_Ta_node_list,
      "loc_Te" = loc_Te_node_list,
      "loc_Th" = loc_Th_node_list,
      "loc_Ti" = loc_Ti_node_list,
      "loc_Tl" = loc_Tl_node_list,
      "loc_U" = loc_U_node_list,
      "loc_V" = loc_V_node_list,
      "loc_W" = loc_W_node_list,
      "loc_Y" = loc_Y_node_list,
      "loc_Yb" = loc_Yb_node_list,
      "loc_Zn" = loc_Zn_node_list,
      "loc_Zr" = loc_Zr_node_list,
      "igneous" = igneous_node_list,
      "o_spinel" = mag_o_spinel_node_list
      
    )
  })
  
  edge_list <- reactive({
    switch(
      input$graph_set,
      "em_all_elements" = em_all_edge_list,
      "em_H" = em_H_edge_list,
      "em_Li" = em_Li_edge_list,
      "em_Be" = em_Be_edge_list,
      "em_B" = em_B_edge_list,
      "em_C" = em_C_edge_list,
      "em_N" = em_N_edge_list,
      "em_O" = em_O_edge_list,
      "em_F" = em_F_edge_list,
      "em_Na" = em_Na_edge_list,
      "em_Mg" = em_Mg_edge_list,
      "em_Al" = em_Al_edge_list,
      "em_Si" = em_Si_edge_list,
      "em_P" = em_P_edge_list,
      "em_S" = em_S_edge_list,
      "em_Cl" = em_Cl_edge_list,
      "em_K" = em_K_edge_list,
      "em_Ca" = em_Ca_edge_list,
      "em_Sc" = em_Sc_edge_list,
      "em_Ti" = em_Ti_edge_list,
      "em_V" = em_V_edge_list,
      "em_Cr" = em_Cr_edge_list,
      "em_Mn" = em_Mn_edge_list,
      "em_Fe" = em_Fe_edge_list,
      "em_Co" = em_Co_edge_list,
      "em_Ni" = em_Ni_edge_list,
      "em_Cu" = em_Cu_edge_list,
      "em_Zn" = em_Zn_edge_list,
      "em_Ga" = em_Ga_edge_list,
      "em_Ge" = em_Ge_edge_list,
      "em_As" = em_As_edge_list,
      "em_Se" = em_Se_edge_list,
      "em_Br" = em_Br_edge_list,
      "em_Rb" = em_Rb_edge_list,
      "em_Sr" = em_Sr_edge_list,
      "em_Y" = em_Y_edge_list,
      "em_Zr" = em_Zr_edge_list,
      "em_Nb" = em_Nb_edge_list,
      "em_Mo" = em_Mo_edge_list,
      "em_Ru" = em_Ru_edge_list,
      "em_Rh" = em_Rh_edge_list,
      "em_Pd" = em_Pd_edge_list,
      "em_Ag" = em_Ag_edge_list,
      "em_Cd" = em_Cd_edge_list,
      "em_In" = em_In_edge_list,
      "em_Sn" = em_Sn_edge_list,
      "em_Sb" = em_Sb_edge_list,
      "em_Te" = em_Te_edge_list,
      "em_I" = em_I_edge_list,
      "em_Cs" = em_Cs_edge_list,
      "em_Ba" = em_Ba_edge_list,
      "em_La" = em_La_edge_list,
      "em_Ce" = em_Ce_edge_list,
      "em_Nd" = em_Nd_edge_list,
      "em_Sm" = em_Sm_edge_list,
      "em_Gd" = em_Gd_edge_list,
      "em_Dy" = em_Dy_edge_list,
      "em_Er" = em_Er_edge_list,
      "em_Yb" = em_Yb_edge_list,
      "em_Hf" = em_Hf_edge_list,
      "em_Ta" = em_Ta_edge_list,
      "em_W" = em_W_edge_list,
      "em_Re" = em_Re_edge_list,
      "em_Os" = em_Os_edge_list,
      "em_Ir" = em_Ir_edge_list,
      "em_Pt" = em_Pt_edge_list,
      "em_Au" = em_Au_edge_list,
      "em_Hg" = em_Hg_edge_list,
      "em_Tl" = em_Tl_edge_list,
      "em_Pb" = em_Pb_edge_list,
      "em_Bi" = em_Bi_edge_list,
      "em_Th" = em_Th_edge_list,
      "em_U" = em_U_edge_list,#####
      "em_all_ab_elements" = em_all_ab_edge_list,
      "em_H_ab" = em_H_ab_edge_list,
      "em_Li_ab" = em_Li_ab_edge_list,
      "em_Be_ab" = em_Be_ab_edge_list,
      "em_B_ab" = em_B_ab_edge_list,
      "em_C_ab" = em_C_ab_edge_list,
      "em_N_ab" = em_N_ab_edge_list,
      "em_O_ab" = em_O_ab_edge_list,
      "em_F_ab" = em_F_ab_edge_list,
      "em_Na_ab" = em_Na_ab_edge_list,
      "em_Mg_ab" = em_Mg_ab_edge_list,
      "em_Al_ab" = em_Al_ab_edge_list,
      "em_Si_ab" = em_Si_ab_edge_list,
      "em_P_ab" = em_P_ab_edge_list,
      "em_S_ab" = em_S_ab_edge_list,
      "em_Cl_ab" = em_Cl_ab_edge_list,
      "em_K_ab" = em_K_ab_edge_list,
      "em_Ca_ab" = em_Ca_ab_edge_list,
      "em_Sc_ab" = em_Sc_ab_edge_list,
      "em_Ti_ab" = em_Ti_ab_edge_list,
      "em_V_ab" = em_V_ab_edge_list,
      "em_Cr_ab" = em_Cr_ab_edge_list,
      "em_Mn_ab" = em_Mn_ab_edge_list,
      "em_Fe_ab" = em_Fe_ab_edge_list,
      "em_Co_ab" = em_Co_ab_edge_list,
      "em_Ni_ab" = em_Ni_ab_edge_list,
      "em_Cu_ab" = em_Cu_ab_edge_list,
      "em_Zn_ab" = em_Zn_ab_edge_list,
      "em_Ga_ab" = em_Ga_ab_edge_list,
      "em_Ge_ab" = em_Ge_ab_edge_list,
      "em_As_ab" = em_As_ab_edge_list,
      "em_Se_ab" = em_Se_ab_edge_list,
      "em_Br_ab" = em_Br_ab_edge_list,
      "em_Rb_ab" = em_Rb_ab_edge_list,
      "em_Sr_ab" = em_Sr_ab_edge_list,
      "em_Y_ab" = em_Y_ab_edge_list,
      "em_Zr_ab" = em_Zr_ab_edge_list,
      "em_Nb_ab" = em_Nb_ab_edge_list,
      "em_Mo_ab" = em_Mo_ab_edge_list,
      "em_Ru_ab" = em_Ru_ab_edge_list,
      "em_Rh_ab" = em_Rh_ab_edge_list,
      "em_Pd_ab" = em_Pd_ab_edge_list,
      "em_Ag_ab" = em_Ag_ab_edge_list,
      "em_Cd_ab" = em_Cd_ab_edge_list,
      "em_In_ab" = em_In_ab_edge_list,
      "em_Sn_ab" = em_Sn_ab_edge_list,
      "em_Sb_ab" = em_Sb_ab_edge_list,
      "em_Te_ab" = em_Te_ab_edge_list,
      "em_I_ab" = em_I_ab_edge_list,
      "em_Cs_ab" = em_Cs_ab_edge_list,
      "em_Ba_ab" = em_Ba_ab_edge_list,
      "em_La_ab" = em_La_ab_edge_list,
      "em_Ce_ab" = em_Ce_ab_edge_list,
      "em_Nd_ab" = em_Nd_ab_edge_list,
      "em_Sm_ab" = em_Sm_ab_edge_list,
      "em_Gd_ab" = em_Gd_ab_edge_list,
      "em_Dy_ab" = em_Dy_ab_edge_list,
      "em_Er_ab" = em_Er_ab_edge_list,
      "em_Yb_ab" = em_Yb_ab_edge_list,
      "em_Hf_ab" = em_Hf_ab_edge_list,
      "em_Ta_ab" = em_Ta_ab_edge_list,
      "em_W_ab" = em_W_ab_edge_list,
      "em_Re_ab" = em_Re_ab_edge_list,
      "em_Os_ab" = em_Os_ab_edge_list,
      "em_Ir_ab" = em_Ir_ab_edge_list,
      "em_Pt_ab" = em_Pt_ab_edge_list,
      "em_Au_ab" = em_Au_ab_edge_list,
      "em_Hg_ab" = em_Hg_ab_edge_list,
      "em_Tl_ab" = em_Tl_ab_edge_list,
      "em_Pb_ab" = em_Pb_ab_edge_list,
      "em_Bi_ab" = em_Bi_ab_edge_list,
      "em_Th_ab" = em_Th_ab_edge_list,
      "em_U_ab" = em_U_ab_edge_list,#####
      "loc_H" = em_H_edge_list,
      "loc_Li" = loc_Li_edge_list,
      "loc_Be" = loc_Be_edge_list,
      "loc_B" = loc_B_edge_list,
      "loc_C" = loc_C_edge_list,
      "loc_N" = loc_N_edge_list,
      "loc_O" = em_O_edge_list,
      "loc_F" = loc_F_edge_list,
      "loc_Na" = loc_Na_edge_list,
      "loc_Mg" = loc_Mg_edge_list,
      "loc_Al" = loc_Al_edge_list,
      "loc_Si" = loc_Si_edge_list,
      "loc_P" =  loc_P_edge_list,
      "loc_S" = loc_S_edge_list,
      "loc_Cl" = loc_Cl_edge_list,
      "loc_K" = loc_K_edge_list,
      "loc_Ca" = loc_Ca_edge_list,
      "loc_Sc" = loc_Sc_edge_list,
      "loc_Ti" = loc_Ti_edge_list,
      "loc_V" = loc_V_edge_list,
      "loc_Cr" = loc_Cr_edge_list,
      "loc_Mn" = loc_Mn_edge_list,
      "loc_Fe" = loc_Fe_edge_list,
      "loc_Co" = loc_Co_edge_list,
      "loc_Ni" = loc_Ni_edge_list,
      "loc_Cu" = loc_Cu_edge_list,
      "loc_Zn" = loc_Zn_edge_list,
      "loc_Ga" = loc_Ga_edge_list,
      "loc_Ge" = loc_Ge_edge_list,
      "loc_As" = loc_As_edge_list,
      "loc_Se" = loc_Se_edge_list,
      "loc_Br" = loc_Br_edge_list,
      "loc_Rb" = loc_Rb_edge_list,
      "loc_Sr" = loc_Sr_edge_list,
      "loc_Y" = loc_Y_edge_list,
      "loc_Zr" = loc_Zr_edge_list,
      "loc_Nb" = loc_Nb_edge_list,
      "loc_Mo" = loc_Mo_edge_list,
      "loc_Ru" = loc_Ru_edge_list,
      "loc_Rh" = loc_Rh_edge_list,
      "loc_Pd" = loc_Pd_edge_list,
      "loc_Ag" = loc_Ag_edge_list,
      "loc_Cd" = loc_Cd_edge_list,
      "loc_In" = loc_In_edge_list,
      "loc_Sn" = loc_Sn_edge_list,
      "loc_Sb" = loc_Sb_edge_list,
      "loc_Te" = loc_Te_edge_list,
      "loc_I" = loc_I_edge_list,
      "loc_Cs" = loc_Cs_edge_list,
      "loc_Ba" = loc_Ba_edge_list,
      "loc_La" = loc_La_edge_list,
      "loc_Ce" = loc_Ce_edge_list,
      "loc_Nd" = loc_Nd_edge_list,
      "loc_Sm" = loc_Sm_edge_list,
      "loc_Gd" = loc_Gd_edge_list,
      "loc_Dy" = loc_Dy_edge_list,
      "loc_Er" = loc_Er_edge_list,
      "loc_Yb" = loc_Yb_edge_list,
      "loc_Hf" = loc_Hf_edge_list,
      "loc_Ta" = loc_Ta_edge_list,
      "loc_W" = loc_W_edge_list,
      "loc_Re" = loc_Re_edge_list,
      "loc_Os" = loc_Os_edge_list,
      "loc_Ir" = loc_Ir_edge_list,
      "loc_Pt" = loc_Pt_edge_list,
      "loc_Au" = loc_Au_edge_list,
      "loc_Hg" = loc_Hg_edge_list,
      "loc_Tl" = loc_Tl_edge_list,
      "loc_Pb" = loc_Pb_edge_list,
      "loc_Bi" = loc_Bi_edge_list,
      "loc_Th" = loc_Th_edge_list,
      "loc_U" =  loc_U_edge_list,
      "igneous" = igneous_edge_list,
      "o_spinel" = mag_o_spinel_edge_list
      
    )
  })

  # output$attribution <- renderUI({
  #   switch(
  #     input$graph_set,
  #     "goltzius" = includeMarkdown("data/citations/goltzius.md"),
  #     "les_mis" = includeMarkdown("data/citations/les_mis.md"),
  #     "karate" = includeMarkdown("data/citations/karate.md"),
  #     "polbooks" = includeMarkdown("data/citations/polbooks.md"),
  #     "copperfield" = includeMarkdown("data/citations/copperfield.md"),
  #     "football" = includeMarkdown("data/citations/football.md")
  #     )
  # })

  # Generate a selection menu for ordering choices
  output$ordering_choices <- renderUI({
    base <- c(
      "name",
      "community"
    )
    dataset_names <- names(node_list())
    var_choices <- dataset_names[grep("comm", dataset_names, invert = TRUE)] %>% union(base)
    return(selectInput(
      "arr_var",
      "Arrange by",
      choices = var_choices,
      selected = "community"
    ))
  })
  
  # Generate a selection menu for ordering choices
  output$ordering_choices_elm <- renderUI({
    base <- c(
      "name",
      "community"
    )
    dataset_names <- names(node_list())
    var_choices <- dataset_names[grep("comm", dataset_names, invert = TRUE)] %>% union(base)
    return(selectInput(
      "arr_var_elm",
      "Arrange by",
      choices = var_choices,
      selected = "community"
    ))
  })
  

  # Generate a selection menu for community detection choices
  output$comm_choices <- renderUI({
    dataset_names <- names(node_list())
    comm_choices <- dataset_names[grep("comm", dataset_names)]

    return(selectInput(
      "comm_var",
      "Community Detection Algorithm",
      choices = comm_choices,
      selected = "optimal_comm"
    ))
  })
  
  weighted <- reactive({
    return("weight" %in% names(edge_list()))
  })
  output$weighted <- reactive({weighted()})
  outputOptions(output, 'weighted', suspendWhenHidden = FALSE)

  # List non-calculated node attributes
  annotate_vars <- reactive({
    dataset_names <- names(node_list())

    base <- c(
      "name",
      "degree",
      "closeness",
      "betweenness",
      "eigen",
      "walktrap_comm",
      "edge_comm",
      "optimal_comm",
      "spinglass_comm",
      "fastgreedy_comm",
      "multilevel_comm"
      )

    return(setdiff(dataset_names, base))

  })

  annotatable <- reactive({
    return(input$arr_var %in% annotate_vars())
  })
  output$annotate_vars <- reactive({annotatable()})
  outputOptions(output, "annotate_vars", suspendWhenHidden = FALSE)

  # Returns a character vector of the vertices ordered based on given variables
  ordering <- reactive({
    if(input$arr_var == "community") {
      return((node_list() %>% arrange_(input$comm_var))$name)
    } else {
      return((node_list() %>% arrange_(input$arr_var))$name)
    }
  })
  
  # Determine a community for each edge. If two nodes belong to the
  # same community, label the edge with that community. If not,
  # the edge community value is 'NA'
  coloring <- reactive({
    colored_edges <- edge_list() %>%
      inner_join(node_list() %>% select_("name", "community" = input$comm_var), by = c("from" = "name")) %>%
      inner_join(node_list() %>% select_("name", "community" = input$comm_var), by = c("to" = "name")) %>%
      mutate(group = ifelse(community.x == community.y, community.x, NA) %>% factor())
    return(colored_edges)
  })

  # Sort the edge list based on the given arrangement variable
  plot_data <- reactive({
    name_order <- ordering()
    sorted_data <- coloring() %>% mutate(
      to = factor(to, levels = name_order),
      from = factor(from, levels = name_order))
    return(sorted_data)
  })

  
  ###############################################
  observeEvent(input$elementNmae , {
    print(input$elementNmae )
    node_elm_list <- reactive(
      {switch(
        input$elementNmae,
        "H" =  em_H_node_list,
        "Li" = em_Li_node_list,
        "Be" = em_Be_node_list,
        "B" = em_B_node_list,
        "C" = em_C_node_list,
        "N" = em_N_node_list,
        "O" = em_O_node_list,
        "F" = em_F_node_list,
        "Na" = em_Na_node_list,
        "Mg" = em_Mg_node_list,
        "Al" = em_Al_node_list,
        "Si" = em_Si_node_list,
        "P" = em_P_node_list,
        "S" = em_S_node_list,
        "Cl" = em_Cl_node_list,
        "K" = em_K_node_list,
        "Ca" = em_Ca_node_list,
        "Sc" = em_Sc_node_list,
        "Ti" = em_Ti_node_list,
        "V" = em_V_node_list,
        "Cr" = em_Cr_node_list,
        "Mn" = em_Mn_node_list,
        "Fe" = em_Fe_node_list,
        "Co" = em_Co_node_list,
        "Ni" = em_Ni_node_list,
        "Cu" = em_Cu_node_list,
        "Zn" = em_Zn_node_list,
        "Ga" = em_Ga_node_list,
        "Ge" = em_Ge_node_list,
        "As" = em_As_node_list,
        "Se" = em_Se_node_list,
        "Br" = em_Br_node_list,
        "Rb" = em_Rb_node_list,
        "Sr" = em_Sr_node_list,
        "Y" = em_Y_node_list,
        "Zr" = em_Zr_node_list,
        "Nb" = em_Nb_node_list,
        "Mo" = em_Mo_node_list,
        "Ru" = em_Ru_node_list,
        "Rh" = em_Rh_node_list,
        "Pd" = em_Pd_node_list,
        "Ag" = em_Ag_node_list,
        "Cd" = em_Cd_node_list,
        "In" = em_In_node_list,
        "Sn" = em_Sn_node_list,
        "Sb" = em_Sb_node_list,
        "Te" = em_Te_node_list,
        "I" = em_I_node_list,
        "Cs" = em_Cs_node_list,
        "Ba" = em_Ba_node_list,
        "La" = em_La_node_list,
        "Ce" = em_Ce_node_list,
        "Pr" = em_Pr_node_list,
        "Nd" = em_Nd_node_list,
        "Sm" = em_Sm_node_list,
        "Gd" = em_Gd_node_list,
        "Dy" = em_Dy_node_list,
        "Er" = em_Er_node_list,
        "Yb" = em_Yb_node_list,
        "Hf" = em_Hf_node_list,
        "Ta" = em_Ta_node_list,
        "W" = em_W_node_list,
        "Re" = em_Re_node_list,
        "Os" = em_Os_node_list,
        "Ir" = em_Ir_node_list,
        "Pt" = em_Pt_node_list,
        "Au" = em_Au_node_list,
        "Hg" = em_Hg_node_list,
        "Tl" = em_Tl_node_list,
        "Pb" = em_Pb_node_list,
        "Bi" = em_Bi_node_list,
        "Th" = em_Th_node_list,
        "U" = em_U_node_list
      )}
      )
    edge_elm_list <- reactive({switch(
        input$elementNmae,
        "H" =  em_H_edge_list,
        "Li" = em_Li_edge_list,
        "Be" = em_Be_edge_list,
        "B" = em_B_edge_list,
        "C" = em_C_edge_list,
        "N" = em_N_edge_list,
        "O" = em_O_edge_list,
        "F" = em_F_edge_list,
        "Na" = em_Na_edge_list,
        "Mg" = em_Mg_edge_list,
        "Al" = em_Al_edge_list,
        "Si" = em_Si_edge_list,
        "P" = em_P_edge_list,
        "S" = em_S_node_list,
        "Cl" = em_Cl_edge_list,
        "K" = em_K_edge_list,
        "Ca" = em_Ca_edge_list,
        "Sc" = em_Sc_edge_list,
        "Ti" = em_Ti_edge_list,
        "V" = em_V_edge_list,
        "Cr" = em_Cr_edge_list,
        "Mn" = em_Mn_edge_list,
        "Fe" = em_Fe_edge_list,
        "Co" = em_Co_edge_list,
        "Ni" = em_Ni_edge_list,
        "Cu" = em_Cu_edge_list,
        "Zn" = em_Zn_edge_list,
        "Ga" = em_Ga_edge_list,
        "Ge" = em_Ge_edge_list,
        "As" = em_As_edge_list,
        "Se" = em_Se_edge_list,
        "Br" = em_Br_edge_list,
        "Rb" = em_Rb_edge_list,
        "Sr" = em_Sr_edge_list,
        "Y" = em_Y_edge_list,
        "Zr" = em_Zr_edge_list,
        "Nb" = em_Nb_edge_list,
        "Mo" = em_Mo_edge_list,
        "Ru" = em_Ru_edge_list,
        "Rh" = em_Rh_edge_list,
        "Pd" = em_Pd_edge_list,
        "Ag" = em_Ag_edge_list,
        "Cd" = em_Cd_edge_list,
        "In" = em_In_edge_list,
        "Sn" = em_Sn_edge_list,
        "Sb" = em_Sb_edge_list,
        "Te" = em_Te_edge_list,
        "I" = em_I_edge_list,
        "Cs" = em_Cs_edge_list,
        "Ba" = em_Ba_edge_list,
        "La" = em_La_edge_list,
        "Ce" = em_Ce_edge_list,
        "Pr" = em_Pr_edge_list,
        "Nd" = em_Nd_edge_list,
        "Sm" = em_Sm_edge_list,
        "Gd" = em_Gd_edge_list,
        "Dy" = em_Dy_edge_list,
        "Er" = em_Er_edge_list,
        "Yb" = em_Yb_edge_list,
        "Hf" = em_Hf_edge_list,
        "Ta" = em_Ta_edge_list,
        "W" = em_W_edge_list,
        "Re" = em_Re_edge_list,
        "Os" = em_Os_edge_list,
        "Ir" = em_Ir_edge_list,
        "Pt" = em_Pt_edge_list,
        "Au" = em_Au_edge_list,
        "Hg" = em_Hg_edge_list,
        "Tl" = em_Tl_edge_list,
        "Pb" = em_Pb_edge_list,
        "Bi" = em_Bi_edge_list,
        "Th" = em_Th_edge_list,
        "U" = em_U_edge_list
      )})
    
    weighted_elm <- reactive({
      return("weight" %in% names(edge_elm_list()))
    })
    output$weighted_elm <- reactive({weighted_elm()})
    outputOptions(output, 'weighted_elm', suspendWhenHidden = FALSE)
    
    #####下面这段代码不需要
    output$comm_choices_elm <- renderUI({
      dataset_names <- names(node_elm_list())
      comm_choices_elm <- dataset_names[grep("comm", dataset_names)]
      return(selectInput(
        "comm_var_elm",
        "Community Algorithm",
        choices = comm_choices_elm,
        selected = "optimal_comm"
      ))
    })

    # List non-calculated node attributes
    annotate_vars_elm <- reactive({
      
      dataset_names <- names(node_elm_list())
      
      base <- c(
        "name",
        "degree",
        "closeness",
        "betweenness",
        "eigen",
        "walktrap_comm",
        "edge_comm",
        "optimal_comm",
        "spinglass_comm",
        "fastgreedy_comm",
        "multilevel_comm"
      )
      return(setdiff(dataset_names, base))
      
    })
    
    annotatable_elm <- reactive({
      return(input$arr_var_elm %in% annotate_vars_elm())
    })
    output$annotate_vars_elm <- reactive({annotatable_elm()})
    outputOptions(output, "annotate_vars_elm", suspendWhenHidden = FALSE)
    
    
    # Returns a character vector of the vertices ordered based on given variables
    ordering_elm <- reactive({
      if(input$arr_var_elm == "community") {
        return((node_elm_list() %>% arrange_(input$comm_var_elm))$name)
      } else {
        return((node_elm_list() %>% arrange_(input$arr_var_elm))$name)
      }
    })
    
    coloring_elm <- reactive({
      colored_edges <- edge_elm_list() %>%
        inner_join(node_elm_list() %>% select_("name", "community" = input$comm_var_elm), by = c("from" = "name")) %>%
        inner_join(node_elm_list() %>% select_("name", "community" = input$comm_var_elm), by = c("to" = "name")) %>%
        mutate(group = ifelse(community.x == community.y, community.x, NA) %>% factor())
      return(colored_edges)
    })
    
    plot_data_elm <- reactive({
      name_order <- ordering_elm()
      sorted_data <- coloring_elm() %>% mutate(
        to = factor(to, levels = name_order),
        from = factor(from, levels = name_order))
      return(sorted_data)
    })
    
    #Render ptable_plot
    output$ptable_plot <- renderPlot({
      
      if(weighted_elm() & input$alpha_weight_elm) {
        p <- ggplot(plot_data_elm(), aes(x = from, y = to, fill = group, alpha = weight))
      } else {
        p <- ggplot(plot_data_elm(), aes(x = from, y = to, fill = group))
      }
      adj_size = 12*72/nlevels(p$data$from)
      if (adj_size <2){
        adj_size = 2
      }
      if (adj_size>14){
        adj_size =14
      }
      
      
      p <- p + geom_raster() +
        theme_bw() +
        # Because we need the x and y axis to display every node,
        # not just the nodes that have connections to each other,
        # make sure that ggplot does not drop unused factor levels
        scale_x_discrete(drop = FALSE) +
        scale_y_discrete(drop = FALSE) +
        theme(
          # Rotate the x-axis lables so they are legible
          #axis.text.x = element_text(angle = 270, hjust = 0, size = 12),
          #axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 270, hjust = 0, size = adj_size),
          axis.text.y = element_text(size = adj_size),
          # Force the plot into a square aspect ratio
          aspect.ratio = 1,
          # Hide the legend (optional)
          legend.position = "bottom")
      
      # Annotate the plot based on preexisting node attributes
      if(annotatable_elm() & input$ann_var_elm) {
        
        # Determine the "first" and "last" members of a node group
        ordered_anns <- node_elm_list() %>%
          group_by_(input$arr_var_elm) %>%
          summarize(min = first(name), max = last(name)) %>%
          filter(min != max)
        
        ann_groups <- ordered_anns[[input$arr_var_elm]]
        
        # For each node grouping, add an annotation layer
        for(val in ann_groups[!is.na(ann_groups)]) {
          
          # Retrieve the min and max value for the given group value
          ann_min <- ordered_anns[ordered_anns[, input$arr_var_elm] == val, ][["min"]]
          ann_max <- ordered_anns[ordered_anns[, input$arr_var_elm] == val, ][["max"]]
          
          p <- p + annotate(
            "rect",
            xmin = ann_min,
            xmax = ann_max,
            ymin = ann_min,
            ymax = ann_max,
            alpha = .1) +
            annotate(
              "text",
              label = val,
              x = ann_min,
              y = ann_max,
              hjust = 0
            )
        }
      }
      
      return(p)
    })
    
    
    
    
  })
  

  
  
  
  
  output$adj_plot <- renderPlot({

    if(weighted() & input$alpha_weight) {
      p <- ggplot(plot_data(), aes(x = from, y = to, fill = group, alpha = weight))
    } else {
      p <- ggplot(plot_data(), aes(x = from, y = to, fill = group))
    }
    adj_size = 12*72/nlevels(p$data$from)
    if (adj_size <2){
      adj_size = 2
    }
    if (adj_size>14){
      adj_size =14
    }
      
    
    p <- p + geom_raster() +
      theme_bw() +
      # Because we need the x and y axis to display every node,
      # not just the nodes that have connections to each other,
      # make sure that ggplot does not drop unused factor levels
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme(
        # Rotate the x-axis lables so they are legible
        #axis.text.x = element_text(angle = 270, hjust = 0, size = 12),
        #axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270, hjust = 0, size = adj_size),
        axis.text.y = element_text(size = adj_size),
        # Force the plot into a square aspect ratio
        aspect.ratio = 1,
        # Hide the legend (optional)
        legend.position = "bottom")

    # Annotate the plot based on preexisting node attributes
    if(annotatable() & input$ann_var) {

      # Determine the "first" and "last" members of a node group
      ordered_anns <- node_list() %>%
        group_by_(input$arr_var) %>%
        summarize(min = first(name), max = last(name)) %>%
        filter(min != max)

      ann_groups <- ordered_anns[[input$arr_var]]

      # For each node grouping, add an annotation layer
      for(val in ann_groups[!is.na(ann_groups)]) {

        # Retrieve the min and max value for the given group value
        ann_min <- ordered_anns[ordered_anns[, input$arr_var] == val, ][["min"]]
        ann_max <- ordered_anns[ordered_anns[, input$arr_var] == val, ][["max"]]

        p <- p + annotate(
          "rect",
          xmin = ann_min,
          xmax = ann_max,
          ymin = ann_min,
          ymax = ann_max,
          alpha = .1) +
          annotate(
            "text",
            label = val,
            x = ann_min,
            y = ann_max,
            hjust = 0
          )
      }
    }

    return(p)
  })
  

  comm_membership <- reactive({
    membership_list <- node_list() %>%
      select_("name", "community" = input$comm_var)
    return(membership_list)
  })

  # Render an HTML list of community memberships beneath the display
  output$membership_list <- renderUI({
    members <- comm_membership()
    comms <- unique(members$community)
    member_html <- list()
    for(i in comms) {
      group_membs <- members$name[members$community == i]
      member_html[[i]] <- list(box(title = paste("Group", i), width = 3, status = "info", p(group_membs)))
    }
    return(member_html)
  })
  
  #shinyjs::runjs("initJS();")#"document.body.addEventListener('click', function() {alert('hello')});")  
  
})
