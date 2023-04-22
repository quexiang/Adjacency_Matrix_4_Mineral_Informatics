"""
Use Algorithm 1: Generate the Nodes & edges list for ore-forming elements.

@author: xiangque
"""

import numpy as np
import pandas as pd
import csv
import os

elementcsvfile = "elems_abum.csv"
csvFile_abum = open(elementcsvfile, "r",encoding='utf-8')
df_elm_abum = pd.read_csv(csvFile_abum,header = 0)

df_elm_abum = df_elm_abum[df_elm_abum["mineral_elements"] ==1]
element_list = df_elm_abum["Element"].tolist()
pct_elm_list = df_elm_abum["percentage_value"].tolist()

file_elm_list = ['H', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Na', 'Mg', 'Al', 'Si', 'P', 'S',
                    'Cl', 'K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge',
                    'As', 'Se', 'Br', 'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd',
                    'In', 'Sn', 'Sb', 'Te', 'I', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Sm', 'Gd', 'Dy', 'Er',
                    'Yb', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Th', 'U']

file_in_path = "./73elmcsv/"
file_out_path = "./73elmcsv/73out_elm/"
if os.path.exists(file_out_path) == False:
        os.makedirs(file_out_path) 

idx_abum = 0        
for iem in element_list:
    cur_in_file = file_in_path +iem +".csv" 
    csvFile = open(cur_in_file, "r",encoding='utf-8')
    df = pd.read_csv(csvFile,header = 0)
    df = df.drop(columns = ["zaxis","yaxis"])
    df.info()
    lst_nodes = list(df.columns)

    all_data  = df.values
    
    list_from = []
    list_to = []
    list_weihgt = []
    
    idx_row = 0
    for row in range(all_data.shape[0]):
        idx_col = 0
        for col in range(all_data.shape[1]):

                 if (0!=all_data[row,col]):
                     list_weihgt.append(all_data[row,col])
                     # consider the element abum
                     list_from.append(lst_nodes[row])
                     list_to.append(lst_nodes[col])
                 idx_col +=1
                
        idx_row += 1
            
    temp_list_to =  list_to[:] 
    temp_list_from =  list_from[:]  
       
    list_from.extend(temp_list_to)
    list_to.extend(temp_list_from)
    list_weihgt.extend(list_weihgt)
    
    out_fname_edges = file_out_path +iem+ "_edges.csv"
     
    #output edges
    df_output_edges = pd.DataFrame({"from":list_from,"to":list_to,"weight":list_weihgt})
    df_output_edges.to_csv(out_fname_edges,index=False) 
    
    #output nodes 
    array_elementss = np.array(lst_nodes).reshape((-1,1))
    df_output_nodes = pd.DataFrame(array_elementss, columns = ["Names"] )  
    out_fname_nodes  = file_out_path +iem+ "_nodes.csv"
   
    df_output_nodes.to_csv(out_fname_nodes,index=False) 
    idx_abum += 1





