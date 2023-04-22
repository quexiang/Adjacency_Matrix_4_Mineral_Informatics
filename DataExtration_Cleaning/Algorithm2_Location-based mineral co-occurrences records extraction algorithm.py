#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Algorithm 2: Location-based mineral co-occurrences records extraction algorithm
Created on Mon Jan 23 11:36:53 2023

@author: xiangque
"""

import numpy as np
import pandas as pd
import csv
import shlex

import re


def replacenth(string, sub, wanted, n):
    where = [m.start() for m in re.finditer(sub, string)][n-1]
    before = string[:where]
    after = string[where:]
    after = after.replace(sub, wanted, 1)
    newString = before + after
    return newString    

File_name = "RRUFF_Export_20230131_115814.csv"
csvFile = open(File_name, "r",encoding='utf-8')
df = pd.read_csv(csvFile,header=0)
df = df.loc[:,["Mineral Name"]]
mag_o_spinel_list = df.values.tolist()
newl_mo_spinel_list = [x[0] for x in mag_o_spinel_list]


#1.read csv file get the localitity of O
f_name = "./2019_05_22/MED_export.csv"
f_data = open(f_name,"r")
lines = f_data.readlines()

# sm_list = lines[80000:100000]


str_name_list = ["mindat_ids_list","MED_id_list","bottom_level_list","mindat_url_list","LSN_list","LLN_list","gps_list","gps_source_list",
                 "MED_elements_all_list","MED_minerals_all_list","MED_elements_at_loc_list","MED_mineral_at_loc_list"]
mindat_ids_list = []
MED_id_list = []
bottom_level_list = []
mindat_url_list = []
LSN_list = []
LLN_list = []
gps_list = []
gps_source_list = []
MED_elements_all_list =[]
MED_minerals_all_list = []
MED_elements_at_loc_list =[]
MED_mineral_at_loc_list=[]

mo_spinel_loc_atlist =[]
mindat_out_idlist = []
loc_out_names =[]
minerals_counts_list = []


for line in lines:
    #record = line.split()
    cnt = line.count('\"')
    if cnt> 4:
        sep = int(cnt/2)
        if (sep & 1 !=0):
            sep = sep-1   
        sub = '\"'
        wanted = '\''
        
        rp_ridx = sep+2
        n_repl = cnt- rp_ridx
        while n_repl >0:
            line = replacenth(line,sub,wanted,rp_ridx)
            n_repl -=1
            
        rp_lidx = 2
        n_lrepl = sep - 2
        while n_lrepl >0:
            line = replacenth(line,sub,wanted,rp_lidx)
            n_lrepl -=1
            
    
    record = shlex.split(line) 
    
    if record[2] ==0:
        continue
    idx = 0
    for f_val in record:
        eval(str_name_list[idx]).append(f_val)
        idx += 1
    if idx > 9:
        cur_minerals = [x.strip() for x in MED_minerals_all_list[-1].split(',') if not x.strip() == '']  
        c_minerals_list =[]
        cnt = 0
        for c_mineral in cur_minerals:
            if c_mineral in newl_mo_spinel_list:
                  c_minerals_list.append(c_mineral)
                  cnt +=1
        if len(c_minerals_list) != 0:          
            c_minerals_str = ','.join(c_minerals_list)
            mindat_out_idlist.append(record[0])
            mo_spinel_loc_atlist.append(c_minerals_str)
            loc_out_names.append(record[5])
            minerals_counts_list.append(cnt)
        
    while idx < 12:
        eval(str_name_list[idx]).append(np.nan)
        idx+=1

csv_o = {'mindat_ID':mindat_out_idlist,'location name':loc_out_names,'count':minerals_counts_list,'mo_spinel_minerals':mo_spinel_loc_atlist}
df = pd.DataFrame(csv_o)

df.to_csv("mo_spine_minerals.csv", index=False)

   