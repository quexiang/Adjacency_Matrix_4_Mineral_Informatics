""" 
Data Extraction: quantity of all the ore-forming elements. 

@author: xiangque
"""
from bs4 import BeautifulSoup
import requests
import csv
import pandas as pd
import numpy as np
#url = 'https://rruff.info/mineral_list/MED/minerals_per_locality.php?element=N'
url = 'element_match.php_elm.html'
fp = open(url,'r',encoding='utf-8')
soup = BeautifulSoup(fp, 'html.parser')
all_str = soup.prettify()
    


element_list = ['','H', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Na', 'Mg', 'Al', 'Si', 'P', 'S',
                'Cl', 'K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge',
                'As', 'Se', 'Br', 'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd',
                'In', 'Sn', 'Sb', 'Te', 'I', 'Cs', 'Ba', 'La', 'Ce', 'Nd', 'Sm', 'Gd', 'Dy', 'Er',
                'Yb', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Th', 'U']
       
netflix_data = pd.DataFrame(columns=element_list)


for row in soup.find("tbody").find_all('tr'):
    col = row.find_all("td")
    c_len = len(element_list)
    c_tmp_list = []
    for c_i in range(c_len):
        c_tmp_list.append(col[c_i].text)
    netflix_data.loc[len(netflix_data.index)] = np.asarray(c_tmp_list)

netflix_data = netflix_data.drop([0])        
netflix_data.head()
netflix_data.to_csv("Elements_all_adjacents.csv",index=False)



