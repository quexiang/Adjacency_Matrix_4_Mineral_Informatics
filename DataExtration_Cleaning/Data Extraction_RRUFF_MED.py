"""
Data Extraction: Get all the Element_localities_based minerals list from the RRUFF 

@author: xiangque
"""


from bs4 import BeautifulSoup
import requests
import csv
import pandas as pd
#url = 'https://rruff.info/mineral_list/MED/minerals_per_locality.php?element=N'
#elmList

elmlist =['H','Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Na', 'Mg', 'Al','Si', 'P', 'S','Cl', 
 'K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge','As',
    'Se', 'Br', 'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Ru', 
    'Rh', 'Pd', 'Ag', 'Cd','In', 'Sn', 'Sb', 'Te', 'I', 
    'Cs', 'Ba', 'La', 'Ce', 'Nd', 'Sm', 'Gd', 'Dy', 'Er',
    'Yb', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg','Tl', 'Pb', 'Bi', 'Th', 'U']

name_cur  = "./minerals_per_locality_"       
for elm in elmlist:
    cur_url =  name_cur + elm +".html"   
    #url = 'minerals_per_locality_B.htm'
    fp = open(cur_url,'r',encoding='utf-8')
    soup = BeautifulSoup(fp, 'html.parser')
    all_str = soup.prettify()
    netflix_data = pd.DataFrame(columns=["mindat_ID", "Locality_Name", "Minerals_count", "Minerals_list"])
    
    for row in soup.find("tbody").find_all('tr'):
        col = row.find_all("td")
        if (len(col) != 0):
            mditid = col[0].text
            Local = col[1].text
            mineralcnt = col[2].text
            mineral_lst = col[3].text
            netflix_data = netflix_data.append({"mindat_ID":mditid, "Locality_Name":Local, "Minerals_count":mineralcnt, "Minerals_list":mineral_lst}, ignore_index=True)  
    netflix_data.head()
    outfile_name = "Element_localities_"+elm +".csv"
    netflix_data.to_csv(outfile_name)
    
    
    
