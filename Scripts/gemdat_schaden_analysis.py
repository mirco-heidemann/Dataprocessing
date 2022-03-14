#!/usr/bin/env python3
#-*- coding: ISO-8859-1 -*-
#@Filename : gemdat_schaden_analysis
#@Date : 2018-07-05-17-26
#@Poject: Data_Analysis
#@AUTHOR : Mirco Heidemann

import os as os
import pandas as pd

# get current working directory
os.getcwd()

# read the CSV into a pandas data frame
df = pd.read_csv("data/A_Schadendatenper31.05.2018_9639.csv", delimiter=';', encoding = "ISO-8859-1")

# print first five rows, analog to R's head(df)
df.head()
# pandas equivalents for R functions str()
df.info()

# rename columns: 'alter_name': 'neuer_name'
df.rename(columns={'GebaeudeId': 'geb_id',
                   'GebaeudeSuchbegriff': 'geb_nr',
                   'SchadenVersicherungWert': 'vers_sum',
                   'GebaeudeBaujahr': 'baujahr',
                   'GebaeudeGemeindeName': 'gemeinde',
                   'Ausdr2': 'gemeinde_id',
                   'GebaeudeZweckCode': 'zwk_code',
                   'GebaeudeZweckText': 'zwk_code_text',
                   'SchadenId': 'schaden_id',
                   'SchadenNr': 'schaden_nr',
                   'SchadenDatum': 'schaden_datum',
                   'SchadenSumme': 'schaden_sum',
                   'SchadenArtBezeichnung': 'schaden_art',
                   'CodTextDt': 'schaden_code',
                   'StcTextDt': 'status'},
          inplace = True) # rename the existing DataFrame (rather than creating a copy)

# converting schaden_datum to date format
df['schaden_datum'] = pd.to_datetime(df.schaden_datum)

# # selecting columns
# df_selected = df[['geb_id', 'schaden_id', 'schaden_datum']]

# dropping columns
df_new = df.drop(columns=['Erstellung', 'Ausdr1', 'GebaeudeStrasse', 'GebaeudeHausNr', 'GebaeudePlz',
                          'GebaeudeOrt', 'Ausdr3', 'GebaeudeVolumen', 'EdiStcDis', 'EdiAfDat'])

df_new.info()

# write the new csv
# df_selected.to_csv('schad_example_selected.csv', index = False, encoding = "ISO-8859-1") # encoding = "utf-8"