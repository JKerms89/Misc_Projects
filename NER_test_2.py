import spacy
from spacy import displacy
import os
import pandas as pd
import IPython
from collections import Counter
import seaborn as sns
import matplotlib.pyplot as plt
import seaborn as sns

# Change directory

os.chdir(r"C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/NER")

# load spacy

nlp = spacy.load("en_core_web_sm")
nlp.max_length = 1500000 #or any large value, as long as you don't run out of RAM

# Upload dataset sample (100 political claims)

df = pd.read_excel('df_ner.xlsx', usecols=['text'], nrows=100)
df['Entities'] = df['text'].apply(lambda sent: [(ent.text, ent.label_) for ent in nlp(sent).ents 
if ent.label_ =='NORP' or ent.label_ == 'GPE'])  
print(df)

# Export df to R / excel




