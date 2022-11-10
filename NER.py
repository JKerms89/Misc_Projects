import spacy
from spacy import displacy
import os
import pandas as pd
import IPython
from collections import Counter
import seaborn as sns
import matplotlib.pyplot as plt

# Change directory

os.chdir(r"C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/NER")

# load spacy

NER = spacy.load("en_core_web_sm")
NER.max_length = 10000000 #or any large value, as long as you don't run out of RAM

# Upload dataset sample (100 political claims)

df = pd.read_excel('df_ner_facet.xlsx', sheet_name = 'DE', usecols=['text'])
print(df)
raw_text = df.to_string()


# Examine char length of dataset


print(len(raw_text))

# Apply NER to raw data

text = NER(raw_text)

# Examine distribution of entity types

for word in text.ents:
    print(word.text,word.label_)

# Filter by entity type

for word in text.ents:
    if word.label_ in ["GPE"]:
        print(word.text, word.label)


# Count no. of GPE and NORP entities and sort by value

ents = Counter()

for word in text.ents:
    if word.label_ in ["GPE", "NORP"]:
        ents[f"{word.label_}:{word.text}"] += 1

d = [(val ,key) for key, val in ents.items()]
d.sort(reverse=True)
for val, key in d:
    print(key, val, sep="\t")

column_names=["Count",'GPE']
df = pd.DataFrame(d, columns=column_names)
print(df)
sns.barplot(data=df.nlargest(20, 'Count'), x='GPE', y="Count").set(title='NER counts')
plt.xticks(fontsize=6, rotation=90)
plt.show()

# [('NORP:European', 32), ('GPE:Italy', 23), ('GPE:China', 17), ('NORP:Italian', 16), ('GPE:Beijing', 8), ('NORP:Chinese', 7), ('GPE:Moscow', 6), ('GPE:US', 6), ('NORP:Europeans', 6), ('NORP:Maltese', 6), ('GPE:Russia', 5), ('GPE:UK', 5), ('GPE:Poland', 5), ('GPE:Malta', 5), ('GPE:France', 4), ('NORP:German', 4), ('GPE:Germany', 4), ('GPE:Brussels', 4), ('GPE:Crimea', 4), ('GPE:Paris', 3), ('GPE:MEPs', 3), ('NORP:British', 3), ('NORP:pro-European', 3), ('GPE:Strasbourg', 3), ('GPE:Pd', 3), ('NORP:American', 3), ('GPE:Hungary', 3), ('NORP:Ukrainians', 3), ('NORP:Russian', 3), ('GPE:Ireland', 
# 2), ('GPE:Belgium', 2), ('GPE:Berlin', 2), ('GPE:Rome', 2), ('GPE:The United States', 2), ('GPE:Member States', 2), ('GPE:London', 2), ('GPE:USA', 2), ('GPE:Greece', 2), ('GPE:Bulgaria', 2), ('NORP:Crimean', 2), ('GPE:Ukraine', 2), ('NORP:Polish', 2), ('NORP:French', 2), ('NORP:Islamic', 2), ('NORP:Africans', 2), ('GPE:Naples', 2), ('NORP:Irish', 1), ('GPE:Madrid', 1), ('GPE:Egypt', 1), ('GPE:Britain', 1), ('NORP:Roman', 1), ('NORP:pro-Europeans', 1), ('NORP:Milanese', 1), ('GPE:Zagreb', 1), ('GPE:America', 1), ('GPE:New York', 1), ('GPE:Rimini', 1), ('GPE:Mexico', 1), ('GPE:Canada', 1), ('NORP:Americans', 1), ('GPE:Romagna', 1), ('GPE:Brescia', 1), ('NORP:Dallan', 1), ('NORP:Pyrenees', 1), ('GPE:Rosignano Solvay', 1), ('NORP:Mise', 1), ('GPE:Solvay', 1), ('GPE:PD', 1), ('GPE:Lunigiana', 1), ('NORP:pro-EU', 1), ('GPE:Piazza San Carlo', 1), ('GPE:the Kingdom of Italy', 1), ('NORP:Austrian', 1), ('GPE:Florence', 1), ('GPE:Giorgia', 1), ('NORP:Italians', 1), ('GPE:Slovakia', 1), ('GPE:Iran', 1), ('NORP:Canadian', 1), ('NORP:Czech', 1), ('GPE:Austin', 1), ('GPE:Burbank', 1), ('GPE:California', 1), ('NORP:Eastern European', 1), ('NORP:Balkan', 1), ("GPE:the People's Republic", 1), ('GPE:Sweden', 1), ('GPE:Yalta', 1), ('GPE:The Hague', 1), ('GPE:Theresa', 1), ('NORP:trans-European', 1), ('GPE:the Czech Republic Germany', 1), 
# ('GPE:Portugal', 1), ('GPE:Romania', 1), ('GPE:Westminster', 1), ('GPE:Northern Ireland', 1), ('GPE:Gdansk', 1), ('NORP:Poles', 1), ('GPE:Milan', 1), ('NORP:Christian', 1), ('NORP:Democrat', 1), ('NORP:Japanese', 1), ('GPE:States', 1), ('NORP:Somali', 1), ('NORP:neo-Nazi', 1), ('GPE:Burkina', 1), 
# ("GPE:the Republic'", 1), ('NORP:Ghanaian', 1)]
#From the sample, we can see that 'European' and MS nationalities are the most visible entities. 

#Key 

#NORP: Nationalities or religious or political groups.
#GPE: Geo-political entities

#RCA - we need to be able to detect the actor making the claim which can usually be found by name of actor mentioned before "" of an
#utterance. For object, proxy variable is NORP. We need to find a way of connecting these two, so we can then develop a network graph.

