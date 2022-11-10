import os
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer

os.chdir(r"C:\Users\kerme\Downloads")

doc_1 = open('No.txt',  'r', encoding = 'utf-8').read().lower()
doc_2 = open('Yes.txt',  'r', encoding = 'utf-8').read().lower()
doc_3 = open('Treaties.txt',  'r', encoding = 'utf-8').read().lower()

data = [doc_1, doc_2, doc_3]

from sklearn.feature_extraction.text import CountVectorizer

count_vectorizer = CountVectorizer(stop_words= 'english')
vector_matrix = count_vectorizer.fit_transform(data)
print(vector_matrix)

tokens = count_vectorizer.get_feature_names_out()
print(tokens)

vector_matrix.toarray()

import pandas as pd

def create_dataframe(matrix, tokens):

    doc_names = [f'doc_{i+1}' for i, _ in enumerate(matrix)]
    df = pd.DataFrame(data=matrix, index=doc_names, columns=tokens)
    return(df)

create_dataframe(vector_matrix.toarray(),tokens)

from sklearn.metrics.pairwise import cosine_similarity

cosine_similarity_matrix_1 = cosine_similarity(vector_matrix)
create_dataframe(cosine_similarity_matrix_1,['doc_1','doc_2', 'doc_3'])

from sklearn.feature_extraction.text import TfidfVectorizer

Tfidf_vect = TfidfVectorizer(stop_words = 'english')
vector_matrix = Tfidf_vect.fit_transform(data)

tokens = Tfidf_vect.get_feature_names_out()
create_dataframe(vector_matrix.toarray(),tokens)

cosine_similarity_matrix_2 = cosine_similarity(vector_matrix)
create_dataframe(cosine_similarity_matrix_2,['doc_1','doc_2', 'doc_3'])

print(cosine_similarity_matrix_1)
print(cosine_similarity_matrix_2)

#                          TREATY
#[[1.         0.89127919 0.40448366] #NO
#[0.89127919 1.          0.40506653] #YES
#[0.40448366 0.40506653 1.        ]]




#                          TREATY
#[[1.         0.8809277  0.39830065] #NO
#[0.8809277  1.         0.39195708]  #YES
#[0.39830065 0.39195708 1.        ]]