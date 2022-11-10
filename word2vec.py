import os
import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
import re
from sklearn.feature_extraction.text import TfidfVectorizer 
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.metrics.pairwise import euclidean_distances
import nltk
from nltk.corpus import stopwords
nltk.download('stopwords')

os.chdir(r"C:\Users\kerme\Downloads")

doc_1 = open('No.txt',  'r', encoding = 'utf-8').read().lower()
doc_2 = open('Yes.txt',  'r', encoding = 'utf-8').read().lower()
doc_3 = open('Treaties.txt',  'r', encoding = 'utf-8').read().lower()

data = [doc_1, doc_2, doc_3]

documents_df=pd.DataFrame(data,columns=['documents'])
print(documents_df)

# removing special characters and stop words from the text
stop_words_l=stopwords.words('english')
documents_df['documents_cleaned']=documents_df.documents.apply(lambda x: " ".join(re.sub(r'[^a-zA-Z]',' ',w).lower() 
for w in x.split() if re.sub(r'[^a-zA-Z]',' ',w).lower() not in stop_words_l) )
print(documents_df['documents_cleaned'])
print(documents_df)

tfidfvectoriser=TfidfVectorizer()
tfidfvectoriser.fit(documents_df.documents_cleaned)
tfidf_vectors=tfidfvectoriser.transform(documents_df.documents_cleaned)

pairwise_similarities=np.dot(tfidf_vectors,tfidf_vectors.T).toarray()
pairwise_differences=euclidean_distances(tfidf_vectors)


def most_similar(doc_id,similarity_matrix,matrix):
    print (f'Document: {documents_df.iloc[doc_id]["documents"]}')
    print ('\n')
    print ('Similar Documents:')
    if matrix=='Cosine Similarity':
        similar_ix=np.argsort(similarity_matrix[doc_id])[::-1]
    elif matrix=='Euclidean Distance':
        similar_ix=np.argsort(similarity_matrix[doc_id])
    for ix in similar_ix:
        if ix==doc_id:
            continue
        print('\n')
        print (f'Document: {documents_df.iloc[ix]["documents"]}')
        print (f'{matrix} : {similarity_matrix[doc_id][ix]}')


most_similar(0,pairwise_differences,'Euclidean Distance') 

# documents similar to the first document in the corpus
most_similar(0,pairwise_similarities,'Cosine Similarity')

print (tfidf_vectors[0].toarray())
print (pairwise_similarities.shape)

# Cosine similarity scores for 'No' Text. This means that 'No' text is 39% similar to treaty text.

print (pairwise_similarities[0][:])
# [1.         0.88343077 0.39244683]

most_similar(1,pairwise_differences,'Euclidean Distance') 

# documents similar to the first document in the corpus
most_similar(1,pairwise_similarities,'Cosine Similarity')

#################################### 'Yes' campaign

print (tfidf_vectors[1].toarray())
print (pairwise_similarities.shape)

# Cosine similarity scores for 'Yes' Text
print (pairwise_similarities[1][:])

# [0.88343077 1.         0.38768188]. These results show few differences in 'Yes' and 'No' campaign texts...























