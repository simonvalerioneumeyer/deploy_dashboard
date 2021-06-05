from sqlalchemy import create_engine
import pandas as pd
from gensim.corpora.dictionary import Dictionary
from gensim.models.tfidfmodel import TfidfModel
from gensim.models import LdaModel
from gensim.matutils import corpus2csc, Sparse2Corpus
from gensim.models.ldamulticore import LdaMulticore
import re
from gensim.parsing.preprocessing import preprocess_string, split_alphanum, strip_tags, strip_punctuation, strip_multiple_whitespaces, strip_numeric,remove_stopwords ,strip_short ,stem_text
from pprint import pprint

def sql_to_list_converter(chunks):
    preprocessed_texts = []
    for i in chunks:
        
        #have to split each sentence since this is the input for gensim
        list_to_add = [iterator.split() for iterator in i.preprocessed_text.tolist()]
        preprocessed_texts += list_to_add
        
    return preprocessed_texts

def tf_idf_vect(preprocessed_texts):
    dictionary = Dictionary(preprocessed_texts)
    
    # remove words that appear in less than 20 documents, or more than 50% of the documents.
    dictionary.filter_extremes(no_below=20, no_above=0.5)

    num_docs = dictionary.num_docs
    num_terms = len(dictionary.keys())
    
    #transform into bow
    corpus_bow = [dictionary.doc2bow(doc) for doc in preprocessed_texts]
    
    #transform into tf-idf:
    tfidf = TfidfModel(corpus_bow) #,normalize = True)
    corpus_tfidf = tfidf[corpus_bow]
    
    #transform into sparse matrix:
    corpus_tfidf_sparse = corpus2csc(corpus_tfidf, num_terms,num_docs=num_docs)
    
                       
    #NB: After reading online, LDA works just as well and even better in some case
    #with corpus_tfidf, so I can use the tfidf instead
    #NB: need to return dictionary to use in the LDA model
    
    return dictionary, corpus_tfidf, corpus_bow #, corpus_tfidf_sparse



# get the data:
engine = create_engine('mysql+pymysql://bes:seb_1_bes@18.224.190.138:3306/tweets', echo=False)
chunks = pd.read_sql('SELECT preprocessed_text FROM seb_table LIMIT 2;', con=engine, chunksize=1000)

prep_list = sql_to_list_converter(chunks)
dictionary, corpus_tfidf, corpus_bow = tf_idf_vect(prep_list)


# Set training parameters.
num_topics = 10
chunksize = 2000
passes = 20
iterations = 400
eval_every = None  # Don't evaluate model perplexity, takes too much time.

# Make a index to word dictionary.
temp = dictionary[0]  # This is only to "load" the dictionary.
id2word = dictionary.id2token

model = LdaMulticore(
    corpus=corpus_tfidf,
    id2word=id2word,
    chunksize=chunksize,
    workers=None,
    alpha='auto',
    eta='auto',
    iterations=iterations,
    num_topics=num_topics,
    passes=passes,
    eval_every=eval_every
)

dict_to_export = dictionary.token2id
corp_to_export = corpus_bow