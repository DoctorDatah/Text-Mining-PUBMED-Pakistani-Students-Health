# Text-Mining-PUBMED-Pakistani-Students-Health

![Text Mining](img/cover/text-mining-cover.jpg)

## What sort of documents are published on Pakistani students health? 


## Introduction #
The Project is focused on text mining the documents from PubMed.org related to Pakistani students health. 

## Data Source 
Source: [(here)](https://pubmed.ncbi.nlm.nih.gov/?term=pakistani+students+health).


### directory structure
------------

The directory structure of project:

```

├── README.md          <- The top-level README for project report.
├── data			   <- MubMed.org downloaded documents 
├── lib			       <- Custom modules
│   ├── PhraseMining_Utils.R     
│   └── PubMed_Utils.R            
│   └── SentimentAnalysis_Utils.R             
│   └── toDataFrame.R            

├── _docs              <- docs used in report
│
├── notebooks          <- Jupyter notebooks. 
│
├── images             <- Generated graphics and figures to be used in reporting
│
├── src                <- Source code for use in this project.
│   ├── __init__.py    <- Makes src a Python module
│   │
│   ├── data           <- Scrap and format data
│   │   └── read_write.py
