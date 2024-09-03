# Lung-microbiome-biomarker-analysis
Non-small-cell lung cancer (NSCLC) accounts for 80-85% of lung cancer (LC) cases, mainly classified into Adenocarcinoma (AC) and Squamous cell carcinoma (SCC). 
The disruption in the lung microbiome homeostasis was correlated with the increased risk of LC. 
Therefore, we aim to classify AC and SCC using lung microbiome with machine learning (ML) and deep learning (DL) based algorithms for early diagnosis and prognosis to enhance survival. 

## Methods
We have obtained 16S rRNA sequencing data from the NCBI database. In total, 136 AC and 132 SCC patients samples for their lung microbiome and metadata were analyzed. 
The differential microbial features of the analyzed data were extracted for classifying the NSCLC subtypes. Further, various ML algorithms were implemented to select the best microbiome features for subtype classification. 
The prediction performance of the models was evaluated for their classification potential of the selected features using 5-fold validation.

## Results
ML-based models were developed to discriminate NSCLC subtypes based on their microbial information.
Consequently, 17 features were extracted as a biomarker, and they showed good performance in distinguishing AC from SCC with an accuracy of 76.25% in the XGB on original and independent dataset.

## Conclusion
This study proposed a supervised ML framework where we can rely on taxonomic features along with ML and DL techniques to classify overlapped AC and SCC metagenomic data. 
This framework provides lung microbiome as a predictive and diagnostic biomarker in LC. Moreover, this framework will also be helpful to obtain further biomarkers and perform analysis of overlapped subtypes in different diseases.
 
*************************************************************************************************************************************************************

