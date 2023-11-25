set.seed(1997)

######################### relative abundance plot ##

# Removing all NA and making split barplot
### read dataset2
## it has dimension 301 rows and 39 column .It is basically sraruntable data(metadata information)
dataset2 <- read.csv("/home/pragya/pragya/LCP/SraRunTable.tsv", header = TRUE, sep = "\t")

# Assuming you have a dataset called 'dataset2'
## subset_data <- dataset2[dataset2$Host_disease == "Acinar cell carcinoma", ]

# In dataset2', i am changing names to adenocarcinoma and scc
#and saving in variable changed_data
changed_data<- dataset2$Host_disease[dataset2$Host_disease == "Acinar cell carcinoma"] <- "Adenocarcinoma"


dataset2$Host_disease[dataset2$Host_disease == "Adenocarcinoma with neuroendocrine differen."] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Bronchiolo-alveolar adenocarcinoma"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Bronchiolo-alveolar carcinoma\\, non-mucinous"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Bronchiolo-alveolar carcinoma\\, non-mucinous"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Clear cell adenocarcinoma\\, NOS"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Mucinous adenocarcinoma"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Papillary adenocarcinoma\\, NOS"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Solid carcinoma\\, NOS"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Adenocarcinoma with mixed subtypes"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Adenocarcinoma\\, NOS"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Bronchiolo-alveolar carcinoma\\, mucinous"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Non-small cell carcinoma"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Signet ring cell carcinoma"] <- "Adenocarcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Sq. cell carcinoma\\, spindle cell"] <- "Squamous cell carcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Squamous cell carcinoma\\, NOS"] <- "Squamous cell carcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Basaloid squamous cell carcinoma"] <- "Squamous cell carcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Lymphoepithelial carcinoma"] <- "Squamous cell carcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Sq. cell carcinoma\\, keratinizing\\, NOS"] <- "Squamous cell carcinoma"
dataset2$Host_disease[dataset2$Host_disease == "Squamous cell carcinoma\\, clear cell type"] <- "Squamous cell carcinoma"

## delete row 2 from dataset2
# Assuming you have a dataset called 'dataset2'
#dataset2 <- dataset2[-1, ]

##view column23 in dataset2
column12 <- dataset2[, 23]
column12

# In 'dataset2' i want to count values in column 23
#it gives count of acc  ac+sc and scc
counts <- table(dataset2[, 23])
counts

### In 'dataset2' , i am indexing Adenosquamous ,whereever it is present it gives true
#otherwise false
rows_with_Adenosquamous <- grepl("Adenosquamous", dataset2$Host_disease)
rows_with_Adenosquamous

##In 'dataset2' , i am getting rows with Adenosquamous ,it gives sample id
subset_data23 <- dataset2[rows_with_Adenosquamous, ]

#### read dataset 1
dataset1 <- read.csv("/home/pragya/pragya/LCP/feature-taxonomy-table",sep = ",")

## delete column1 from dataset1
dataset1 <- dataset1[,-1]

##changing names of columns - 
colnames(dataset2)[1] <- "sample"

##new dataset
dataset3<-cbind.data.frame(dataset2$sample,dataset2$Host_disease)

##change column names
colnames(dataset3)[1] <- "sample"
colnames(dataset1)[1] <- "otu_id"
# Perform the merge
merged_data <- merge(dataset1, dataset3, by = "sample", all.x = TRUE)

##saving file dataset4 under name feature-taxonomy-subtype-merged.tsv
write.csv(merged_data,"/home/pragya/pragya/LCP/feature-taxonomy-subtype-merged")

##save this file under variable new_lcp
dataset4_lcp<-read.csv("/home/pragya/pragya/LCP/feature-taxonomy-subtype-merged")
##remove column 1
dataset4_lcp<-dataset4_lcp[,-1]
colnames(dataset4_lcp)

##saving file dataset4_lcp under name dataset4_lcp
write.csv(dataset4_lcp,"/home/pragya/pragya/LCP/dataset4_lcp")

#*************************************************************#

### Read the feature table
feature_table <- read.csv("/home/pragya/pragya/LCP/Otu_Counts.tsv",
                          header = TRUE, sep = "," ,row.names = 1)
dim(feature_table)
colnames(feature_table)
ft = matrix(unlist(feature_table),nrow = 3607,ncol=284)
rownames(ft) = rownames(feature_table)



typeof(feature_table)

## read feature_taxonomy_table
mydata<-read.csv("/home/pragya/pragya/LCP/feature-taxonomy-table")


# Transpose the data frame
transposed_df <- t(feature_table)
typeof(transposed_df)
colnames(transposed_df)
rownames(transposed_df)

##saving file transposed_df under name kegg_data
write.csv(transposed_df,"/home/pragya/pragya/LCP/kegg_data")

# Convert character values to numeric
# Apply as.numeric() to each column of the transposed data frame
transposed_df_numeric <- apply(transposed_df, 2, as.numeric)
typeof(transposed_df_numeric)
warnings()

#install.packages('vegan')
library(vegan)



# Plot the ggplot



rownames(dataset3) = dataset3$sample

library(ape)


table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 <- transposed_df[1:283, ]
ft <- matrix(as.numeric(unlist(transposed_df2)), nrow = 283, ncol = 3607)


# Set row names of 'dataset3' to the sample column
rownames(dataset3) =dataset3$sample
# Subset the metadata data frame based on the row names of 'transposed_df2'
meta_dt = dataset3[rownames(transposed_df2),]
exists("dataset3")
exists("transposed_df2")
dim(dataset3)
dim(transposed_df2)
rownames(transposed_df2)
rownames(dataset3)
colnames(dataset3$`dataset2$Host_disease`)




# Create logical indices for Adenocarcinoma (ACC) and Squamous Cell Carcinoma (
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"
sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"


# Create a logical index for excluding 'Adenosquamous carcinoma'
true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
# Subset 'meta_dt' based on the logical index
meta_dt2 = meta_dt[true_index,]

##changing rownames 
rownames(meta_dt) <- 1:nrow(meta_dt) 


######################for phylum###########################################



#colnames(summarized_data)[1] <- column_names[i]
#summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
###
rownames(meta_dt) = meta_dt$sample 
colnames(meta_dt)[2]<-"disease"

###
library(tidyr)
library(dplyr)
## remove pesky taxonomic classifier labels(i.e."d" for domain,"p" for phylum etc)
feature_table$taxonomy<-gsub("(d__)|(p__)|(c__)|(o__)|(f__)|(g__)|(s__)","",paste(feature_table$taxonomy))

## Now convert the file from wide to long format using the pivot_longer()function
feature_table_long<- pivot_longer(feature_table,cols="SRR3991444":"SRR3991582",names_to ="sample",values_to = "count")
head(feature_table_long)

feature_table_long$count<-as.integer(feature_table_long$count)

### adding disease column and removing Adenosquamous carcinoma
feature_table_long$disease = meta_dt[feature_table_long$sample,]$disease
feature_table_long2 = feature_table_long[!feature_table_long$disease=="Adenosquamous carcinoma",]
### this will plot for phylum, likewise change for class,order,family etc.

modified_data <- group_by(feature_table_long2,Phylum,sample)

summarized_data <- summarize(modified_data, count=sum(count))
###converting to relative abundance from counts
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance
summarized_data$relative_abundance <- summarized_data$count / total_count

# Display the updated dataframe
summarized_data

##convert relative abundance to % relative abundance

library(dplyr)
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance and multiply by 100
summarized_data <- summarized_data %>% 
  mutate(relative_abundance = (count / total_count) * 100)

# Display the updated dataframe
summarized_data

factor(summarized_data$Phylum)
summarized_data$Phylum=="NA"
sum(summarized_data$Phylum=="NA")
sum(is.na(summarized_data$Phylum))
~is.na(summarized_data$Phylum)
!is.na(summarized_data$Phylum)
summarized_data2 = summarized_data[!is.na(summarized_data$Phylum),]

summarized_data$disease =  meta_dt[summarized_data$sample,]$disease
## 
summarized_data2 = summarized_data[!is.na(summarized_data$Phylum),]



################################################

### install.packages("forcats")
library(forcats)

# Define the distinct colors for each Phylum
phylum_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", 
                   "#FF00FF", "#FFA500", "#008000", "#800080", "#FFC0CB", 
                   "#800000", "#000080", "#FFD700", "#008080", "#800080", 
                   "#FF1493", "#00FA9A", "#FF4500", "#008B8B", "#C71585",
                   "#FF69B4", "#1E90FF", "#FF6347", "#00CED1", "#DA70D6",
                   "#ADFF2F", "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF",
                   "#32CD32", "#9400D3", "#FFD700", "#98FB98", "#8B008B",
                   "#DC143C", "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1",
                   "#6495ED", "#FF4500")

ggplot(summarized_data2, aes(x = disease, y = relative_abundance, fill = fct_reorder(Phylum, relative_abundance))) +
  geom_bar(position = position_stack(vjust = 0.5), stat = "identity", width = 0.8) +
  labs(x = "Lung cancer", y = "Relative abundance(%)",
       fill = "Phylum") +
  scale_fill_manual(values = phylum_colors)+
  theme(legend.position = "right")


# Save the plot as a PNG file
ggsave("ggplot_phylum.png", width = 8, height = 6, dpi =1000, path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_phylum.pdf", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a svg file
ggsave("ggplot_phylum.svg", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

##install.packages("svglite")


## above code is for phylum ,similarly we can plot for other classifications.

####################for genus#################################################

#colnames(summarized_data)[1] <- column_names[i]
#summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
###
rownames(meta_dt) = meta_dt$sample 
colnames(meta_dt)[2]<-"disease"

###
### adding disease column and removing Adenosquamous carcinoma
feature_table_long$disease = meta_dt[feature_table_long$sample,]$disease
feature_table_long2 = feature_table_long[!feature_table_long$disease=="Adenosquamous carcinoma",]
### this will plot for phylum, likewise change for class,order,family etc.

modified_data <- group_by(feature_table_long2,Genus,sample)

summarized_data <- summarize(modified_data, count=sum(count))
###converting to relative abundance from counts
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance
summarized_data$relative_abundance <- summarized_data$count / total_count

# Display the updated dataframe
summarized_data

##convert relative abundance to % relative abundance

library(dplyr)
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance and multiply by 100
summarized_data <- summarized_data %>% 
  mutate(relative_abundance = (count / total_count) * 100)

# Display the updated dataframe
summarized_data

factor(summarized_data$Genus)
summarized_data$Genus=="NA"
sum(summarized_data$Genus=="NA")
sum(is.na(summarized_data$Genus))
~is.na(summarized_data$Genus)
!is.na(summarized_data$Genus)
summarized_data2 = summarized_data[!is.na(summarized_data$Genus),]

summarized_data$disease =  meta_dt[summarized_data$sample,]$disease
## 
summarized_data2 = summarized_data[!is.na(summarized_data$Genus),]






library(forcats)

# Define the distinct colors for each Genus
genus_colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", 
  "#FF00FF", "#FFA500", "#008000", "#800080", "#FFC0CB", 
  "#800000", "#000080", "#FFD700", "#008080", "#800080", 
  "#FF1493", "#00FA9A", "#FF4500", "#008B8B", "#C71585",
  "#FF69B4", "#1E90FF", "#FF6347", "#00CED1", "#DA70D6",
  "#ADFF2F", "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF",
  "#32CD32", "#9400D3", "#FFD700", "#98FB98", "#8B008B",
  "#DC143C", "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1",
  "#6495ED", "#FF4500", "#FF0000", "#00FF00", "#0000FF", 
  "#FFFF00", "#00FFFF", "#FF00FF", "#FFA500", "#008000", 
  "#800080", "#FFC0CB", "#800000", "#000080", "#FFD700", 
  "#008080", "#800080", "#FF1493", "#00FA9A", "#FF4500", 
  "#008B8B", "#C71585", "#FF69B4", "#1E90FF", "#FF6347", 
  "#00CED1", "#DA70D6", "#ADFF2F", "#6A5ACD", "#FF8C00", 
  "#00BFFF", "#FF00FF", "#32CD32", "#9400D3", "#FFD700", 
  "#98FB98", "#8B008B", "#DC143C", "#00FFFF", "#FF7F50", 
  "#00FF7F", "#FFB6C1", "#6495ED", "#FF4500", "#FF0000", 
  "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", 
  "#FFA500", "#008000", "#800080", "#FFC0CB", "#800000", 
  "#000080", "#FFD700", "#008080", "#800080", "#FF1493", 
  "#00FA9A", "#FF4500", "#008B8B", "#C71585", "#FF69B4", 
  "#1E90FF", "#FF6347", "#00CED1", "#DA70D6", "#ADFF2F", 
  "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF", "#32CD32", 
  "#9400D3", "#FFD700", "#98FB98", "#8B008B", "#DC143C", 
  "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1", "#6495ED", 
  "#FF4500")




ggplot(summarized_data2, aes(x = disease, y = relative_abundance, fill = fct_reorder(Genus, relative_abundance))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Lung cancer", y = "Relative abundance(%)",
       fill = "Genus") +
  scale_fill_manual(values = genus_colors)+
  theme(legend.position = "right")


# Save the plot as a PNG file
ggsave("ggplot_genus.png", width = 20, height = 16, dpi =1000, path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_genus.pdf", width = 20, height = 16, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a svg file
ggsave("ggplot_genus.svg", width = 20, height = 16, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))



## above code is for genus ,similarly we can plot for other classifications.

####################for species##################################



##for species

#colnames(summarized_data)[1] <- column_names[i]
#summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
###
rownames(meta_dt) = meta_dt$sample 
colnames(meta_dt)[2]<-"disease"

###
### adding disease column and removing Adenosquamous carcinoma
feature_table_long$disease = meta_dt[feature_table_long$sample,]$disease
feature_table_long2 = feature_table_long[!feature_table_long$disease=="Adenosquamous carcinoma",]
### this will plot for phylum, likewise change for class,order,family etc.

modified_data <- group_by(feature_table_long2,Species,sample)

summarized_data <- summarize(modified_data, count=sum(count))
###converting to relative abundance from counts
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance
summarized_data$relative_abundance <- summarized_data$count / total_count

# Display the updated dataframe
summarized_data

##convert relative abundance to % relative abundance

library(dplyr)
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance and multiply by 100
summarized_data <- summarized_data %>% 
  mutate(relative_abundance = (count / total_count) * 100)

# Display the updated dataframe
summarized_data

factor(summarized_data$Species)
summarized_data$Species=="NA"
sum(summarized_data$Species=="NA")
sum(is.na(summarized_data$Species))
~is.na(summarized_data$Species)
!is.na(summarized_data$Species)
summarized_data2 = summarized_data[!is.na(summarized_data$Species),]

summarized_data$disease =  meta_dt[summarized_data$sample,]$disease
## 
summarized_data2 = summarized_data[!is.na(summarized_data$Species),]



library(forcats)

# Define the distinct colors for each Species
Species_colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", 
  "#FF00FF", "#FFA500", "#008000", "#800080", "#FFC0CB", 
  "#800000", "#000080"
)





ggplot(summarized_data2, aes(x = disease, y = relative_abundance, fill = fct_reorder(Species, relative_abundance))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Lung cancer", y = "Relative abundance(%)",
       fill = "Species") +
  scale_fill_manual(values = Species_colors)+
  theme(legend.position = "right")

# Save the plot as a PNG file
ggsave("ggplot_species.png", width = 8, height = 6, dpi =1000, path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_species.pdf", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a svg file
ggsave("ggplot_species.svg", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))



####################for class#############

#colnames(summarized_data)[1] <- column_names[i]
#summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
###
rownames(meta_dt) = meta_dt$sample 
colnames(meta_dt)[2]<-"disease"

###
### adding disease column and removing Adenosquamous carcinoma
feature_table_long$disease = meta_dt[feature_table_long$sample,]$disease
feature_table_long2 = feature_table_long[!feature_table_long$disease=="Adenosquamous carcinoma",]
### this will plot for phylum, likewise change for class,order,family etc.

modified_data <- group_by(feature_table_long2,Class,sample)

summarized_data <- summarize(modified_data, count=sum(count))
###converting to relative abundance from counts
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance
summarized_data$relative_abundance <- summarized_data$count / total_count

# Display the updated dataframe
summarized_data

##convert relative abundance to % relative abundance

library(dplyr)
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance and multiply by 100
summarized_data <- summarized_data %>% 
  mutate(relative_abundance = (count / total_count) * 100)

# Display the updated dataframe
summarized_data

factor(summarized_data$Class)
summarized_data$Class=="NA"
sum(summarized_data$Class=="NA")
sum(is.na(summarized_data$Class))
~is.na(summarized_data$Class)
!is.na(summarized_data$Class)
summarized_data2 = summarized_data[!is.na(summarized_data$Class),]

summarized_data$disease =  meta_dt[summarized_data$sample,]$disease
## 
summarized_data2 = summarized_data[!is.na(summarized_data$Class),]




library(forcats)

# Define the distinct colors for each Class
Class_colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", 
  "#FF00FF", "#FFA500", "#008000", "#800080", "#FFC0CB", 
  "#800000", "#000080", "#FFD700", "#008080", "#800080", 
  "#FF1493", "#00FA9A", "#FF4500", "#008B8B", "#C71585",
  "#FF69B4", "#1E90FF", "#FF6347", "#00CED1", "#DA70D6",
  "#ADFF2F", "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF",
  "#32CD32", "#9400D3", "#FFD700", "#98FB98", "#8B008B",
  "#DC143C", "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1",
  "#6495ED", "#FF4500"
)




ggplot(summarized_data2, aes(x = disease, y = relative_abundance, fill = fct_reorder(Class, relative_abundance))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Lung cancer", y = "Relative abundance(%)",
       fill = "Class") +
  scale_fill_manual(values = Class_colors)+
  theme(legend.position = "right")


# Save the plot as a PNG file
ggsave("ggplot_class.png", width = 8, height = 6, dpi =1000, path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_class.pdf", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_class.svg", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))


####################for order####


#colnames(summarized_data)[1] <- column_names[i]
#summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
###
rownames(meta_dt) = meta_dt$sample 
colnames(meta_dt)[2]<-"disease"

###
### adding disease column and removing Adenosquamous carcinoma
feature_table_long$disease = meta_dt[feature_table_long$sample,]$disease
feature_table_long2 = feature_table_long[!feature_table_long$disease=="Adenosquamous carcinoma",]
### this will plot for phylum, likewise change for class,order,family etc.

modified_data <- group_by(feature_table_long2,Order,sample)

summarized_data <- summarize(modified_data, count=sum(count))
###converting to relative abundance from counts
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance
summarized_data$relative_abundance <- summarized_data$count / total_count

# Display the updated dataframe
summarized_data

##convert relative abundance to % relative abundance

library(dplyr)
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance and multiply by 100
summarized_data <- summarized_data %>% 
  mutate(relative_abundance = (count / total_count) * 100)

# Display the updated dataframe
summarized_data

factor(summarized_data$Order)
summarized_data$Order=="NA"
sum(summarized_data$Order=="NA")
sum(is.na(summarized_data$Order))
~is.na(summarized_data$Order)
!is.na(summarized_data$Order)
summarized_data2 = summarized_data[!is.na(summarized_data$Order),]

summarized_data$disease =  meta_dt[summarized_data$sample,]$disease
## 
summarized_data2 = summarized_data[!is.na(summarized_data$Order),]






library(forcats)

# Define the distinct colors for each Order
Order_colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", 
  "#FF00FF", "#FFA500", "#008000", "#800080", "#FFC0CB", 
  "#800000", "#000080", "#FFD700", "#008080", "#800080", 
  "#FF1493", "#00FA9A", "#FF4500", "#008B8B", "#C71585",
  "#FF69B4", "#1E90FF", "#FF6347", "#00CED1", "#DA70D6",
  "#ADFF2F", "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF",
  "#32CD32", "#9400D3", "#FFD700", "#98FB98", "#8B008B",
  "#DC143C", "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1",
  "#6495ED", "#FF4500", "#FF0000", "#00FF00", "#0000FF", 
  "#FFFF00", "#00FFFF", "#FF00FF", "#FFA500", "#008000", 
  "#800080", "#FFC0CB", "#800000", "#000080", "#FFD700", 
  "#008080", "#800080", "#FF1493", "#00FA9A", "#FF4500", 
  "#008B8B", "#C71585", "#FF69B4", "#1E90FF", "#FF6347", 
  "#00CED1", "#DA70D6", "#ADFF2F", "#6A5ACD", "#FF8C00", 
  "#00BFFF", "#FF00FF", "#32CD32", "#9400D3", "#FFD700", 
  "#98FB98", "#8B008B", "#DC143C", "#00FFFF", "#FF7F50", 
  "#00FF7F", "#FFB6C1", "#6495ED", "#FF4500", "#FF0000", 
  "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", 
  "#FFA500", "#008000", "#800080", "#FFC0CB", "#800000", 
  "#000080", "#FFD700", "#008080", "#800080", "#FF1493", 
  "#00FA9A", "#FF4500", "#008B8B", "#C71585", "#FF69B4", 
  "#1E90FF", "#FF6347", "#00CED1", "#DA70D6", "#ADFF2F", 
  "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF", "#32CD32", 
  "#9400D3", "#FFD700", "#98FB98", "#8B008B", "#DC143C", 
  "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1", "#6495ED", 
  "#FF4500"
)




ggplot(summarized_data2, aes(x = disease, y = relative_abundance, fill = fct_reorder(Order, relative_abundance))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Lung cancer", y = "Relative abundance(%)",
       fill = "Order") +
  scale_fill_manual(values = Order_colors)+
  theme(legend.position = "right")


# Save the plot as a PNG file
ggsave("ggplot_order.png", width = 8, height = 6, dpi =1000, path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_order.pdf", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a svg file
ggsave("ggplot_order.svg", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

####################for family###############


#colnames(summarized_data)[1] <- column_names[i]
#summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
###
rownames(meta_dt) = meta_dt$sample 
colnames(meta_dt)[2]<-"disease"

###
### adding disease column and removing Adenosquamous carcinoma
feature_table_long$disease = meta_dt[feature_table_long$sample,]$disease
feature_table_long2 = feature_table_long[!feature_table_long$disease=="Adenosquamous carcinoma",]
### this will plot for phylum, likewise change for class,order,family etc.

modified_data <- group_by(feature_table_long2,Family,sample)

summarized_data <- summarize(modified_data, count=sum(count))
###converting to relative abundance from counts
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance
summarized_data$relative_abundance <- summarized_data$count / total_count

# Display the updated dataframe
summarized_data

##convert relative abundance to % relative abundance

library(dplyr)
# Calculate the sum of counts
total_count <- sum(summarized_data$count)

# Convert count to relative abundance and multiply by 100
summarized_data <- summarized_data %>% 
  mutate(relative_abundance = (count / total_count) * 100)

# Display the updated dataframe
summarized_data

factor(summarized_data$Family)
summarized_data$Family=="NA"
sum(summarized_data$Family=="NA")
sum(is.na(summarized_data$Family))
~is.na(summarized_data$Family)
!is.na(summarized_data$Family)
summarized_data2 = summarized_data[!is.na(summarized_data$Family),]

summarized_data$disease =  meta_dt[summarized_data$sample,]$disease
## 
summarized_data2 = summarized_data[!is.na(summarized_data$Family),]






library(forcats)

# Define the distinct colors for each Family
Family_colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", 
  "#FF00FF", "#FFA500", "#008000", "#800080", "#FFC0CB", 
  "#800000", "#000080", "#FFD700", "#008080", "#800080", 
  "#FF1493", "#00FA9A", "#FF4500", "#008B8B", "#C71585",
  "#FF69B4", "#1E90FF", "#FF6347", "#00CED1", "#DA70D6",
  "#ADFF2F", "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF",
  "#32CD32", "#9400D3", "#FFD700", "#98FB98", "#8B008B",
  "#DC143C", "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1",
  "#6495ED", "#FF4500", "#FF0000", "#00FF00", "#0000FF", 
  "#FFFF00", "#00FFFF", "#FF00FF", "#FFA500", "#008000", 
  "#800080", "#FFC0CB", "#800000", "#000080", "#FFD700", 
  "#008080", "#800080", "#FF1493", "#00FA9A", "#FF4500", 
  "#008B8B", "#C71585", "#FF69B4", "#1E90FF", "#FF6347", 
  "#00CED1", "#DA70D6", "#ADFF2F", "#6A5ACD", "#FF8C00", 
  "#00BFFF", "#FF00FF", "#32CD32", "#9400D3", "#FFD700", 
  "#98FB98", "#8B008B", "#DC143C", "#00FFFF", "#FF7F50", 
  "#00FF7F", "#FFB6C1", "#6495ED", "#FF4500", "#FF0000", 
  "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", 
  "#FFA500", "#008000", "#800080", "#FFC0CB", "#800000", 
  "#000080", "#FFD700", "#008080", "#800080", "#FF1493", 
  "#00FA9A", "#FF4500", "#008B8B", "#C71585", "#FF69B4", 
  "#1E90FF", "#FF6347", "#00CED1", "#DA70D6", "#ADFF2F", 
  "#6A5ACD", "#FF8C00", "#00BFFF", "#FF00FF", "#32CD32", 
  "#9400D3", "#FFD700", "#98FB98", "#8B008B", "#DC143C", 
  "#00FFFF", "#FF7F50", "#00FF7F", "#FFB6C1", "#6495ED", 
  "#FF4500"
)




ggplot(summarized_data2, aes(x = disease, y = relative_abundance, fill = fct_reorder(Family, relative_abundance))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Lung cancer", y = "Relative abundance(%)",
       fill = "Family") +
  scale_fill_manual(values = Family_colors)+
  theme(legend.position = "right")



# Save the plot as a PNG file
ggsave("ggplot_family.png", width = 8, height = 6, dpi =1000, path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))

# Save the plot as a Pdf file
ggsave("ggplot_family.pdf", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))


# Save the plot as a svg file
ggsave("ggplot_family.svg", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/taxa plots/differential relative abundance"))


