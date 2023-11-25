set.seed(1997)
### read dataset2
dataset2 <- read.csv("/home/pragya/pragya/LCP/SraRunTable.tsv", header = TRUE, sep = "\t")
# Assuming you have a dataset called 'dataset2'
subset_data <- dataset2[dataset2$Host_disease == "Acinar cell carcinoma", ]
# Assuming you have a dataset called 'dataset2'
##changing names to adenocarcinoma and scc

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
dataset2 <- dataset2[-1, ]

##view column23 in dataset2
column12 <- dataset2[, 23]

# Assuming you have a dataset called 'dataset2' and want to count values in column 1
counts <- table(dataset2[, 23])

### Assuming you have a dataset called 'dataset2'
rows_with_Adenosquamous <- grepl("Adenosquamous", dataset2$Host_disease)

##view sampleid with adenosquamous as host_disease 
subset_data23 <- dataset2[rows_with_Adenosquamous, ]

#### read dataset 1
dataset1 <- read.csv("/home/pragya/pragya/LCP/feature-taxonomy-table",sep = ",")

## delete column1 from dataset1
# Assuming you have a dataset called 'dataset1'
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


##remove colum x from dataset3
# Assuming you have a data frame called 'dataset3' with column 'x'

# Remove the second column from the dataset
## dataset4 <- dataset4[, -2]

##saving file dataset4 under name feature-taxonomy-subtype-merged.tsv
write.csv(merged_data,"/home/pragya/pragya/LCP/feature-taxonomy-subtype-merged")

##save this file under variable new_lcp
dataset4_lcp<-read.csv("/home/pragya/pragya/LCP/feature-taxonomy-subtype-merged")
##remove column 1
dataset4_lcp<-dataset4_lcp[,-1]
colnames(dataset4_lcp)

##saving file dataset4_lcp under name dataset4_lcp
write.csv(dataset4_lcp,"/home/pragya/pragya/LCP/dataset4_lcp")


### Read the feature table
feature_table <- read.csv("/home/pragya/pragya/LCP/Otu_Counts.tsv",
                          header = TRUE, sep = "," ,row.names = 1)
dim(feature_table)
ft = matrix(unlist(feature_table),nrow = 3607,ncol=284)
rownames(ft) = rownames(feature_table)



typeof(feature_table)

## read feature_taxonomy_table
mydata<-read.csv("/home/pragya/pragya/LCP/feature-taxonomy-table")


# Transpose the data frame
transposed_df <- t(feature_table)
typeof(transposed_df)

# Convert character values to numeric
# Apply as.numeric() to each column of the transposed data frame
transposed_df_numeric <- apply(transposed_df, 2, as.numeric)
typeof(transposed_df_numeric)



library(vegan)



# Plot the ggplot



rownames(dataset3) = dataset3$sample

library(ape)


table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 <- transposed_df[1:283, ]
ft <- matrix(as.numeric(unlist(transposed_df2)), nrow = 283, ncol = 3607)
rownames(ft)
# Define the row numbers to remove
# rows_to_remove <- 278:283

# Remove the specified rows from sample2
# ft_NA_rm <- ft[-rows_to_remove, ]

# Assuming you have a dissimilarity matrix named 'bray_curtis_matrix'

bray_dist <- vegdist(ft, method = "bray") ## binary = TRUE, diag = FALSE, upper = FALSE, na.rm = FALSE)



## Principal coordinate analysis and simple ordination plot

pcoa_data<-pcoa(bray_dist, correction="none", rn=NULL)


pcoa_data$vectors

# Set row names of 'dataset3' to the sample column
rownames(dataset3) =dataset3$sample
# Subset the metadata data frame based on the row names of 'transposed_df2'
meta_dt = dataset3[rownames(transposed_df2),]

# Create logical indices for Adenocarcinoma (ACC) and Squamous Cell Carcinoma (
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"
sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"


# Create a logical index for excluding 'Adenosquamous carcinoma'
true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
# Subset 'meta_dt' based on the logical index
meta_dt2 = meta_dt[true_index,]

library(ggplot2)

##ggplot

plot(pcoa_data$vectors[,1],pcoa_data$vectors[,2])
pcoa_df = data.frame(pcoa_data$vectors)
pcoa_df2 =pcoa_df[true_index,]
custom_colors <- c("Adenocarcinoma" = "magenta", "Squamous cell carcinoma" = "orange")

ggplot(pcoa_df2, aes(x=pcoa_df2$Axis.1,y=pcoa_df2$Axis.2, color=meta_dt2$`dataset2$Host_disease`))+
  geom_point()+xlab("Axis1")+ylab("Axis2")+
  scale_color_manual(values = custom_colors)+
  labs(color = "Groups")

### ggplot with solid circle
ggplot(pcoa_df2, aes(x=pcoa_df2$Axis.1,y=pcoa_df2$Axis.2, color=meta_dt2$`dataset2$Host_disease`))+
  geom_point()+stat_ellipse()+stat_ellipse(level = 0.95, type = "t")+xlab("Axis1")+ylab("Axis2")+
  scale_color_manual(values = custom_colors)+
  labs(color = "Lung cancer subtypes")


###permanova using adonis function for statiscally significance


##changing rownames 
rownames(meta_dt) <- 1:nrow(meta_dt) 

library(vegan)
colnames(meta_dt) <- c("sample","disease")
# adonis2(ft_NA_rm ~ disease,data=meta_dt)

# Check for missing values in the 'disease' column
sum(is.na(meta_dt$disease))
meta_dt <- na.omit(meta_dt)
adonis2(ft ~ disease, data = meta_dt)

adonis2(ft ~ disease,data=meta_dt, permutations = 10000, method = "bray",
        sqrt.dist = FALSE, add = FALSE, by = "terms",
        parallel = getOption("mc.cores"), na.action = na.fail,
        strata = NULL)
## p value = 4e-04 ***



# Save the plot as a Pdf file
ggsave("beta_bray.pdf", width = 6, height = 3, dpi =1000,path = setwd("/home/pragya/pragya/LCP/beta diversity/bray_curtis/"))


# Save the plot as a svg file
ggsave("beta_bray.svg", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/beta diversity/bray_curtis/"))

# Save the plot as a svg file
ggsave("beta_bray.png", width = 8, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/beta diversity/bray_curtis/"))

