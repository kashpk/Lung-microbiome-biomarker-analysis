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
colnames(feature_table)
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

###############################

# Plot the ggplot


simp<-diversity(as.numeric(unlist(transposed_df[1,],"simpson")))
rownames(dataset3) = dataset3$sample



table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 = transposed_df[1:283,]
ft = matrix(as.numeric(unlist(transposed_df2)),nrow = 283,ncol=3607)
simp <- diversity(ft, "simpson")
rownames(dataset3) =dataset3$sample
meta_dt = dataset3[rownames(transposed_df2),]
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"
sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"

adeno_simp = simp[adeno_index]
squa_simp = simp[sq_index]
meta_dt[adeno_index ,"scores"]  = adeno_simp
meta_dt[sq_index,"scores"] = squa_simp

true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
meta_dt2 = meta_dt[true_index,]
library(ggplot2)

plot_gg<- ggplot(meta_dt2, aes(x = meta_dt2$`dataset2$Host_disease`,y=meta_dt2$scores,fill=`dataset2$Host_disease`)) +
  geom_boxplot(width=0.3)+xlab("Lung cancer subtypes")+ylab("Simpson")+
  labs(fill = "Lung cancer subtypes")+
 scale_fill_manual(values = c("Adenocarcinoma" = "#3366CC66", "Squamous cell carcinoma" = "#FF666666"))


plot_gg

wilcox.test(adeno_simp,squa_simp)

plot_pvalue <- plot_gg + annotate(
  "text",
  x = 1.5, # X-axis position of the annotation
  y = 1.0, # Y-axis position of the annotation
  label = "p =0.5931 ", # Label text
  size = 4, # Text size
  vjust = 0.5, # Vertical justification
)
plot_pvalue 

# Save the plot as a Pdf file
ggsave("alpha_simpson.pdf", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/simpson/"))


# install.packages("svglite")


# Save the plot as a svg file
ggsave("alpha_simpson.svg", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/simpson"))

# Save the plot as a svg file
ggsave("alpha_simpson.png", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/simpson"))


#######################shannon diversity
library(vegan)
shann<-diversity(as.numeric(unlist(transposed_df[1,],"shannon")))
rownames(dataset3) = dataset3$sample



table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 = transposed_df[1:283,]
ft = matrix(as.numeric(unlist(transposed_df2)),nrow = 283,ncol=3607)
shann <- diversity(ft, "shannon")
rownames(dataset3) =dataset3$sample
meta_dt = dataset3[rownames(transposed_df2),]
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"
sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"

adeno_shann = shann[adeno_index]
squa_shann = shann[sq_index]
meta_dt[adeno_index ,"scores"]  = adeno_shann
meta_dt[sq_index,"scores"] = squa_shann

true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
meta_dt2 = meta_dt[true_index,]

plot_gg_shann<- ggplot(meta_dt2, aes(x = meta_dt2$`dataset2$Host_disease`,y=meta_dt2$scores,fill=`dataset2$Host_disease`)) +
  geom_boxplot(width=0.3)+xlab("Lung cancer subtypes")+ylab("Shannon")+
  labs(fill = "Lung cancer subtypes")+
  scale_fill_manual(values = c("Adenocarcinoma" = "#3366CC66", "Squamous cell carcinoma" = "#FF666666"))
plot_gg_shann

wilcox.test(adeno_shann,squa_shann)

plot_pvalue <- plot_gg_shann + annotate(
  "text",
  x = 1.5, # X-axis position of the annotation
  y = 5.0, # Y-axis position of the annotation
  label = "p =0.046 ", # Label text
  size = 4, # Text size
  vjust = 0.5 # Vertical justification
)
plot_pvalue 

# Save the plot as a Pdf file
ggsave("alpha_shannon.pdf", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/shannon/"))


# Save the plot as a svg file
ggsave("alpha_shannon.svg", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/shannon/"))

# Save the plot as a Png file
ggsave("alpha_shannon.png", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/shannon/"))

## inverse simpson diversity
library(vegan)
fisher_alpha<-diversity(as.numeric(unlist(transposed_df[1,],"")))
rownames(dataset3) = dataset3$sample

table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 = transposed_df[1:283,]
ft = matrix(as.numeric(unlist(transposed_df2)),nrow = 283,ncol=3607)
inv_shann <- diversity(ft, "inv")
rownames(dataset3) =dataset3$sample
meta_dt = dataset3[rownames(transposed_df2),]
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"
sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"

adeno_inv_simp = inv_shann[adeno_index]
squa_inv_simp = inv_shann[sq_index]
meta_dt[adeno_index ,"scores"]  = adeno_inv_simp
meta_dt[sq_index,"scores"] = squa_inv_simp

true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
meta_dt2 = meta_dt[true_index,]
meta_dt2

### CHECK WHETHER THIS SAMPLE IS PRESENT IN meta_dt
## sample_id <- "SRR3991571"
## row_indices <- which(meta_dt2[, 1] == sample_id)
## row_indices
## count adenocarcinoma,squamous,adenosqua in meta_dt
## count_2 <- sum(dataset3[, 2] == "Squamous cell carcinoma")
## count_2


plot_gg<- ggplot(meta_dt2, aes(x = meta_dt2$`dataset2$Host_disease`,y=meta_dt2$scores,fill=`dataset2$Host_disease`)) +
  geom_boxplot(width=0.3)+xlab("Lung cancer subtypes")+ylab("Inverse simpson")+
  labs(fill = "Lung cancer subtypes")+
  scale_fill_manual(values = c("Adenocarcinoma" = "#3366CC66", "Squamous cell carcinoma" = "#FF666666"))
plot_gg

wilcox.test(adeno_inv_simp,squa_inv_simp)

plot_pvalue <- plot_gg + annotate(
  "text",
  x = 1.5, # X-axis position of the annotation
  y = 160.0, # Y-axis position of the annotation
  label = "p =0.0465 ", # Label text
  size = 4, # Text size
  #vjust = 0.5 # Vertical justification
)
plot_pvalue 

# Save the plot as a Pdf file
ggsave("alpha_inv_simp.pdf", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/inverse simpson"))


# Save the plot as a svg file
ggsave("alpha_inv_simp.svg", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/inverse simpson/"))

# Save the plot as a Png file
ggsave("alpha_inv_simp.png", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/inverse simpson/"))





### pileous eveeness



library(vegan)
table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 = transposed_df[1:283,]
ft = matrix(as.numeric(unlist(transposed_df2)),nrow = 283,ncol=3607)
H <- diversity(ft,)
H
S<- specnumber(ft)
S
J<-H/log(S)
J
rownames(dataset3) =dataset3$sample

meta_dt = dataset3[rownames(transposed_df2),]
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"

sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"

adeno_pileou = J[adeno_index]
squa_pileou = J[sq_index]
meta_dt[adeno_index ,"scores"]  = adeno_pileou
meta_dt[sq_index,"scores"] = squa_pileou

true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
meta_dt2 = meta_dt[true_index,]

library(ggplot2)

plot_gg<- ggplot(meta_dt2, aes(x = meta_dt2$`dataset2$Host_disease`,y=meta_dt2$scores,fill=`dataset2$Host_disease`)) +
  geom_boxplot(width=0.3)+xlab("Lung cancer subtypes")+ylab("Pileou evenness")+
  labs(fill = "Lung cancer subtypes")+
  scale_x_discrete(limits = c("Adenocarcinoma", "Squamous cell carcinoma")) +
  scale_fill_manual(values = c("Adenocarcinoma" = "#3366CC66", "Squamous cell carcinoma" = "#FF666666"))



plot_gg   # Set the limits of the x-axis

wilcox.test(adeno_pileou,squa_pileou)


plot_pvalue <- plot_gg + annotate(
  "text",
  x = 1.5, # X-axis position of the annotation
  y = 1, # Y-axis position of the annotation
  label = "p =8.043e-06 ", # Label text
  size = 4,# Text size
  vjust = 0.5 # Vertical justification
)
plot_pvalue 


# Save the plot as a Pdf file
ggsave("alpha_pileou.pdf", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/Pileou/"))


# Save the plot as a svg file
ggsave("alpha_pileou.svg", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/Pileou/"))

# Save the plot as a Png file
ggsave("alpha_pileou.png", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/Pileou/"))





##############specnumber is for species richness
library(vegan)
table(dataset4_lcp$dataset2.Host_disease)
dim(transposed_df)
transposed_df2 = transposed_df[1:283,]
ft = matrix(as.numeric(unlist(transposed_df2)),nrow = 283,ncol=3607)

##specnumber is for species richness
S<- specnumber(ft)
S
rownames(dataset3) =dataset3$sample
meta_dt = dataset3[rownames(transposed_df2),]
adeno_index = meta_dt$`dataset2$Host_disease`=="Adenocarcinoma"

sq_index = meta_dt$`dataset2$Host_disease`=="Squamous cell carcinoma"
adeno_rich = S[adeno_index]
squa_rich = S[sq_index]
meta_dt[adeno_index ,"scores"]  = adeno_rich
meta_dt[sq_index,"scores"] = squa_rich
true_index = !(meta_dt$`dataset2$Host_disease`=="Adenosquamous carcinoma")
meta_dt2 = meta_dt[true_index,]

library(ggplot2)

plot_gg<- ggplot(meta_dt2, aes(x = meta_dt2$`dataset2$Host_disease`,y=meta_dt2$scores,fill=`dataset2$Host_disease`)) +
  geom_boxplot(width=0.3)+xlab("Lung cancer subtypes")+ylab("Species richness")+
  labs(fill = "Lung cancer subtypes")+
  scale_x_discrete(limits = c("Adenocarcinoma", "Squamous cell carcinoma")) +
  scale_fill_manual(values = c("Adenocarcinoma" = "#3366CC66", "Squamous cell carcinoma" = "#FF666666"))




wilcox.test(adeno_rich,squa_rich)

plot_gg   # Set the limits of the x-axis


plot_pvalue <- plot_gg + annotate(
  "text",
  x = 1.5, # X-axis position of the annotation
  y = 500, # Y-axis position of the annotation
  label = "p =8.043e-06 ", # Label text
  size = 4,# Text size
  vjust = 0.5 # Vertical justification
)
plot_pvalue 


# Save the plot as a Pdf file
ggsave("alpha_richness.pdf", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/Richness/"))


# Save the plot as a svg file
ggsave("alpha_richness.svg", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/Richness/"))

# Save the plot as a Png file
ggsave("alpha_richness.png", width = 6, height = 6, dpi =1000,path = setwd("/home/pragya/pragya/LCP/alpha diversity/Richness/"))


