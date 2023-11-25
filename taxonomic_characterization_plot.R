set.seed(1997)
# Read the feature table
feature_table <- read.table("/home/pragya/pragya/LCP/Otu_Counts.tsv", header = TRUE, sep = ",")
feature_table

## storing column names

column_names <-colnames(feature_table)
column_names


## remove pesky taxonomic classifier labels(i.e."d" for domain,"p" for phylum etc)
feature_table$taxonomy<-gsub("(d__)|(p__)|(c__)|(o__)|(f__)|(g__)|(s__)","",paste(feature_table$taxonomy))
library(tidyr)
library(dplyr)
feature_table <-separate(feature_table,col=taxonomy,into=c("Domain","Phylum","Class","Order","Family","Genus","Species"),sep = ";")


## Now convert the file from wide to long format using the pivot_longer()function
feature_table_long<- pivot_longer(feature_table,cols="SRR3991444":"SRR3991582",names_to ="sample",values_to = "count")
head(feature_table_long)

feature_table_long$count<-as.integer(feature_table_long$count)

#### create figures

library(ggplot2)
library(viridis)
library(dplyr)

##storing column names to loop over in the for loop
column_names<-colnames(feature_table_long)


for(i in 2:8){
  modified_data <- group_by(feature_table_long,get(column_names[i]),sample)
  print(modified_data)
  summarized_data <- summarize(modified_data, count=sum(count))
  print(summarized_data)
  colnames(summarized_data)[1] <- column_names[i]
  summarized_data[,1]<- with(summarized_data, reorder(get(column_names[i]), desc(count)))
  custom_colors <-t(as.data.frame(sort(unique(with(summarized_data,reorder(get(column_names[i]),desc(count)))))))
  colnames(custom_colors) <- custom_colors
  row.names(custom_colors) <-"Hex Color"
  custom_colors[1,]<-viridis(length(custom_colors))
  try(custom_colors[,"NA"]<-"#808080", silent=TRUE)
  
  
  
  print(ggplot(summarized_data,aes(x=sample,y=count,fill=column_names[i],group=sample))+
          geom_bar(aes(fill=get(column_names[i])),color="black",position="fill",stat="identity")+
          labs(fill=paste(column_names[i],sep=""))+
          scale_fill_manual(values=custom_colors)+
          theme(legend.position="bottom",legend.box="horizontal")
  )
}

