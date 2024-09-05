library(xlsx)
library(ggplot2)
library(ggplot2)
library(ggpubr)

load("graph_prop.rda")

results$Genotype = gsub("2HN","22", results$Genotype)
results$Genotype = gsub("3HN","33", results$Genotype)
results$Genotype = gsub("4HN","44", results$Genotype)
results = results[results$Genotype !="KO",]

my_comparisons <- list( c("E22", "E33") ,  c("E22", "E44"),  c("E33", "E44")  )


ggplot(results, aes_string(x="Genotype", y="CC", color ="Genotype" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Clustering coefficient") 
ggsave("CC_Geno.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )



ggplot(results, aes_string(x="Genotype", y="PL", color ="Genotype" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Path Length") 
ggsave("PL_Geno.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )




ggplot(results, aes_string(x="Genotype", y="BW", color ="Genotype" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Betweenness") 
ggsave("BW_Geno.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Genotype", y="DG", color ="Genotype" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Degree of Connectivity") 
ggsave("DG_Geno.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )


######################


my_comparisons <- list( c("Male", "Female")  )


ggplot(results, aes_string(x="Sex", y="CC", color ="Sex" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Clustering coefficient") 
ggsave("CC_Sex.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )



ggplot(results, aes_string(x="Sex", y="PL", color ="Sex" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Path Length") 
ggsave("PL_Sex.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )




ggplot(results, aes_string(x="Sex", y="BW", color ="Sex" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Betweenness") 
ggsave("BW_Sex.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Sex", y="DG", color ="Sex" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Degree of Connectivity") 
ggsave("DG_Sex.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )




###################
######################
# #no HFD 
# table(results3$Exercise, results3$Genotype, results3$Sex)
########

my_comparisons <- list( c("HFD", "CTRL")  )

results2 = results[results$Exercise=="NO",]
ggplot(results2, aes_string(x="Diet", y="CC", color ="Diet" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Clustering coefficient") 
ggsave("CC_Diet.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )



ggplot(results2, aes_string(x="Diet", y="PL", color ="Diet" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Path Length") 
ggsave("PL_Diet.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )




ggplot(results, aes_string(x="Diet", y="BW", color ="Diet" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Betweenness") 
ggsave("BW_Diet.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results2, aes_string(x="Diet", y="DG", color ="Diet" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Degree of Connectivity") 
ggsave("DG_Diet.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )




###################
######################

results$Exercise[results$Exercise=="NO"] = "Sedentary"
results$Exercise[results$Exercise=="YES"] = "Exercise"
my_comparisons <- list( c("Sedentary", "Exercise")  )
results3 = results[results$Diet=="CTRL",]

ggplot(results3, aes_string(x="Exercise", y="CC", color ="Exercise" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Clustering coefficient") 
ggsave("CC_Exercise.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )



ggplot(results3, aes_string(x="Exercise", y="PL", color ="Exercise" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Path Length") 
ggsave("PL_Exercise.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )




ggplot(results3, aes_string(x="Exercise", y="BW", color ="Exercise" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Betweenness") 
ggsave("BW_Exercise.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results3, aes_string(x="Exercise", y="DG", color ="Exercise" )) + 
  geom_violin() + geom_boxplot() + geom_blank() + theme_bw() +  geom_point() + 
  stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20), legend.position="none") +   scale_y_continuous("Degree of Connectivity") 
ggsave("DG_Exercise.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )
##################
results$Age = as.numeric(results$Age ) 

ggplot(results, aes_string(x="Age", y="CC", color ="Genotype" )) + geom_point(aes_string(x="Age", y="CC", color ="Genotype" )) +
 geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Clustering coefficient") 
ggsave("CC_Age.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Age", y="PL", color ="Genotype" )) + geom_point(aes_string(x="Age", y="PL", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Path Length") 
ggsave("PL_Age.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Age", y="BW", color ="Genotype" )) + geom_point(aes_string(x="Age", y="BW", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Betweenness") 
ggsave("BW_Age.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Age", y="DG", color ="Genotype" )) + geom_point(aes_string(x="Age", y="DG", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Degree of Connectivity") 
ggsave("DG_Age.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )



###################
##################
results$Mass = as.numeric(results$Mass  ) 

ggplot(results, aes_string(x="Mass", y="CC", color ="Genotype" )) + geom_point(aes_string(x="Mass", y="CC", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Clustering coefficient") 
ggsave("CC_Mass.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Mass", y="PL", color ="Genotype" )) + geom_point(aes_string(x="Mass", y="PL", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Path Length") 
ggsave("PL_Mass.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Mass", y="BW", color ="Genotype" )) + geom_point(aes_string(x="Mass", y="BW", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Betweenness") 
ggsave("BW_Mass.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )

ggplot(results, aes_string(x="Mass", y="DG", color ="Genotype" )) + geom_point(aes_string(x="Mass", y="DG", color ="Genotype" )) +
  geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
  #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
  theme(text=element_text(size=20)) +   scale_y_continuous("Degree of Connectivity") 
ggsave("DG_Mass.PNG", plot= last_plot(), device='png', 
       scale=1, width=8, 
       height=8, unit=c("in") )



###################


file_xlsx = "Clustering_Coefficient.xlsx"
sheets = excel_sheets(file_xlsx)

pvals= matrix(NA, 1, length(sheets) )
colnames(pvals) = sheets
for (i in 1:length(sheets)) {
temp= read_xlsx(file_xlsx, sheet = sheets[i] )
pvals[i] = unlist(temp[1,6])
}

for (i in 1:dim(pvals)[2]) {
  if (pvals[i] <= 0.05) {
    name = colnames(pvals)[i]

    results[, which(colnames(results)==name)] = as.numeric(results[, which(colnames(results)==name)])
    
    ggplot(results, aes_string(x=name, y="CC", color ="Genotype" )) + geom_point(aes_string(x=name, y="CC", color ="Genotype" )) +
      geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
      #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
      theme(text=element_text(size=20)) +   scale_y_continuous("Clustering coefficient") 
    ggsave( paste0("CC_", name, ".png") , plot= last_plot(), device='png', 
           scale=1, width=8, 
           height=8, unit=c("in") )
    
  }
}


###################


file_xlsx = "Path_Length.xlsx"
sheets = excel_sheets(file_xlsx)

pvals= matrix(NA, 1, length(sheets) )
colnames(pvals) = sheets
for (i in 1:length(sheets)) {
  temp= read_xlsx(file_xlsx, sheet = sheets[i] )
  pvals[i] = unlist(temp[1,6])
}

for (i in 1:dim(pvals)[2]) {
  if (pvals[i] <= 0.05) {
    name = colnames(pvals)[i]
    
    results[, which(colnames(results)==name)] = as.numeric(results[, which(colnames(results)==name)])
    
    ggplot(results, aes_string(x=name, y="PL", color ="Genotype" )) + geom_point(aes_string(x=name, y="PL", color ="Genotype" )) +
      geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
      #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
      theme(text=element_text(size=20)) +   scale_y_continuous("Path Length") 
    ggsave( paste0("PL_", name, ".png") , plot= last_plot(), device='png', 
            scale=1, width=8, 
            height=8, unit=c("in") )
    
  }
}

#########
###################


file_xlsx = "Betweenness.xlsx"
sheets = excel_sheets(file_xlsx)

pvals= matrix(NA, 1, length(sheets) )
colnames(pvals) = sheets
for (i in 1:length(sheets)) {
  temp= read_xlsx(file_xlsx, sheet = sheets[i] )
  pvals[i] = unlist(temp[1,6])
}

for (i in 1:dim(pvals)[2]) {
  if (pvals[i] <= 0.05) {
    name = colnames(pvals)[i]
    
    results[, which(colnames(results)==name)] = as.numeric(results[, which(colnames(results)==name)])
    
    ggplot(results, aes_string(x=name, y="BW", color ="Genotype" )) + geom_point(aes_string(x=name, y="BW", color ="Genotype" )) +
      geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
      #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
      theme(text=element_text(size=20)) +   scale_y_continuous("Betweenness") 
    ggsave( paste0("BW_", name, ".png") , plot= last_plot(), device='png', 
            scale=1, width=8, 
            height=8, unit=c("in") )
    
  }
}

#########

###################


file_xlsx = "Degree_of_Connectivity.xlsx"
sheets = excel_sheets(file_xlsx)

pvals= matrix(NA, 1, length(sheets) )
colnames(pvals) = sheets
for (i in 1:length(sheets)) {
  temp= read_xlsx(file_xlsx, sheet = sheets[i] )
  pvals[i] = unlist(temp[1,6])
}

for (i in 1:dim(pvals)[2]) {
  if (pvals[i] <= 0.05) {
    name = colnames(pvals)[i]
    
    results[, which(colnames(results)==name)] = as.numeric(results[, which(colnames(results)==name)])
    
    ggplot(results, aes_string(x=name, y="DG", color ="Genotype" )) + geom_point(aes_string(x=name, y="DG", color ="Genotype" )) +
      geom_blank() + theme_bw() +   geom_smooth(method='lm') + 
      #stat_compare_means(method = "t.test", comparisons = my_comparisons,  size = 5, label="p.signif")+
      theme(text=element_text(size=20)) +   scale_y_continuous("Degree of Connectivity") 
    ggsave( paste0("DG_", name, ".png") , plot= last_plot(), device='png', 
            scale=1, width=8, 
            height=8, unit=c("in") )
    
  }
}

#########




