library(xlsx)
load("graph_prop.rda")

##### within ctrl compare excercise and sedentary

# response3 = results[results$Diet=="CTRL",]
response3 = results
response3$Genotype = gsub("2HN","22", response3$Genotype)
response3$Genotype = gsub("3HN","33", response3$Genotype)
response3$Genotype = gsub("4HN","44", response3$Genotype)

colnames(response3)[colnames(response3)=="CC"] = "Clustering_Coefficient"
colnames(response3)[colnames(response3)=="PL"] = "Path_Length"
colnames(response3)[colnames(response3)=="BW"] = "Betweenness"
colnames(response3)[colnames(response3)=="DG"] = "Degree_of_Connectivity"


# response3$Lifestyle [response3$Lifestyle=="Sedentary/Control"]  = "Sedentary"

for (i in 11:(dim(response3)[2]-4)) {
  name = colnames(response3)[i]
  cat(i)
  lm = lm( Clustering_Coefficient  ~  as.numeric(get(name))+as.numeric(Age)+as.factor(Genotype)+as.factor(Diet)+as.factor(Sex)+as.numeric(Mass), response3 )
  # lm = lm(get(name)  ~ Clustering_Coefficient*Path_Length*Betweenness*Degree_of_Connectivity, response3 )
  temp = anova(lm)
  rownames(temp) = gsub("as.numeric","", rownames(temp) )
  rownames(temp) = gsub("as.factor","", rownames(temp) )
  rownames(temp) = gsub('\\(',"", rownames(temp) )
  rownames(temp) = gsub('\\)',"", rownames(temp) )
  rownames(temp)[rownames(temp) =="getname"] = name
  # anova(lm)
  write.xlsx(file = "Clustering_Coefficient.xlsx",  temp,  sheetName = paste0(name) , append = T)  
}


for (i in 11:(dim(response3)[2]-4)) {
  name = colnames(response3)[i]
  cat(i)
  lm = lm( Path_Length ~as.numeric(get(name)) +as.numeric(Age)+as.factor(Genotype)+as.factor(Diet)+as.factor(Sex)+as.numeric(Mass), response3 )
  # lm = lm(get(name)  ~ Clustering_Coefficient*Path_Length*Betweenness*Degree_of_Connectivity, response3 )
  temp = anova(lm)
  rownames(temp) = gsub("as.numeric","", rownames(temp) )
  rownames(temp) = gsub("as.factor","", rownames(temp) )
  rownames(temp) = gsub('\\(',"", rownames(temp) )
  rownames(temp) = gsub('\\)',"", rownames(temp) )
  rownames(temp)[rownames(temp) =="getname"] = name
  
  # anova(lm)
  write.xlsx(file = "Path_Length.xlsx",  temp,  sheetName = paste0(name) , append = T)  
}


for (i in 11:(dim(response3)[2]-4)) {
  name = colnames(response3)[i]
  cat(i)
  lm = lm( Betweenness ~as.numeric(get(name)) +as.numeric(Age)+as.factor(Genotype)+as.factor(Diet)+as.factor(Sex)+as.numeric(Mass), response3 )
  # lm = lm(get(name)  ~ Clustering_Coefficient*Path_Length*Betweenness*Degree_of_Connectivity, response3 )
  temp = anova(lm)
  rownames(temp) = gsub("as.numeric","", rownames(temp) )
  rownames(temp) = gsub("as.factor","", rownames(temp) )
  rownames(temp) = gsub('\\(',"", rownames(temp) )
  rownames(temp) = gsub('\\)',"", rownames(temp) )
  rownames(temp)[rownames(temp) =="getname"] = name
  # anova(lm)
  write.xlsx(file = "Betweenness.xlsx", temp,  sheetName = paste0(name) , append = T)  
}


for (i in 11:(dim(response3)[2]-4)) {
  name = colnames(response3)[i]
  cat(i)
  lm = lm(Degree_of_Connectivity ~as.numeric(get(name)) +as.numeric(Age)+as.factor(Genotype)+as.factor(Diet)+as.factor(Sex)+as.numeric(Mass), response3 )
  # lm = lm(get(name)  ~ Clustering_Coefficient*Path_Length*Betweenness*Degree_of_Connectivity, response3 )
  temp = anova(lm)
  rownames(temp) = gsub("as.numeric","", rownames(temp) )
  rownames(temp) = gsub("as.factor","", rownames(temp) )
  rownames(temp) = gsub('\\(',"", rownames(temp) )
  rownames(temp) = gsub('\\)',"", rownames(temp) )
  rownames(temp)[rownames(temp) =="getname"] = name
  write.xlsx(file = "Degree_of_Connectivity.xlsx",  temp,  sheetName = paste0(name) , append = T)  
}
