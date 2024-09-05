

library(readxl)
library(dplyr)
library(DirectedClustering)
library(NetworkToolbox)


path_data='Full_Cardiac_Metrics_02082024_Exercise.csv'
data=read.csv(path_data )
data = as.data.frame(data)
data = as.data.frame(t(na.omit(t(data))))

master_path = 'MasterSheet_Experiments2021.xlsx'
master = read_xlsx(master_path, sheet = "18ABB11_readable02.22.22_BJ_Cor")


datatemp = master
#################
path_connec="/Users/ali/Desktop/Mar24/fmri_all_to_batch9_conn/time_ser/"
file_list=list.files(path_connec)
plain_index = grep("FC", file_list)


file_list = file_list[plain_index]


results = data[0,]
colnames = c(colnames(results), "CC", "PL","BW", "DG")
results=data.frame(matrix(vector(), 0, length(colnames),
                  dimnames=list(c(), colnames)), stringsAsFactors=F)

notfound=0
sum = 0 
CC = NA
for (i in 1:length(file_list)) {
  res = matrix(NA, 1, length(colnames))
  colnames(res) = colnames
  temp_index=which(datatemp$ARunno==(substr(file_list[i],4,12)))
  
  if (length(temp_index)==0) {temp_index=which(datatemp$Second_ARunno==(substr(file_list[i],1,9))) }
  
  
  if (length(temp_index)>0) { 
    
    cat(datatemp$Cardiac_ID[temp_index], " i=", i , "\n" )
    # if(gsub("_","-", datatemp$CIVMID[temp_index])  %in% gsub("\\:.*","",matcher$BadeID) ) 
    {
      # {cat("here", i); sum = sum+1}
      # index_matcher = which( gsub("_","-", datatemp$CIVMID[temp_index])  == matcher$BadeID)
      index_alex_al = which (datatemp$Cardiac_ID[temp_index] == gsub("-","_", data$ID ) )
      if (length(index_alex_al)>0){
        

        
        res = data[index_alex_al,]
        res$ID = datatemp$ARunno[temp_index]
        
        temp_conn = as.matrix(read.csv( paste0(path_connec,file_list[i]) , header = F ))
        temp_conn[is.na(temp_conn)] = 0  
        temp_conn = ( temp_conn + t(temp_conn) )/ 2
        # temp_conn[abs(temp_conn)<quantile(abs(temp_conn),0.1)]=0
        temp_conn[abs(temp_conn)<0.2]=0
        
        CC= clustcoeff(temp_conn)$CC 
        PL= pathlengths(temp_conn)$ASPL
        BW= mean(betweenness(temp_conn))
        DG=mean(degree(temp_conn))
        res$CC = CC
        res$PL = PL
        res$BW = BW
        res$DG = DG
        results =  rbind(results,res)
      }
    }
    
  }
  
  
  else {notfound=c(notfound, substr(file_list[i],1,9))} 
  
}



save(file="graph_prop.rda",results )

# 
# 
# notfound=0
# for (i in 1:length(file_list)) {
#   res = matrix(NA, 1, length(colnames))
#   colnames(res) = colnames
#   temp_index=which(datatemp$ARunno==(substr(file_list[i],4,12)))
#   
#   if (length(temp_index)==0) {temp_index=which(datatemp$Second_ARunno==(substr(file_list[i],1,9))) }
#   
#   
#   if (length(temp_index)>0) { 
#     
#     cat(datatemp$Cardiac_ID[temp_index], " i=", i , "\n" )
#     # if(gsub("_","-", datatemp$CIVMID[temp_index])  %in% gsub("\\:.*","",matcher$BadeID) ) 
#     {
#       # {cat("here", i); sum = sum+1}
#       # index_matcher = which( gsub("_","-", datatemp$CIVMID[temp_index])  == matcher$BadeID)
#       index_alex_al = which (datatemp$Cardiac_ID[temp_index] == gsub("-","_", data$ID ) )
#       if (length(index_alex_al)>0){
#         
#         
#         
#         res = data[index_alex_al,]
#         res$ID = datatemp$ARunno[temp_index]
#         
#         temp_conn = as.matrix(read.csv( paste0(path_connec,file_list[i]) , header = F ))
#         temp_conn[is.na(temp_conn)] = 0  
#         temp_conn = ( temp_conn + t(temp_conn) )/ 2
#         # temp_conn[abs(temp_conn)<quantile(abs(temp_conn),0.1)]=0
#         temp_conn[abs(temp_conn)<0.2]=0
#         
#         CC= clustcoeff(temp_conn)$CC 
#         PL= pathlengths(temp_conn)$ASPL
#         BW= mean(betweenness(temp_conn))
#         DG=mean(degree(temp_conn))
#         res$CC = CC
#         res$PL = PL
#         res$BW = BW
#         res$DG = DG
#         results =  rbind(results,res)
#       }
#     }
#     
#   }
#   
#   
#   else {notfound=c(notfound, substr(file_list[i],1,9))} 
#   
# }
# 
# save(file="graph_prop.rda",results )


