library(dplyr)
library(brainconn2)

library(ggplot2)
library(ggpubr)

noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

load(file='connectivity.rda')
load(file='response.rda')



not_in_Atlas = c(23, 86, 139, 150, 189, 274, 296, 316)
present_rois = setdiff(1:332, not_in_Atlas)


connectivity_new = array(0,c(332 , 332, dim(connectivity)[3]))

for (i in 1:dim(connectivity)[3]) {
  connectivity_new[present_rois,present_rois,i] = connectivity[,,i]
}
connectivity = connectivity_new

not_in_Atlas_symm = -1
for (i in not_in_Atlas) {
  if ( i + 166 <=332) {if (!(i+166)%in% not_in_Atlas) { not_in_Atlas_symm = c(not_in_Atlas_symm, i+166)}  } 
  if ( i - 166 >=0) {if (!(i-166)%in% not_in_Atlas) { not_in_Atlas_symm = c(not_in_Atlas_symm, i-166) }  } 
}
not_in_Atlas_symm = not_in_Atlas_symm[-1]

for (i in 1:dim(connectivity)[3]) {
  connectivity[not_in_Atlas_symm,,i] = 0
  connectivity[,not_in_Atlas_symm,i] = 0
}



# temp=connectivity[-noreadcsf,-noreadcsf,1]
temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  



#riskfactors=matrix(NA,  dim(response)[1], (dim(response)[2]-1))
riskfactors=response [ , 2:(dim(response)[2]-1) ]
# riskfactors = riskfactors %>% select(-c( "DOB", "CT.Date"))
riskfactors <- riskfactors[,-c(6,7)]
table(response$Genotype)
table(response$Diet)


##########no HN
# indeceis_apoe_no_hn= riskfactors$Genotype %in% c("APOE22", "APOE33", "APOE44", "APOE22HN", "APOE33HN", "APOE44HN")
# riskfactors=riskfactors[indeceis_apoe_no_hn,]
# riskfactors=riskfactors%>%dplyr::select(Sex, Age_Months)
dim(connectivity)
# connectivity = connectivity [ , , indeceis_apoe_no_hn , drop =T] 


Gene=riskfactors$Genotype
Gene[Gene=="E22"]=2
Gene[Gene=="E33"]=3
Gene[Gene=="E44"]=4
Gene[Gene=="E2HN"]=2
Gene[Gene=="E3HN"]=3
Gene[Gene=="E4HN"]=4
Gene[Gene=="KO"]=0
riskfactors$Genotype=as.numeric(Gene)

Sex=riskfactors$Sex
Sex[Sex=="Male"]=-1
Sex[Sex=="Female"]=1
riskfactors$Sex=as.numeric(Sex)

Diet=riskfactors$Diet
Diet[Diet=="CTRL"]=-1
Diet[Diet=="HFD"]=1
riskfactors$Diet=as.numeric(Diet)



Exercise=riskfactors$Exercise
Exercise[Exercise=="NO"]=-1
Exercise[Exercise=="YES"]=1
riskfactors$Exercise=as.numeric(Exercise)


riskfactors$Age=as.numeric(riskfactors$Age)

riskfactors=as.data.frame(riskfactors)
riskfactors[] <- lapply(riskfactors, as.numeric)
class(riskfactors)


riskfactors_orig = riskfactors






inddz1=0
for (i in 1:dim(riskfactors)[2]) if(sd(riskfactors[,i])==0 ) {inddz1=rbind(inddz1,i);  cat ( i , sd(riskfactors[,i]), "\n" );}
if (length(inddz1)>1){
  inddz1=inddz1[2:dim(inddz1)[1]]
  riskfactors=riskfactors[,-inddz1]
}





# temp=connectivity[-noreadcsf,-noreadcsf,1]
temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  

image=matrix(NA,  dim(connectivity)[3], len) # -6 becasue of cfs removal

for (i in 1:dim(connectivity)[3]){
  # temp=connectivity[-noreadcsf,-noreadcsf,i]
  temp=connectivity[,,i]
  # print(sum( abs( temp[148,])))
  indexlower=lower.tri(temp, diag=FALSE)
  temp=temp[indexlower]
  temp[abs(temp)<0.30]=0
  image[i,]=temp
  # print(sum( abs( image[148,])))
}
dim(image)





indd=0
for (i in 1:dim(image)[2]) if(sd(image[,i])==0 ) {indd=rbind(indd,i);  cat ( i , sd(image[,i]), "\n" );}
if (length(indd)>1){
  indd=indd[2:dim(indd)[1]]
  image=image[,-indd] }



############### load boot cca

load("boot.rda")


zeroone = boot$uboots
zeroone[ boot$uboots!=0] = 1
zeroone_sum =  apply(zeroone , 1, sum )

zeroone_sum [zeroone_sum<=900]=0
zeroone_sum[zeroone_sum!=0] = 1 


average_edge= apply(boot$uboots , 1, mean )

average_edge = average_edge*zeroone_sum 

sum(average_edge!=0)


v_average  = apply(boot$vboots, 1, mean)



##############

u = as.matrix(average_edge)



v = v_average

sum(u==0)
#len=length(u)
sum(u!=0)
u[u!=0]
sum(u==0)/len #sparsity 

uout=matrix(NA, 332*331/2,1 )
#put those zeros back
uout[indd]=0
# uout=u
uout[-indd]=u

connectivityexample=connectivity[,,1]

connectivityexample[indexlowertrue[indd]] ##yes the're them
connectivityexample[indexlowertrue[indd]]="zeros" # lest make them known for a special word
indexofzeros=which(connectivityexample=="zeros", arr.ind=TRUE)

#results of connectivities that matter:
nonzeroindex=which(uout!=0)

connectivityexample=connectivity[,,1]
connectivityexample[]=0
connectivitvals=connectivityexample
nonzerouout=uout[uout!=0]
for (i in 1:length(nonzeroindex)) {
  connectivityexample[indexlowertrue[nonzeroindex[i]]]=c("nonzero") # lest make them known for a special word
  connectivitvals[indexlowertrue[nonzeroindex[i]]]=nonzerouout[i] #store their coefitient values
}


sum(connectivitvals!=0)



library('igraph');
connectivitvalsones=connectivitvals
#####
diag(connectivitvals)=0


dim(connectivitvals)
sum(connectivitvals!=0)



#####
t=which(connectivitvalsones!=0, arr.ind=TRUE)
t <- cbind(t, connectivitvals[which(connectivitvals!=0,arr.ind=TRUE)]) 
t.graph=graph.data.frame(t,directed=F)
E(t.graph)$color <- ifelse(E(t.graph)$V3 > 0,'blue','red') 
#t.names <- colnames(cor.matrix)[as.numeric(V(t.graph)$name)]
minC <- rep(-Inf, vcount(t.graph))
maxC <- rep(Inf, vcount(t.graph))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(t.graph, minx=minC, maxx=maxC,
                    miny=minC, maxy=maxC)      

pathnames='mouse_anatomy.csv'
datanmes=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
datanmes$ROI

# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

#datanmes=datanmes[-noreadcsf]

# datanmess=datanmes$ROI[-noreadcsf] # remove csf
datanmess=datanmes$ROI





connectivitvals=connectivitvals+t(connectivitvals) #symetric


plot = brainconn(atlas ="CHASS", conmat=connectivitvals, 
                 view="top", node.size =4, 
                 node.color = "black", 
                 edge.width = 8, edge.color="red", 
                 edge.alpha = 0.65,
                 edge.color.weighted = T,
                 scale.edge.width=T,
                 labels = T,
                 all.nodes =F, 
                 show.legend = T, 
                 label.size=14, background.alpha=1, 
                 label.edge.weight=F,background = "Chass")  

library(ggplot2)
ggsave(paste0("glasstop_",sum(u!=0) ,".png"), plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)






subnets=groups(components(t.graph))
subnetsresults=vector(mode = "list", length = length(subnets))
colsumabs=colSums(abs(connectivitvals))
colsum=colSums(connectivitvals)

leftright=datanmes$Hemisphere




####################3
for (i in 1:length(subnets)) {
  temp=subnets[[i]]
  temp=as.numeric(temp)
  net=matrix(NA,8,length(temp) )
  net[1,]=as.numeric(temp)
  tt=as.numeric(net[1,])
  #tt=c(1,200)
  #indofleftright=tt>=164
  #net[5,][indofleftright]="Right"
  #net[5,][!indofleftright]="Left"
  
  
  net[2,]=datanmess[temp]
  net[5,]=leftright[temp]
  net[1,]=paste(net[1,],net[5,])
  net[3,]= as.numeric( colsum[temp]   )
  net[4,]= as.numeric( colsumabs[temp]   )
  net[6,]=sum(as.numeric(net[4,]))
  net[7,]=sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex=which(datanmes$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]]=net 
}

##############new net

net_new=matrix(NA, length(subnetsresults),4)


for (j in 1:dim(net_new)[1]) {
  temps=subnetsresults[[j]]
  net_new[j,1]=j
  net_new[j,2]= paste(temps[8,], collapse = ", ")
  net_new[j,3] = paste(paste(temps[5,],temps[2,]), collapse = ", ")
  net_new[j,4] = paste(temps[7,1])
  
}
colnames(net_new)=c("Sub-Network", "Region Number", "Region Name", "Sub-Network Weight")


#install.packages("xlsx")
library(xlsx)


for (i in 1:length(subnetsresults)){
  net=t(subnetsresults[[i]])
  # write.xlsx2(net, "nets.xlsx", sheetName =  paste0(i), append=TRUE )
}

write.xlsx2(net_new, paste0(sum(u!=0),"_net_new.xlsx") )

