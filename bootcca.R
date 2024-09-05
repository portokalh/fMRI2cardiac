  
  boootcca=function(x=image, z=riskfactors, 
                    nsim=1000, alpha=0.05, samplesize=dim(image)[1] )
  {
    xorig=x;
    zorig=z;
    uboots=matrix(NA,dim(xorig)[2], nsim)
    vboots=matrix(NA,dim(zorig)[2], nsim)
    corboots=matrix(NA, nsim,1)
  
    for (ii in 1:nsim) {
      
      bootind=sample(dim(image)[1], replace = TRUE)
      x=xorig[bootind,];z=zorig[bootind,];
      
      bootindu=0
      for (i in 1:dim(x)[2]) if(sd(x[,i])==0 ) {bootindu=rbind(bootindu,i);}
      if (length(bootindu)>1){
        bootindu=bootindu[2:dim(bootindu)[1]]
        x=x[,-bootindu] }
      
      
      bootindv=0
      for (i in 1:dim(z)[2]) if(sd(z[,i])==0 ) {bootindv=rbind(bootindv,i);}
      if (length(bootindv)>1){
        bootindv=bootindv[2:dim(bootindv)[1]]
        z=z[,-bootindv] }
      
      
      outemp <- CCA(x=x,z=z, typex="standard",typez="standard")
      corboots[ii]=outemp$cors
      cat('number', ii, " ",corboots[ii]  ,  '\n' )
      
      uouttemp=uboots[,ii]
      if(sum(bootindu)>0){
      uouttemp[bootindu]=0
      uouttemp[-bootindu]=outemp$u}
      else {  uouttemp =outemp$u }
      uboots[,ii]=uouttemp
      
      vouttemp=vboots[,ii]
      if(sum(bootindv)>0){
      vouttemp[bootindv]=0
      vouttemp[-bootindv]=outemp$v}
      else {  vouttemp= outemp$v}
      vboots[,ii]=vouttemp
      
    }
    
    lowerconfcor= quantile(corboots,  alpha/2)
    higherconfcor=quantile(corboots,  1-alpha/2)
   
  returnlist=list( 'corboots'=corboots,  'uboots'=uboots, 
                   'vboots'=vboots, 'lowerconfcor'=lowerconfcor,
                  'higherconfcor'=higherconfcor )
  return(returnlist)
    
  }
  
  
  
  
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
  riskfactors = riskfactors %>% select(-c( "DOB", "CT.Date"))
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
  
  
  
  
  ######
  
  # 
  # #lets run
  # ## Not run:
  # #install.packages("PMA")
  # #install.packages("https://gitlab.oit.duke.edu/am983/PMA2/-/archive/master/PMA2-master.tar.gz", repos = NULL, type="source")
  # library(PMA)
  # 
  # set.seed(3189) #for reproductivity
  # 
  # # Can run CCA with default settings, and can get e.g. 3 components
  # #??glmnet
  # 
  # #errors in this loop
  # # this is selecing a lambda for traits so it would penalize the most without sparisity 
  # for (i in 100:1) {
  #   zlamb=i/100
  #   out <- CCA(x=image,z=riskfactors,typex="standard",typez="standard", penaltyz = zlamb)
  #   numzerv=sum(out$v==0)
  #   #if (numzerv>0.6*length(out$v) ) { i=i+1;  zlamb=i/100;  break }
  #   if (numzerv>0 ) { i=i+1;  zlamb=i/100;  break }
  # }
  # 
  # numzeru=sum(out$u!=0)
  # xlamb=out$penaltyx
  # sparsity=0.95
  # persnonsparse=floor((1-sparsity)*length(out$u))
  # while (numzeru>persnonsparse) {
  #   xlamb=0.7*xlamb
  #   out2 <- CCA(x=image,z=riskfactors,typex="standard",typez="standard", penaltyz = zlamb, penaltyx = xlamb)
  #   numzeru=sum(out2$u!=0); 
  # }
  # 
  # for (i in 100:1) {
  #   zlamb=i/100
  #   out <- CCA(x=image,z=riskfactors,typex="standard",typez="standard", penaltyz = zlamb, penaltyx = xlamb)
  #   numzerv=sum(out$v==0)
  #   if (numzerv>0 ) { i=i+1;  zlamb=i/100;  break }
  # }
  # 
  # out <- CCA(x=image,z=riskfactors,typex="standard",typez="standard", penaltyz = zlamb, penaltyx = xlamb)
  # #out <- CCA(x=image,z=riskfactors,typex="standard",typez="standard")
  # 
  # 
  # 
  # 
  # print(out) # could do print(out,verbose=TRUE)
  # #print(image[out$u!=0]) 
  # 
  # 
  
  
  #######
  library(PMA)
  # boot=boootcca(x=image, z=riskfactors,
  # nsim=1000, alpha=0.05, samplesize=dim(image)[1] )
  # save(boot, file = "boot.rda")
  
  load("boot.rda")
  
  boot$lowerconfcor
  boot$higherconfcor
  
  
  #corboots: 1000 bootstrap correlations
  #uboots: weights for connectomes
  #vboots: weights for risk factors
  boot$uboots
  
  #threshold by quantiles
  zeroone = boot$uboots
  zeroone[ boot$uboots!=0] = 1
  zeroone_sum =  apply(zeroone , 1, sum )
  # quantile= 0.95
  # aaa=sum(aaa)
  # zeroone_sum [zeroone_sum<=quantile(zeroone_sum,quantile) ]=0
  # zeroone_sum[zeroone_sum!=0] = 1 
  
  #threshold by frequency
  quantile=0.99
  zeroone_sum [zeroone_sum<=900]=0
  zeroone_sum[zeroone_sum!=0] = 1 
  
  # zeroone_sum_thresh[zeroone_sum_thresh<quantile(zeroone_sum_thresh,quantile)]=0
  # zeroone_sum_thresh[zeroone_sum_thresh!=0]
  
  
  
  average_edge= apply(boot$uboots , 1, mean )
  
  average_edge = average_edge*zeroone_sum 
  
  sum(average_edge!=0)
  
  
  v_average  = apply(boot$vboots, 1, mean)
  paste0(colnames(riskfactors), " ", v_average)
  
  
  mean(boot$corboots)
  
  
  
  # summary stats:
  
  table(riskfactors$Genotype,riskfactors$Sex  )
  table(riskfactors$Exercise,riskfactors$Sex  )
  table(riskfactors$Diet,riskfactors$Sex  )
  