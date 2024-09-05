library(dplyr)
library(pROC)
# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

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
riskfactors  = riskfactors[ , -c(6,7) ]
table(response$Genotype)
table(response$Diet)


##########no HN
# indeceis_apoe_no_hn= riskfactors$Genotype %in% c("APOE22", "APOE33", "APOE44", "APOE22HN", "APOE33HN", "APOE44HN")
# riskfactors=riskfactors[indeceis_apoe_no_hn,]
# riskfactors=riskfactors%>%dplyr::select(Sex, Age_Months)
dim(connectivity)
# connectivity = connectivity [ , , indeceis_apoe_no_hn , drop =T] 

HN_index <- grep("HN",riskfactors$Genotype)
riskfactors$HN <- 1
riskfactors$HN[HN_index] <- -1

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


# riskfactors1=riskfactors%>%select(Sex, Age_Months, Genotype, Diet)
#riskfactors2=riskfactors%>%select(NormSWDist, Distance, Winding)
# riskfactors2=riskfactors%>%select( NormSWDist, Winding)

# inddz1=0
# for (i in 1:dim(riskfactors)[2]) if(sd(riskfactors[,i])==0 ) {inddz1=rbind(inddz1,i);  cat ( i , sd(riskfactors[,i]), "\n" );}
# if (length(inddz1)>1){
#   inddz1=inddz1[2:dim(inddz1)[1]]
#   riskfactors=riskfactors[,-inddz1]
# }
# 


# 
# inddz2=0
# for (i in 1:dim(riskfactors2)[2]) if(sd(riskfactors2[,i])==0 ) {inddz2=rbind(inddz2,i);  cat ( i , sd(riskfactors2[,i]), "\n" );}
# if (length(inddz2)>1){
#   inddz2=inddz2[2:dim(inddz2)[1]]
#   riskfactors2=riskfactors2[,-inddz2]
# }

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
  indexlower=lower.tri(temp, diag=FALSE)
  temp=temp[indexlower]
  image[i,]=temp
}
dim(image)





# indd=0
# for (i in 1:dim(image)[2]) if(sd(image[,i])==0 ) {indd=rbind(indd,i);  cat ( i , sd(image[,i]), "\n" );}
# if (length(indd)>1){
#   indd=indd[2:dim(indd)[1]]
#   image=image[,-indd] }
# 
# 
# 
# 





#lets run
## Not run:
#install.packages("PMA")
#install.packages("https://gitlab.oit.duke.edu/am983/PMA2/-/archive/master/PMA2-master.tar.gz", repos = NULL, type="source")
library(PMA)
library(glmnet)
set.seed(3189) #for reproductivity
train_index <- sample(1:nrow(image), 0.8 * nrow(image))


alphas <- seq(0,1,by=0.05)

# for (i in 7:(dim(riskfactors)[2]-1)) {
#   
#   # i = 7 
#   
#   y <- riskfactors[,i]
#   
#   cv_results <- sapply(alphas, function(alpha) {
#     cv_fit <- cv.glmnet(image[train_index,], y[train_index], alpha = alpha, nfolds = 5)
#     min(cv_fit$cvm)
#   })
#   
#   best_alpha <- alphas[which.min(cv_results)]
#   
#   
#   cv_fit_final = cv.glmnet(image[train_index,], y[train_index], alpha = best_alpha, nfolds = 5)
#   
#   best_lambda <- cv_fit_final$lambda.min  
#   
#   final_model <- glmnet(image[train_index,], y[train_index], alpha = best_alpha, lambda = best_lambda)
#   length(coef(final_model))
#   
#   
# 
# final_model
# 
# y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index,])
# 
# # Calculate RMSE
# print(colnames(riskfactors)[i])
# print(rmse <- sqrt(mean((y[-train_index] - y_pred)^2)))
# print(sd(y[-train_index]))
#   
#   
# 
# }

# Genotype
# Response variable preparation
y <- riskfactors$Genotype
y[y != 4] <- -1
y[y == 4] <- 1
# Cross-validation to find the best alpha
cv_results <- sapply(alphas, function(alpha) {
  cv_fit <- cv.glmnet(image[train_index, ], y[train_index], alpha = alpha, nfolds = 5, family = "binomial")
  min(cv_fit$cvm)
})
# Best alpha and lambda selection
best_alpha <- alphas[which.min(cv_results)]
plot(y=cv_results, x= alphas)
cv_fit_final <- cv.glmnet(image[train_index, ], y[train_index], alpha = best_alpha, nfolds = 5, family = "binomial")
best_lambda <- cv_fit_final$lambda.min  
final_model <- glmnet(image[train_index, ], y[train_index], alpha = best_alpha, lambda = best_lambda, family = "binomial")
# Number of non-zero coefficients
length(coef(final_model))
# Final model
final_model
# Prediction on the test set
y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index, ], type = "response")
# Calculate AUC with 95% CI
roc_obj <- roc(y[-train_index], y_pred)
auc_value <- auc(roc_obj)
ci_95 <- ci.auc(roc_obj, conf.level = 0.95)
# Print the results
print(paste("AUC:", auc_value))
print(paste("95% CI:", ci_95))
# Plot the ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"), col = "blue", lwd = 2)



# Diet
# Response variable preparation
y <- riskfactors$Diet
# y[y != 4] <- -1
# y[y == 4] <- 1
# Cross-validation to find the best alpha
cv_results <- sapply(alphas, function(alpha) {
  cv_fit <- cv.glmnet(image[train_index, ], y[train_index], alpha = alpha, nfolds = 5, family = "binomial")
  min(cv_fit$cvm)
})
# Best alpha and lambda selection
best_alpha <- alphas[which.min(cv_results)]
cv_fit_final <- cv.glmnet(image[train_index, ], y[train_index], alpha = best_alpha, nfolds = 5, family = "binomial")
best_lambda <- cv_fit_final$lambda.min  
final_model <- glmnet(image[train_index, ], y[train_index], alpha = best_alpha, lambda = best_lambda, family = "binomial")
# Number of non-zero coefficients
length(coef(final_model))
# Final model
final_model
# Prediction on the test set
y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index, ], type = "response")
# Calculate AUC with 95% CI
roc_obj <- roc(y[-train_index], y_pred)
auc_value <- auc(roc_obj)
ci_95 <- ci.auc(roc_obj, conf.level = 0.95)
# Print the results
print(paste("AUC:", auc_value))
print(paste("95% CI:", ci_95))
# Plot the ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"), col = "blue", lwd = 2)





# Excercise
# Response variable preparation
y <- riskfactors$Exercise
# y[y != 4] <- -1
# y[y == 4] <- 1
# Cross-validation to find the best alpha
cv_results <- sapply(alphas, function(alpha) {
  cv_fit <- cv.glmnet(image[train_index, ], y[train_index], alpha = alpha, nfolds = 5, family = "binomial")
  min(cv_fit$cvm)
})
# Best alpha and lambda selection
best_alpha <- alphas[which.min(cv_results)]
cv_fit_final <- cv.glmnet(image[train_index, ], y[train_index], alpha = best_alpha, nfolds = 5, family = "binomial")
best_lambda <- cv_fit_final$lambda.min  
final_model <- glmnet(image[train_index, ], y[train_index], alpha = best_alpha, lambda = best_lambda, family = "binomial")
# Number of non-zero coefficients
length(coef(final_model))
# Final model
final_model
# Prediction on the test set
y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index, ], type = "response")
# Calculate AUC with 95% CI
roc_obj <- roc(y[-train_index], y_pred)
auc_value <- auc(roc_obj)
ci_95 <- ci.auc(roc_obj, conf.level = 0.95)
# Print the results
print(paste("AUC:", auc_value))
print(paste("95% CI:", ci_95))
# Plot the ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"), col = "blue", lwd = 2)


# Sex
# Response variable preparation
y <- riskfactors$Sex
# y[y != 4] <- -1
# y[y == 4] <- 1
# Cross-validation to find the best alpha
cv_results <- sapply(alphas, function(alpha) {
  cv_fit <- cv.glmnet(image[train_index, ], y[train_index], alpha = alpha, nfolds = 5, family = "binomial")
  min(cv_fit$cvm)
})
# Best alpha and lambda selection
best_alpha <- alphas[which.min(cv_results)]
cv_fit_final <- cv.glmnet(image[train_index, ], y[train_index], alpha = best_alpha, nfolds = 5, family = "binomial")
best_lambda <- cv_fit_final$lambda.min  
final_model <- glmnet(image[train_index, ], y[train_index], alpha = best_alpha, lambda = best_lambda, family = "binomial")
# Number of non-zero coefficients
length(coef(final_model))
# Final model
final_model
# Prediction on the test set
y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index, ], type = "response")
# Calculate AUC with 95% CI
roc_obj <- roc(y[-train_index], y_pred)
auc_value <- auc(roc_obj)
ci_95 <- ci.auc(roc_obj, conf.level = 0.95)
# Print the results
print(paste("AUC:", auc_value))
print(paste("95% CI:", ci_95))
# Plot the ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"), col = "blue", lwd = 2)


# HN
# Response variable preparation
y <- riskfactors$HN
# y[y != 4] <- -1
# y[y == 4] <- 1
# Cross-validation to find the best alpha
cv_results <- sapply(alphas, function(alpha) {
  cv_fit <- cv.glmnet(image[train_index, ], y[train_index], alpha = alpha, nfolds = 5, family = "binomial")
  min(cv_fit$cvm)
})
# Best alpha and lambda selection
best_alpha <- alphas[which.min(cv_results)]
cv_fit_final <- cv.glmnet(image[train_index, ], y[train_index], alpha = best_alpha, nfolds = 5, family = "binomial")
best_lambda <- cv_fit_final$lambda.min  
final_model <- glmnet(image[train_index, ], y[train_index], alpha = best_alpha, lambda = best_lambda, family = "binomial")
# Number of non-zero coefficients
length(coef(final_model))
# Final model
final_model
# Prediction on the test set
y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index, ], type = "response")
# Calculate AUC with 95% CI
roc_obj <- roc(y[-train_index], y_pred)
auc_value <- auc(roc_obj)
ci_95 <- ci.auc(roc_obj, conf.level = 0.95)
# Print the results
print(paste("AUC:", auc_value))
print(paste("95% CI:", ci_95))
# Plot the ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"), col = "blue", lwd = 2)



