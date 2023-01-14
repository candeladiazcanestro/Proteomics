#####This script is used for performing linear regression normalized by covariates

args = commandArgs(trailingOnly = TRUE)

olink_data_all<-read.csv(args[1], row.names = 1)
olink_data_metadata<-read.csv(args[2], row.names = 1)

Diabetes_HOMAIR<-lapply(colnames(olink_data_all), function(name){
  summary(lm(olink_data_metadata$deltaChest~olink_data_all[,name] + olink_data_metadata$deltaWeight + olink_data_metadata$deltaFat + olink_data_metadata$Age))$coefficients[2,c(1,4)]
})

Diabetes_HOMAIR <- data.frame(matrix(unlist(Diabetes_HOMAIR), nrow=length(Diabetes_HOMAIR), byrow=TRUE))
row.names(Diabetes_HOMAIR)<-colnames(olink_data_all)
colnames(Diabetes_HOMAIR)<-c("Estimation","p_value")
Diabetes_HOMAIR$p_adjust<-p.adjust(Diabetes_HOMAIR$p_value,method = "fdr")

write.csv(Diabetes_HOMAIR, "Linear_Regression_Result.csv")