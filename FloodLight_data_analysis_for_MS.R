library(readxl)
setwd("/Users/shahvr/Documents/GSK/Biogen/Exercise/Part 2_Vrutang")
Data <- read.csv("complete_dataset.csv")
str(Data)

Data$participantIsControl<-factor(Data$participantIsControl)
summary(Data$participantIsControl)
library(plyr)
Data$Y<-mapvalues(Data$participantIsControl, from = c("True", "False"), to = c("0", "1"))

Summary_ID<-dplyr::count(Data,Data$floodlightOpenId)


### Exploreing#####
#Data_columnwise<-tidyr::spread(Data, key=testMetricCode, value=testResultMetricValue)



HC_data<-dplyr::filter(Data, (Y=="0"))
HC_n<-length(unique(HC_data$floodlightOpenId))
MS_data<-dplyr::filter(Data, (Y=="1"))
MS_n<-length(unique(MS_data$floodlightOpenId))

## Based on Lifespace
str(HC_data$testMetricName)
HC_LS_data<-dplyr::filter(HC_data,testMetricName=="Life Space Daily")
str(HC_LS_data$testResultMetricValue)
summary(HC_LS_data$testResultMetricValue)
HC_LS_n<-length(unique(HC_LS_data$floodlightOpenId))

MS_LS_data<-dplyr::filter(MS_data,testMetricName=="Life Space Daily")
summary(MS_LS_data$testResultMetricValue)
MS_LS_n<-length(unique(MS_LS_data$floodlightOpenId))

## No of turns & Turn Average
HC_T_data<-dplyr::filter(HC_data,testMetricName=="Turns")
summary(HC_T_data$testResultMetricValue)
HC_T_n<-length(unique(HC_T_data$floodlightOpenId))

MS_T_data<-dplyr::filter(MS_data,testMetricName=="Turns")
summary(MS_T_data$testResultMetricValue)
MS_T_n<-length(unique(MS_T_data$floodlightOpenId))


HC_TSA_data<-dplyr::filter(HC_data,testMetricName=="Turn Speed Average")
summary(HC_TSA_data$testResultMetricValue)
HC_TSA_n<-length(unique(HC_TSA_data$floodlightOpenId))

MS_TSA_data<-dplyr::filter(MS_data,testMetricName=="Turn Speed Average")
summary(MS_TSA_data$testResultMetricValue)
MS_TSA_n<-length(unique(MS_TSA_data$floodlightOpenId))



Turn_dataset<-data.frame("Turns"=0,"Turn_speed_avg"=100,"Y"=120,"SubjectID"='ABC')
Turn_dataset$SubjectID<-as.character(Turn_dataset$SubjectID)
T_data<-dplyr::filter(Data,testMetricName=="Turns")
Turn_dataset[1:nrow(T_data),1]<-T_data$testResultMetricValue

TSA_data<-dplyr::filter(Data,testMetricName=="Turn Speed Average")
Turn_dataset[1:nrow(T_data),2]<-TSA_data$testResultMetricValue
Turn_dataset[1:nrow(T_data),3]<-as.numeric(as.character(TSA_data$Y))
Turn_dataset$Y<-factor(Turn_dataset$Y)
Turn_dataset[1:nrow(T_data),4]<-as.character(TSA_data$floodlightOpenId)
Turn_dataset$SubjectID<-factor(Turn_dataset$SubjectID)

Turn_dataset_with_only_turns<-Turn_dataset[-which(Turn_dataset$Turns==0),]

Unique_id=unique(Turn_dataset_with_only_turns$SubjectID)

n=length(unique(Turn_dataset_with_only_turns$SubjectID))

Turn_dataset_with_only_turns_baseline_only<-Turn_dataset_with_only_turns[0,]

for (i in (1:n)){
  Turn_dataset_with_only_turns_baseline_only[i,]<-Turn_dataset_with_only_turns[Turn_dataset_with_only_turns$SubjectID==Unique_id[i],][1,]        
}


iid_example <- function(data, V){
  
  .cvFolds <- function(Y, V){  #Create CV folds (stratify by outcome)
    Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
    Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
    folds <- vector("list", length=V)
    for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}		
    return(folds)
  }
  .doFit <- function(v, folds, data){  #Train/test glm for each fold
    fit <- glm(Y~., data=data[-folds[[v]],], family=binomial)
    pred <- predict(fit, newdata=data[folds[[v]],], type="response")
    return(pred)
  }
  folds <- .cvFolds(Y=data$Y, V=V)  #Create folds
  predictions <- unlist(sapply(seq(V), .doFit, folds=folds, data=data))  #CV train/predict
  predictions[unlist(folds)] <- predictions  #Re-order pred values
  # Get CV AUC and confidence interval
  out <- ci.cvAUC(predictions=predictions, labels=data$Y, folds=folds, confidence=0.95)
  return(out)
}


library(cvAUC)
library(pROC)
AUC=matrix(nrow=2,ncol = 6)
List_m=list() 
for (i in (1:2)){
  Data_NA<-Turn_dataset_with_only_turns_baseline_only[,c(i,3)]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=out$cvAUC
  AUC[i,5]=out$ci[1]
  AUC[i,6]=out$ci[2]
}




HC_turn_data<-dplyr::filter(Turn_dataset_with_only_turns_baseline_only,Y=="0")
MS_turn_data<-dplyr::filter(Turn_dataset_with_only_turns_baseline_only,Y=="1")
t.test(HC_turn_data$Turn_speed_avg,MS_turn_data$Turn_speed_avg)
wilcox.test(HC_turn_data$Turn_speed_avg,MS_turn_data$Turn_speed_avg)




library(cvAUC)
library(pROC)
AUC=matrix(nrow=1,ncol = 6)
List_m=list() 
for (i in (1)){
  Data_NA<-Turn_dataset_with_only_turns_baseline_only[,c(i,2,3)]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=out$cvAUC
  AUC[i,5]=out$ci[1]
  AUC[i,6]=out$ci[2]
}

Turn_dataset_with_only_turns_baseline_only_reorder<-Turn_dataset_with_only_turns_baseline_only[order(Turn_dataset_with_only_turns_baseline_only$SubjectID),]





## Age Calculation


Data$testStartedAt<-as.Date(Data$testStartedAt, format = "%m/%d/%Y %H:%M:%OS")
Data$testEndedAt<-as.Date(Data$testEndedAt, format = "%m/%d/%Y %H:%M:%OS")
Data$TestDuraition<-Data$testEndedAt-Data$testStartedAt


Data$testStartedAtNewFormate <- as.POSIXct(Data$testStartedAt, format = "%m/%d/%Y %H:%M:%S")
Data$testStartedYear<-as.numeric(format(Data$testStartedAtNewFormate, format = "%Y"))
Data$Age<-Data$testStartedYear-Data$participantBirthYear


Age_dataset<-Data[,c('Age','participantSex','Y','floodlightOpenId','participantHeightCms','participantWeightLbs')]
colnames(Age_dataset)<-c("Age","Sex","Y","SubjectID","Height","Weight")
Unique_id=unique(Age_dataset$SubjectID)

n=length(unique(Age_dataset$SubjectID))

Age_dataset_baseline_only<-Age_dataset[0,]

for (i in (1:n)){
  Age_dataset_baseline_only[i,]<-Age_dataset[Age_dataset$SubjectID==Unique_id[i],][1,]        
}

### Filtering all age<20 years
Age_dataset_baseline_only<-Age_dataset_baseline_only[-which(Age_dataset_baseline_only$Age<20),]

summary(Age_dataset_baseline_only$Y)

library(cvAUC)
library(pROC)
AUC=matrix(nrow=1,ncol = 6)
List_m=list() 
for (i in (1)){
  Data_NA<-Age_dataset_baseline_only[,c(i,2,3,5,6)]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=1-out$cvAUC
  AUC[i,5]=1-out$ci[1]
  AUC[i,6]=1-out$ci[2]
}

library(ROCit)
measure <- measureit(class = GLM$y, score = GLM$fitted.values,
                     measure = c("ACC", "SENS", "SPEC","PPV", "NPV", "FSCR"))  ### Select row 54
Result=data.frame("threshold"=0,"Accuracy"=1, "Sensitivity"=1,"Specificity"=1,"PPV"=1,"NPV"=1,"F1"=1)
## For Age, Sex

Result[1,]<-c(measure$Cutoff[54],measure$ACC[54],measure$SENS[54],measure$SPEC[54],measure$PPV[54],measure$NPV[54],measure$FSCR[54])

## For Age, Sex, Height, Weight
nn=which(round(measure$Cutoff,2)==round(coords(ROC, x="best", input="threshold", methods="SpEqualSe")[[1]],2))[1]
Result[1,]<-c(measure$Cutoff[nn],measure$ACC[nn],measure$SENS[nn],measure$SPEC[nn],measure$PPV[nn],measure$NPV[nn],measure$FSCR[nn])

library("verification")
library(OptimalCutpoints)
coords(ROC, x="best", input="threshold", methods="SpEqualSe")

library(InformationValue)
optimalCutoff(GLM$y, GLM$fitted.values)[1]


library(caret)
confusionMatrix(GLM$fitted.values, GLM$y)





##
### Mood response
Mood_dataset<-data.frame("MoodResponse"=0,"Y"=120,"SubjectID"='ABC')
Mood_dataset$SubjectID<-as.character(Mood_dataset$SubjectID)
Mood_data<-dplyr::filter(Data,testMetricName=="Mood Response")
Mood_dataset[1:nrow(Mood_data),1]<-Mood_data$testResultMetricValue
Mood_dataset$Y<-factor(Mood_data$Y)
Mood_dataset[1:nrow(Mood_data),3]<-as.character(Mood_data$floodlightOpenId)
Mood_dataset$SubjectID<-factor(Mood_dataset$SubjectID)


Unique_id=unique(Mood_dataset$SubjectID)

m=length(unique(Mood_dataset$SubjectID))

Mood_dataset_baseline_only<-Mood_dataset[0,]

for (i in (1:m)){
  Mood_dataset_baseline_only[i,]<-Mood_dataset[Mood_dataset$SubjectID==Unique_id[i],][1,]        
}

Age_dataset_baseline_only$MoodResponse="NA"
for (i in (1:n)){
  if (length(which(as.character(Mood_dataset_baseline_only$SubjectID)==as.character(Age_dataset_baseline_only$SubjectID[i])))>0){
    Age_dataset_baseline_only$MoodResponse[i]<-Mood_dataset_baseline_only[which(as.character(Mood_dataset_baseline_only$SubjectID)==as.character(Age_dataset_baseline_only$SubjectID[i])),][[1]]
  }
}

Age_dataset_baseline_only$MoodResponse<-as.numeric(Age_dataset_baseline_only$MoodResponse)
library(cvAUC)
library(pROC)
AUC=matrix(nrow=1,ncol = 6)
List_m=list() 
for (i in (1)){
  Data_NA<-Age_dataset_baseline_only[,c(i,2,3,5,6,7)]
  Data_NA<-Data_NA[complete.cases(Data_NA),]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=1-out$cvAUC
  AUC[i,5]=1-out$ci[1]
  AUC[i,6]=1-out$ci[2]
}

library(ROCit)
measure <- measureit(class = GLM$y, score = GLM$fitted.values,
                     measure = c("ACC", "SENS", "SPEC","PPV", "NPV", "FSCR"))  ### Select row 54
Result=data.frame("threshold"=0,"Accuracy"=1, "Sensitivity"=1,"Specificity"=1,"PPV"=1,"NPV"=1,"F1"=1)
# ## For Age, Sex
# 
# Result[1,]<-c(measure$Cutoff[54],measure$ACC[54],measure$SENS[54],measure$SPEC[54],measure$PPV[54],measure$NPV[54],measure$FSCR[54])

## For Age, Sex, Height, Weight
nn=which(round(measure$Cutoff,2)==round(coords(ROC, x="best", input="threshold", methods="SpEqualSe")[[1]],2))[1]
Result[1,]<-c(measure$Cutoff[nn],measure$ACC[nn],measure$SENS[nn],measure$SPEC[nn],measure$PPV[nn],measure$NPV[nn],measure$FSCR[nn])



### Drawing response

Drawing_data<-dplyr::filter(Data,testName=="Draw A Shape")
summary_count<-count(Drawing_data,floodlightOpenId)
summary_count_filer<-summary_count[which(summary_count$n>8),]
summary_count_testMetricCode<-count(Drawing_data, testMetricCode)
Drawing_dataset<-data.frame("A"=0,"B"=0,"C"=0,"D"=0,"E"=0,"F"=0,"G"=0,"H"=0,"j"=0)
colnames(Drawing_dataset)<-as.character(summary_count_testMetricCode$testMetricCode)
Drawing_dataset[1:length(summary_count_filer$floodlightOpenId),]<-NA
#Drawing_dataset$SubjectID<-summary_count_filer$floodlightOpenId

ii=length(summary_count_filer$floodlightOpenId)
jj=length(summary_count_testMetricCode$testMetricCode)
for (i in (1:ii)){
  for (j in (1:jj)){
    if (length(which(as.character(Drawing_data$floodlightOpenId)==as.character(summary_count_filer$floodlightOpenId[i]) & as.character(Drawing_data$testMetricCode)==as.character(summary_count_testMetricCode$testMetricCode[j])))>0){
      Drawing_dataset[i,j]<-Drawing_data$testResultMetricValue[which(as.character(Drawing_data$floodlightOpenId)==as.character(summary_count_filer$floodlightOpenId[i]) & as.character(Drawing_data$testMetricCode)==as.character(summary_count_testMetricCode$testMetricCode[j]))][1]
    }
  }
}

Drawing_dataset$SubjectID<-summary_count_filer$floodlightOpenId
Drawing_dataset$hand_used<-as.factor(Drawing_dataset$hand_used)
n=length(Age_dataset_baseline_only$SubjectID)
for (i in (1:n)){
  if (length(which(as.character(Drawing_dataset$SubjectID)==as.character(Age_dataset_baseline_only$SubjectID[i])))>0){
    Age_dataset_baseline_only[i,8:17]<-Drawing_dataset[which(as.character(Drawing_dataset$SubjectID)==as.character(Age_dataset_baseline_only$SubjectID[i])),]
  }
}

library(cvAUC)
library(pROC)
AUC=matrix(nrow=1,ncol = 6)
List_m=list() 
for (i in (1)){
  Data_NA<-Age_dataset_baseline_only[,c(i,2,3,5:16)]
  Data_NA<-Data_NA[complete.cases(Data_NA),]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=1-out$cvAUC
  AUC[i,5]=1-out$ci[1]
  AUC[i,6]=1-out$ci[2]
}

library(ROCit)
measure <- measureit(class = GLM$y, score = GLM$fitted.values,
                     measure = c("ACC", "SENS", "SPEC","PPV", "NPV", "FSCR"))  ### Select row 54
Result=data.frame("threshold"=0,"Accuracy"=1, "Sensitivity"=1,"Specificity"=1,"PPV"=1,"NPV"=1,"F1"=1)
# ## For Age, Sex
# 
# Result[1,]<-c(measure$Cutoff[54],measure$ACC[54],measure$SENS[54],measure$SPEC[54],measure$PPV[54],measure$NPV[54],measure$FSCR[54])

## For Age, Sex, Height, Weight
nn=which(round(measure$Cutoff,2)==round(coords(ROC, x="best", input="threshold", methods="SpEqualSe")[[1]],2))[1]
Result[1,]<-c(measure$Cutoff[nn],measure$ACC[nn],measure$SENS[nn],measure$SPEC[nn],measure$PPV[nn],measure$NPV[nn],measure$FSCR[nn])



### Adding No. of steps 

Walk_data<-dplyr::filter(Data,testMetricName=="Steps")
Walk_dataset<-data.frame("Steps"=0,"Y"=120,"SubjectID"='ABC')
Walk_dataset$SubjectID<-as.character(Walk_dataset$SubjectID)
Walk_dataset[1:nrow(Walk_data),1]<-Walk_data$testResultMetricValue
Walk_dataset$Y<-factor(Walk_data$Y)
Walk_dataset[1:nrow(Walk_data),3]<-as.character(Walk_data$floodlightOpenId)
Walk_dataset$SubjectID<-factor(Walk_dataset$SubjectID)


Unique_id=unique(Walk_dataset$SubjectID)

m=length(unique(Walk_dataset$SubjectID))

Walk_dataset_baseline_only<-Walk_dataset[0,]

for (i in (1:m)){
  Walk_dataset_baseline_only[i,]<-Walk_dataset[Walk_dataset$SubjectID==Unique_id[i],][1,]        
}

Age_dataset_baseline_only$Steps=NA
n=length(Age_dataset_baseline_only$SubjectID)
for (i in (1:n)){
  if (length(which(as.character(Walk_dataset_baseline_only$SubjectID)==as.character(Age_dataset_baseline_only$SubjectID[i])))>0){
    Age_dataset_baseline_only$Steps[i]<-Walk_dataset_baseline_only[which(as.character(Walk_dataset_baseline_only$SubjectID)==as.character(Age_dataset_baseline_only$SubjectID[i])),][[1]]
  }
}
library(cvAUC)
library(pROC)
AUC=matrix(nrow=1,ncol = 6)
List_m=list() 
for (i in (1)){
  Data_NA<-Age_dataset_baseline_only[,c(i,2,3,5:16,18)]
  Data_NA<-Data_NA[complete.cases(Data_NA),]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=1-out$cvAUC
  AUC[i,5]=1-out$ci[1]
  AUC[i,6]=1-out$ci[2]
}

library(ROCit)
measure <- measureit(class = GLM$y, score = GLM$fitted.values,
                     measure = c("ACC", "SENS", "SPEC","PPV", "NPV", "FSCR"))  ### Select row 54
Result=data.frame("threshold"=0,"Accuracy"=1, "Sensitivity"=1,"Specificity"=1,"PPV"=1,"NPV"=1,"F1"=1)
# ## For Age, Sex
# 
# Result[1,]<-c(measure$Cutoff[54],measure$ACC[54],measure$SENS[54],measure$SPEC[54],measure$PPV[54],measure$NPV[54],measure$FSCR[54])

## For Age, Sex, Height, Weight
nn=which(round(measure$Cutoff,2)==round(coords(ROC, x="best", input="threshold", methods="SpEqualSe")[[1]],2))[1]
Result[1,]<-c(measure$Cutoff[nn],measure$ACC[nn],measure$SENS[nn],measure$SPEC[nn],measure$PPV[nn],measure$NPV[nn],measure$FSCR[nn])

### Only selected

library(cvAUC)
library(pROC)
AUC=matrix(nrow=3,ncol = 6)
List_m=list() 
for (i in (3)){
  Data_NA<-Age_dataset_baseline_only[,c(i,1,2,6,7,10,13,14,18)]
  Data_NA<-Data_NA[complete.cases(Data_NA),]
  Data_NA<-Data_NA[-which(Data_NA$Steps==0),]
  GLM <- glm(Y~., family=binomial(logit), data=Data_NA)
  ROC<-roc(GLM$y , GLM$fitted.values,direction="<")
  set.seed(1)
  AUC[i,1]=ROC$auc
  AUC[i,2]=ci.auc(ROC,method='bootstrap')[1]
  AUC[i,3]=ci.auc(ROC,method='bootstrap')[3]
  out <- iid_example(data=Data_NA, V=5) 
  AUC[i,4]=1-out$cvAUC
  AUC[i,5]=1-out$ci[1]
  AUC[i,6]=1-out$ci[2]
}

library(ROCit)
measure <- measureit(class = GLM$y, score = GLM$fitted.values,
                     measure = c("ACC", "SENS", "SPEC","PPV", "NPV", "FSCR"))  ### Select row 54
Result=data.frame("threshold"=0,"Accuracy"=1, "Sensitivity"=1,"Specificity"=1,"PPV"=1,"NPV"=1,"F1"=1)
# ## For Age, Sex
# 
# Result[1,]<-c(measure$Cutoff[54],measure$ACC[54],measure$SENS[54],measure$SPEC[54],measure$PPV[54],measure$NPV[54],measure$FSCR[54])

## For Age, Sex, Height, Weight
nn=which(round(measure$Cutoff,2)==round(coords(ROC, x="best", input="threshold", methods="SpEqualSe")[[1]],2))[1]
Result[1,]<-c(measure$Cutoff[nn],measure$ACC[nn],measure$SENS[nn],measure$SPEC[nn],measure$PPV[nn],measure$NPV[nn],measure$FSCR[nn])

HC_Steps<-dplyr::filter(Data_NA,Y=="0")
MS_Steps<-dplyr::filter(Data_NA,Y=="1")

t.test(HC_Steps$Steps,MS_Steps$Steps)
wilcox.test(HC_Steps$Steps,MS_Steps$Steps)
mean(HC_Steps$Steps)
sd(HC_Steps$Steps)
mean(MS_Steps$Steps)
sd(MS_Steps$Steps)
par(mfrow=c(2,1))
hist(HC_Steps$Steps)
hist(MS_Steps$Steps)




