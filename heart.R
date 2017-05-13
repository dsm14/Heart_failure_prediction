getwd()
setwd("C:\\Users\\madhurima\\Downloads")

library(xlsx)
library(rJava)
ht <- read.xlsx("ht.xlsx", sheetName="a1")

## Data Assessment##
str(ht)
summary(ht)
colSums(is.na(ht))

##Data Preparation##
# No data preparation required as all variables are in numeric form#


##Sampling##
#set.seed(200)
#index<-sample(nrow(ht), 0.70*nrow(ht), replace=F)
#train<-ht[index,]
#test<-ht[-index,]

##Logistic Regression Model Building##
#Iteration 1
mod<- glm(DV~., data= ht[,-1], family = "binomial")
summary(mod)

##Iteration2
coef(summary(mod))[, "Pr(>|z|)"]< 0.1

mod<-glm(data = ht, DV ~ Sex + cp + trestbps + thalach + ca + thal + slope, family = "binomial")
summary(mod)

##AUC##
pred_test<-predict(mod, type = "response", newdata = ht)
library(ROCR)
p1<-prediction(pred_test, ht$DV)
auc<-performance(p1, "auc")
auc<-unlist(slot(auc, "y.values"))
auc


##Classification Matrix= 0.6##
p1<-prediction(pred_test, ht$DV)
p2<-performance(p1, "tpr", "fpr")
plot(p2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Labeling high and no heart risk patients using cut off as 0.6##
pred_test_label<-ifelse(pred_test>=0.6, "High Risk", "No Risk")
head(pred_test_label, 20)

##Confusion Matrix##
table(pred_test_label, ht$DV)


##Model interpretation and recommendation ##

##Scoring ##
library(gains)
gains(ht$DV, predict(mod, type="response", newdata=ht), groups = 10)

ht$prob<-predict(mod, type = "response", newdata = ht)
quantile(ht$prob, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted<-ht[ht$prob>=0.89629237 & ht$prob<=0.99650896, ]
targeted

library(dplyr)
targeted<-select(targeted, Patient.ID, prob)
targeted

write.xlsx(targeted, "C:\\Users\\madhurima\\Downloads\\targeted.xlsx")

##library(car)
#vif(mod)
