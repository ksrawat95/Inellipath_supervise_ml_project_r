read.csv('C:\\Users\\rawat kundan\\OneDrive\\Documents\\R_Classes\\Assignment\\census-income_.csv',stringsAsFactors = T)->censusData
View(censusData)

#DATA PRE-PROCESSING

#REPLACE ALL MISSING VALUES WITH NA

str(censusData)
  #AFTER LOOKING THE STRUCTURE OF DATA : 
    #IN THIS CASE MISSING VALUES ARE " ?"
  #CONVERT ALL THE FACTOR FIELDS TO CAHR FIELD FOR REPLACING THE VALUES.

censusData$workclass<-as.character(censusData$workclass)
censusData$education<-as.character(censusData$education)
censusData$marital.status<-as.character(censusData$marital.status)
censusData$occupation<-as.character(censusData$occupation)
censusData$relationship<-as.character(censusData$relationship)
censusData$race<-as.character(censusData$race)
censusData$sex<-as.character(censusData$sex)
censusData$native.country<-as.character(censusData$native.country)
censusData$X<-as.character(censusData$X)

  # REPLACE EVERY " ?" IN DATA WITH NA

censusData[censusData==" ?"]<-NA

View(censusData)

  # REMOVE ALL THE ROWS THAT CONTAINS NA VALUES

table(is.na(censusData))

colSums(is.na(censusData))

censusData<-na.omit(censusData)

View(censusData)

  # REMOVE WHITE STATE FROM COLUMNS

library(dplyr)
  #for mutate_if function
library(stringr)
  #for str_trim function

censusData<-mutate_if(censusData,is.character,str_trim)

View(censusData)

#DATA MANUPLATION WITH DPLYR

  #EXTRACT education COLUMN FROM DATA SET AND STORE IT TO ANOTHER VARIABLE

census_ed<-censusData$education

View(census_ed)

census_seq<-censusData%>%
  select(age:relationship)
  #OR
census_seq<-select(censusData,age:relationship)
View(census_seq)

  #EXTRACT COLUMN NUMBER 5 , 8 , 11 and store it in "census_col"
census_col<-censusData[,c(5,8,11)]
View(census_col)

 #EXTRACT THE MALES WHO WORK IN STATE GOVT AND THEN STORE IT IN male_gov

male_gov<-censusData%>%
  filter(sex=="Male" & workclass=="State-gov")
View(male_gov)

  #EXTRACT ALL WITH AGE OF 39 WHO IS HEAVING EITHER BACHELOR's DEGREE OR NATIVE OF US STORE IT IN census_us

census_us<-censusData%>%
  filter(age==39&(education=="Bachelors"|native.country=="United-States"))

View(census_us)

  #EXTRACT 200 RANDOM ROWS FROM THE DATA SET , STORE IT IN census_200

census_200<-sample_n(censusData,200)

View(census_200)

  #EXTRACT COUNT OF DIFFERENT LVL OF COLUMN WORKCLASS AND STORE IN countWcls
countWcls<-count(censusData,workclass)

View(countWcls)

  #CALCULATE THE MEAN OF CAPITAL.GAIN COLUMN GROUPED ACCORDING TO WORKCLASS

censusData%>%
  group_by(workclass)%>%
  summarise(mean(capital.gain))

#DATA VISUALIZATION:
  library(ggplot2)

#BUILD BAR PLOT FOR RELATIONSHIP COLUMN AND FILL ACCORDING TO RACE COLUMN

ggplot(censusData,aes(x=relationship,fill=race))+
  geom_bar()

#SET X AXIS LABLE TO "Categories of Relationships"
#SET Y AXIS LABLE TO "Count of Categories"

ggplot(censusData,aes(x=relationship,fill=race))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories")

# Fill BAR ACCORDING TO SEX

ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories",title = "Distribution of relationship by sex")

#BUILD HISTROGRAM FOR AGE COLUMN WITH NUMBER OF BIN 70
ggplot(censusData,aes(x=age))+
  geom_histogram(bins = 70)

#FILL BARS OF HISTOGRAM ACCORDING TO YEARLY INCOME AND BIN 90
ggplot(censusData,aes(x=age,fill=X))+
  geom_histogram(bins = 70)

#SET TITLE OF PLOT "DISTRIBUTION OF AGE"
ggplot(censusData,aes(x=age,fill=X))+
  geom_histogram(bins = 70)+
  labs(title="DISTRIBUTION OF AGE")

#SET TITLE LEGEND "YEARLY INCOME"
ggplot(censusData,aes(x=age,fill=X))+
  geom_histogram(bins = 70)+
  labs(title="DISTRIBUTION OF AGE",fill="Yearly Income")

#SeT THEAME BLACK AND WHITE
ggplot(censusData,aes(x=age,fill=X))+
  geom_histogram(bins = 70)+
  labs(title="DISTRIBUTION OF AGE",fill="Yearly Income")+
  theme_dark()

# c)Build a scatter-plot between "capital.gain" and "hours.per.week".
#     Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.

ggplot(censusData,aes(x=capital.gain,y=hours.per.week))+
  geom_point(col="Blue")
# i)	Set the transparency of the points to 40% and size as 2.
ggplot(censusData,aes(x=capital.gain,y=hours.per.week))+
  geom_point(alpha=0.60,size=3)
# ii)	Set the color of the points according to the "X" (yearly income) column. 
ggplot(censusData,aes(x=capital.gain,y=hours.per.week,col=X))+
  geom_point()
# iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title
# to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly Income".
ggplot(censusData,aes(x=capital.gain,y=hours.per.week,col=X))+
  geom_point(alpha=0.6,size=2)+
  labs(x="Capital Gain",y="Hours per Week",
       title = "Capital Gain vs Hours per Week by Income", col="Yearly Income") 
#install.packages("plotly")
library(plotly)
plot_ly(data=censusData, x = ~capital.gain, y = ~hours.per.week, color = ~X, type='scatter')

# d)	Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
# "age" on the y-axis.

ggplot(censusData,aes(x=education,y=age))+geom_boxplot()
# i)	Fill the box-plots according to the "sex" column.
ggplot(censusData,aes(x=education,y=age,fill=sex))+geom_boxplot()
# ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(censusData,aes(x=education,y=age,fill=sex))+
  geom_boxplot()+labs(title = "Box-Plot of age by Education and Sex")




# 4.	Linear Regression:
# 
# a)	Build a simple linear regression model as follows:

# i)	Divide the dataset into training and test sets in 70:30 ratio.
# 1 2 3 4 5 6 7 8 9 0
#4 6 7 
#3 6 4 
#5 2 8
#same random numbers 
#2 7 5
#2 7 5
#2 7 5

set.seed(1298) #any integer number no problem
# install.packages("caTools")
library("caTools")
split_data<-sample.split(censusData$hours.per.week,SplitRatio = 0.70)
View(split_data)
censusTrain<-subset(censusData,split_data==TRUE)
censusTest<-subset(censusData,split_data==F)
View(censusTrain)
View(censusTest)
nrow(censusTrain)
nrow(censusTest)

# ii)	Build a linear model on the train set where the dependent variable is
# "hours.per.week" and independent variable is "education.num".
#dependent~independ
View(censusData[c('hours.per.week','education.num')])



LR_model<-lm(hours.per.week~education.num,data=censusTrain)
summary(LR_model)

# iii)	Predict the values on the test set and find the error in prediction. 
#iv)Find the root-mean-square error (RMSE).
censusP<-predict(LR_model,newdata=censusTest)
head(censusP)
View(censusP)
# Actual data - predicted data
Error=censusTest$hours.per.week - censusP
# 3 -4 -7 erros
# 3* + -4* + -7*/3 = ? root(?)

sqrt(mean((Error)^2))


# 5.	Logistic Regression:
# 
# a)	Build a simple logistic regression model as follows:
# 
# i)	Divide the dataset into training and test sets in 65:35 ratio.
split_data1<-sample.split(censusData$X,SplitRatio = 0.65)
censusTrain1<-subset(censusData,split_data1==T)
censusTest1<-subset(censusData,split_data1==F)
nrow(censusTrain1)
nrow(censusTest1)

# ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variable is "occupation".
#as.factor(censusData$X)->censusData$X
log_mod<-glm(X~occupation,data=censusTrain1,family = "binomial") 
#summary(log_mod)
# iii)	Predict the values on the test set.
pred_val<-predict(log_mod,newdata =censusTest1,type = "response")#probability #response, term
range(pred_val)
table(censusTest1$X,pred_val>0.47)


#install.packages("ROCR")
library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest1$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)## Check for which valve accuracy get constant

# iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off.
lm.pred<-ifelse(pred_val>0.47,">50K","<=50K")  
# v)	Build a confusion matrix and find the accuracy.
tab<-table(lm.pred,censusTest1$X)
tab
#TP FP
#FN TN
#TP TN -correctly predicted
#FP FN - wrongly predicted

(7188+660)/(7188+660+1968+741)
accuracy<-sum(diag(tab))/sum(tab)
accuracy

#TP/TP+FP=PRECISION
#TP/TP+FN=RECALL

# vi)	Plot the ROC curve and find the auc(Area Under Curve). 
roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc@y.values

split_data1<-sample.split(censusData$X,SplitRatio = 0.80)
censusTrain2<-subset(censusData,split_data1==T)
censusTest2<-subset(censusData,split_data1==F)

log_mod2<-glm(X~age+workclass+education+occupation,data=censusTrain2,family = "binomial")
summary(log_mod2)
pred_val<-predict(log_mod2,newdata =censusTest2,type = "response")
head(pred_val)
range(pred_val)
#library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest2$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)
lm.pred<-ifelse(pred_val>0.61,">50K","<=50K")  
lm.pred

tab<-table(lm.pred,censusTest2$X)
tab
accuracy<-sum(diag(tab))/sum(tab)
accuracy



performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc



# 6.	Decision Tree:
# 
# a)	Build a decision tree model as follows:
# 
# i)	Divide the dataset into training and test sets in 70:30 ratio.
# ii)	Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables.
# iii)	Plot the decision tree.
# iv)	Predict the values on the test set.
# v)	Build a confusion matrix and calculate the accuracy.
set.seed(123)
split_data<-sample.split(censusData$X,SplitRatio = 0.70)
View(split_data)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot) 
names(censusData)
census_model<-rpart(X~.,
                    data = censusTrain,
                    method = "class")
rpart.plot(census_model, type= 5, extra = 0,tweak = 1.4) # font size- tweak
class(censusData$X)

class_prediction<-predict(census_model,
                          newdata = censusTest,
                          type = "class")

    #TP  FP
#TN 6473 1121
#FN 323  1131
#FN  TN
#TP TN -correctly predicted
#FP FN - wrongly predicted
confusion_mat<-table(class_prediction,censusTest$X)
confusion_mat
(6473+1131)/(6473+1121+323+1131)
sum(diag(confusion_mat))/sum(confusion_mat)
#Precision


# 7.	Random Forest:
# 
# a)	Build a random forest model as follows:
# 
# i)	Divide the dataset into training and test sets in 80:20 ratio.
# ii)	Build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
# iii)	Predict values on the test set
# iv)	Build a confusion matrix and calculate the accuracy

set.seed(12)
split_data<-sample.split(censusData$X,SplitRatio = 0.80)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)

library(randomForest)

census_model<-randomForest(X~.,
                           data=censusTrain,
                           ntree=300) # number of trees to be 300

cenus_prediction<-predict(census_model,
                          newdata = censusTest,
                          type = "class")

tab<-table(cenus_prediction,censusTest$X)
tab
sum(diag(tab))/sum(tab)

