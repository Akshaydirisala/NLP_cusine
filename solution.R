rm(list=ls(all=TRUE))
directory<-setwd("C:\\Users\\adirisala\\Downloads")
############################################################################################
forrest_data<-read.csv(file = "train.csv",header = T)##reading the data
test_data<-read.csv(file = "test.csv",header = T)##reading the data
forrest_data<-forrest_data[,c(-1)]
head(forrest_data)
colnames(forrest_data)
is.data.frame(forrest_data)
str(forrest_data)
####all are intezer variables along with the calssification variable 
sum(is.na(forrest_data))##making sure there are no missing values in data
boxplot(forrest_data$Aspect~forrest_data$Cover_Type,data = forrest_data)
#library(dplyr)
###function for subsetting the data based on the Covertype
fs<-sort(unique(forrest_data$Cover_Type))

covertype<-function(n){
  for(i in  fs)    
  {
    X<-subset(forrest_data,forrest_data$Cover_Type==n)

  }
  return(X)
  }
###SOIL COMBINATIONS
##install.packages("lazyeval")
#library(dplyr)
##############################################################
x1<-covertype(1)


typeof(Soil_combination(4))
###############################################################
Soil_combination<-function(N){
  forrest_label<-covertype(N)
  SOIL_PARTITION<-forrest_label%>%select(starts_with("Soil_"))
#  x<-colnames(SOIL_PARTITION)
 # print(x)
   #print(head(SOIL_PARTITION))
    soil_FActor<-c()
    for(i in seq(SOIL_PARTITION)){
    #print(names(SOIL_PARTITION[,i]))  
    if (any(SOIL_PARTITION[,i]==1)) {
      
     result<- print(i)  
    }
    else{
      result<-paste0("This type of soil is not present in the Cover type :%s",i)
    }
    }
  return(result)
}
Soil_combination(5)
#################################################################################################

######Summary for each cover type

each_cover<-function(n1){
              each<-covertype(n1)
              each_cover_average<-c()  
              for(j in 1:10) 
                  {
                    y1<-mean(each[,c(j)])
                    #print(y1)
                    each_cover_average[j]<-y1
                    names(y1[j])<-names(forrest_data$j)
                                  }
              print(each_cover_average)
}
each_cover(3)
####################################################################

###creating the validatoion data
set.seed(123)
smp_size <- floor(0.75 * nrow(forrest_data))
train_ind <- sample(seq_len(nrow(forrest_data)), size = smp_size)
train <- forrest_data[train_ind, ]
validation_data<- forrest_data[-train_ind,]

#####################################################################

frequency_each_category<-table(train$Cover_Type)
frequency_each_category
###sample size of each category is good


  ##########################################each soil graphs for each covertype


#Pie Chart with Percentages
slices <- train%>%select(starts_with("Soil_"))
lbls <- train$Cover_Type
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")












install.packages("caret")
library("caret")

forrest_data[["Cover_Type"]]<-factor(forrest_data[["Cover_Type"]])


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(forrest_data$Cover_Type ~., data = forrest_data, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

warnings()

############################################################################
###creating the validatoion data
set.seed(123)
smp_size <- floor(0.75 * nrow(forrest_data))
train_ind <- sample(seq_len(nrow(forrest_data)), size = smp_size)
train <- forrest_data[train_ind, ]
validation_data<- forrest_data[-train_ind,]
#####################################################################KNN

##Tranning model on data
install.packages("class")
library(class)

prc_test_pred <- knn(train = train, test = test_data,cl = train$Cover_Type, k=10)


#########################################evaluation the model performance
install.packages("gmodels")
library(gmodels)
CrossTable(x=validation_data$Cover_Type,y=prc_test_pred,prop.chisq = FALSE)

######################################################################SVM

library(e1071)
x1<- subset(forrest_data, select = -forrest_data$Cover_Type) 
y1<- forrest_data$Cover_Type
pred_prob <- predict(model, x1, decision.values = TRUE, probability = TRUE)
pred_prob
names(forrest_data[])
