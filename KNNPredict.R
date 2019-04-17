data(readr)
library(readxl)
dataset<-read_excel(file.choose())
install.packages('DataExplorer')
library(DataExplorer)
str(dataset)

#Exploratory data Ananlysis
datasett<-dataset[-1]
str(datasett)
datasett$`Oil Leakage`<-as.factor(dataset$`Oil Leakage`)
datasett$`Fuel supply`<-as.factor(dataset$`Fuel supply`)
datasett$`Vector Group`<-as.factor(dataset$`Vector Group`)
datasett$Insulation<-as.factor(datasett$Insulation)
dataset$`Energy Losses`<-as.factor(datasett$`Energy Losses`)
datasett$`Pressure Relay`<-as.factor(datasett$`Pressure Relay`)
datasett$`Cooling Operation`<-as.factor(datasett$`Cooling Operation`)
datasett$Bushing<-as.factor(datasett$Bushing)
datasett$`Over Current Protection (OC)`<-as.factor(datasett$`Over Current Protection (OC)`)
datasett$`Fire Fighting Systems (FFS)`<-as.factor(datasett$`Fire Fighting Systems (FFS)`)
datasett$`Silica Gel Color`<-as.factor(datasett$`Silica Gel Color`)
datasett$Outage<-as.factor(datasett$Outage)
#structure of dataset
str(datasett)
#plot variables
plot_str(datasett)
#plot_missing
plot_missing(datasett)
sum(is.na(datasett))
#pakcages 
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}
basic_eda(datasett)
freq(datasett)
plot_num(datasett)
plot_histogram(datasett)
plot_density(datasett)
plot_correlation(datasett, type = 'continuous','Review.Date')
plot_bar(datasett)
boxplot(datasett)
summary(datasett)
dim(datasett)


# Transformation – normalizing numeric data
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
str(datasett)
datasett[c(1,2,9,11,12,13,17,18,19,20,22,23)]<-lapply(datasett[c(1,2,9,11,12,13,17,18,19,20,22,23)], normalize)
summary(datasett)

#radomdise
set.seed(1234)
#PARTITITOON DATASET into traning and testing
sampling<-sample(2,nrow(datasett),replace = T,prob=c(0.8,0.2))
train<-datasett[sampling==1,]
test<-datasett[sampling==2,]
dim(train)
dim(test)


train_labels<-train[,24]
View(train_labels)
test_labels<-test[,24, drop = TRUE]
View(test_labels)
dim(train_labels)
dim(train)
library(class)
cl<-train[,24, drop = TRUE]
#predicting test
test_pred <- knn(train,test,cl,k=12)
head(test_pred)
head(test_labels)
#predicting train
train_pred <- knn(train,train,cl,k=15)
head(train_pred)
head(train_labels)

#gmodels"
install.packages("gmodels")
library(gmodels)
CrossTable(x=test_labels,test_pred,prop.chisq=FALSE)

#error
eror=1-mean(test_pred==test_labels)
eror


error <- c()
for (i in 1:15)
{
  knn.fit <- knn(train = train, test = test, cl, k = i)
  error[i] = 1- mean(knn.fit == test_labels)
}
ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "Blue")
