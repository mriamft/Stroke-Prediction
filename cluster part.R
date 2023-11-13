data<-read.csv("healthcare-dataset-stroke-data.csv")

head(data)

print(data)

str(data)

#Number of rows
nrow(data)
#Number of column
ncol(data)

install.packages("multcomp")
library(multcomp)
install.packages("party")
library(party)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
library("ggplot2")
library("dplyr")
library("ggfortify")

# import necessary libraries
#library(tidyverse) # 
library(ggplot2)
library(lubridate)
library(magrittr)
#library(dplyr)
#library(tidyr) 
#library(readr) 
library(outliers)
#library(caret) 
#library(DMwR) 


library(Hmisc)
describe(data)

#Five number summary:
summary(data)

library(dplyr)
data %>% summarise_if(is.numeric, var)

# check gender
data <- data %>% filter(gender != "Other")
ggplot(data, aes(x = gender, fill = as.factor(stroke))) +
  geom_bar(position = "fill") +
  labs(fill = "STROKE", title= "Incident of stroke between genders")

tab <- data$work_type %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') # text on chart
pie(tab, labels=txt ,main= "Chart of employement status") # plot pie chart

ggplot(data) + geom_point(mapping = aes(y = age, x = stroke, color = stroke ), alpha =0.9 )+ 
  labs(title = "Distribution of Stroke status by age" ) +
  theme( plot.title = element_text(size = 14, face = "bold"), legend.position = "none", axis.line = element_line(size = 1), axis.ticks = element_line() )

ggplot(data, aes(x = ever_married, fill = as.factor(stroke))) +
  geom_bar(position = "fill") +
  labs(fill = "STROKE", title="Correlation between stroke occurence and marrige status")

# Histogram of Average Glucose Level with normal distribution overlay
histglucose <- hist(data$avg_glucose_level,xlim=c(0,300),
                    main="Histogram of Avg. Glucose with Normal Distribution Overlay",
                    xlab="Avg. Glucose",las=1)
xfit <- seq(min(data$avg_glucose_level),max(data$avg_glucose_level))
yfit <- dnorm(xfit,mean=mean(data$avg_glucose_level),sd=sd(data$avg_glucose_level))
yfit <- yfit*diff(histglucose$mids[1:2])*length(data$avg_glucose_level)
lines(xfit,yfit,col="red",lwd=2)

# Change "N/A" to actual NULL
data$bmi[data$bmi=="N/A"] <-NA

# Checking missing values
sum(is.na(data))

# Converting bmi to numeric
data$bmi <- as.numeric(as.character(data$bmi))
# Checking bmi type
class(data$bmi)

# Replacing null values with the mean
data$bmi[is.na(data$bmi)]<-mean(data$bmi, na.rm = TRUE)

# Missing values
sum(is.na(data))

# Checking duplicated rows
sum(duplicated(data))

#install outliers package
library(outliers)

#detect Age outliers
OutAge <- outlier(data$age)
print(OutAge)

#checking the outlier location before delete
indices <- which(data$age == OutAge)

# Print the resulting row indices
print(indices)


#Remove age outlier
data <- data[data$age != OutAge, ]


#detect Average glucose level outliers
OutAvg <- outlier(data$avg_glucose_level)
print(OutAvg)

#Remove Average glucose level outlier
data <- data[data$avg_glucose_level != OutAvg, ]


#detect bmi outliers
OutBMI <- outlier(data$bmi)
print(OutBMI)

#Remove bmi outlier
data <- data[data$bmi != OutBMI, ]


#check after deleting
#Number of rows
nrow(data)
#Number of column
ncol(data)

# outliers row is removed now
print(data[, c("age", "avg_glucose_level","bmi")])


indices <- which(data$age == OutAge)

# Print the resulting row indices
print(indices)


indices3 <- which(data$avg_glucose_level == OutAvg)

# Print the resulting row indices
print(indices3)

indices2 <- which(data$bmi == 97.6)

# Print the resulting row indices
print(indices2)

#only one column with Gender "Other" 
data[data$gender=="Other", ]

# delete it 
data = data[data$gender!="Other", ]

##check
table(data$gender)

## convert stroke to factor(to use scaling)
data$stroke <- as.factor(data$stroke)

head(data)

data$work_type = factor(data$work_type,levels = c("Govt_job","Private", "Self-employed"
                                                  ,"children","Never_worked"), labels = c(5,4,3,2,1))

data$gender = factor(data$gender, levels = c("Male", "Female"), labels = c(1, 2))

data$ever_married= factor(data$ever_married, levels = c("No", "Yes"), labels = c(0, 1))

data$Residence_type= factor(data$Residence_type, levels = c("Urban", "Rural"), labels=c(1,2))

data$smoking_status= factor(data$smoking_status, levels = c("Unknown","never smoked", "formerly smoked","smokes"), label=c(1,2,3,4))

head(data)

normalize <- function(x)
{
  return ((x - min(x))/ (max(x)- min(x)))   
}

data$avg_glucose_level= normalize(data$avg_glucose_level)
data$age= normalize(data$age)
data$bmi= normalize(data$bmi)
head(data)

#
# ensure results are repeatable
#set.seed(7)
# load the library
library(mlbench)
library(caret)

# prepare training scheme
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
#model <- train(stroke~., data, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
#importance <- varImp(model, scale=FALSE)
# summarize importance
#print(importance)
# plot importance
#plot(importance)

# ensure the results are repeatable
#set.seed(7)
# define the control using a random forest selection function
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(data[,1:11], data[,12], sizes=c(1:11), rfeControl=control)
# summarize the results
#print(results)
# list the chosen features
#predictors(results)
# plot the results
#plot(results, type=c("g", "o"))

#delete id coloumn
data <-data[,!names(data) %in% c("id", "gender", "Residence_type")]
head(data)

# upscaling the data
data<-upSample(data[,-9],data$stroke, yname="stroke")
plot(data$stroke)

# checking the number of stroke/ non-stroke observations
prop.table(table(data$stroke))
table(data$stroke)
title(main="Data after oversampling", xlab="Stroke", ylab="observations")

#Finally, checking overall
head(data)

#Number of rows
nrow(data)

################################# classifcation




#data$age= cut(data$age, breaks= seq(0,82, by=40),right=TRUE)
data$avg_glucose_level	= cut(data$avg_glucose_level, breaks= seq(0,1, by=0.5),right=TRUE)
data$bmi	= cut(data$bmi, breaks= seq(0,1, by=0.5),right=TRUE)
myFormula <- stroke ~ age + hypertension +ever_married+avg_glucose_level


#partitioning
set.seed(1234)
ind=sample (2, nrow(data), replace=TRUE, prob=c(0.85 , 0.15))
trainData=data[ind==1,]
testData=data[ind==2,]

####c50
install.packages('C50')
library('C50')


data$stroke<- as.factor(data$stroke)
strokeTree <- C5.0(myFormula, data=trainData)
summary(strokeTree)
plot(strokeTree, main = 'Strokedecisiontree')


head(data)


data$age<- as.factor(data$age )
data$hypertension <- as.factor(data$hypertension )
data$heart_disease <- as.factor(data$heart_disease )
data$ever_married   <- as.factor(data$ever_married  )
data$work_type   <- as.factor(data$work_type  )
data$smoking_status <- as.factor(data$smoking_status )
data$bmi <- as.factor(data$bmi )
data$avg_glucose_level <- as.factor(data$avg_glucose_level)
































##################################cluster
library(factoextra)     
data <- data[,!names(data) %in% c("stroke","id","Residence_type","gender")]

head(data)



#Making changes for converting data types

#Converting interger&factor columns too numeric
data$age<- as.numeric(data$age )
data$hypertension <- as.numeric(data$hypertension )
data$heart_disease <- as.numeric(data$heart_disease )
data$ever_married   <- as.numeric(data$ever_married  )
data$work_type   <- as.numeric(data$work_type  )
data$smoking_status <- as.numeric(data$smoking_status )
data$bmi <- as.numeric(data$bmi )
data$avg_glucose_level <- as.numeric(data$avg_glucose_level)

#for ease creating a data set without color column
data_no_color <- data[1:8]

#Let us see the structure again
str(data_no_color)




#######cluster k=2

#calculate k-mean k=2
km <- kmeans(data, 2, iter.max = 140 , algorithm="Lloyd", nstart=100)
km


#plot k-mean 
fviz_cluster(list(data = data, cluster = km$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#avg silhouette
library(cluster)
sil <- silhouette(km$cluster, dist(data))
rownames(sil) <- rownames(data)
fviz_silhouette(sil)


# Total sum of squares
km$totss



#######cluster k=3

#calculate k-mean
km <- kmeans(data, 3, iter.max = 140 , algorithm="Lloyd", nstart=100)
km


#plot k-mean
fviz_cluster(list(data = data, cluster = km$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#avg silhouette
library(cluster)
sil <- silhouette(km$cluster, dist(data))
rownames(sil) <- rownames(data)
fviz_silhouette(sil)

# Total sum of squares
km$totss



#######cluster k=5

#calculate k-mean
km <- kmeans(data, 5, iter.max = 140 , algorithm="Lloyd", nstart=100)
km


#plot k-mean
fviz_cluster(list(data = data, cluster = km$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#avg silhouette
library(cluster)
sil <- silhouette(km$cluster, dist(data))
rownames(sil) <- rownames(data)
fviz_silhouette(sil)

# Total sum of squares
km$totss







##################elbow with wss

fviz_nbclust(data, kmeans, method = "wss")+ labs(subtitle = "Elbow method")


























































