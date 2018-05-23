---
title: "KNN"
author: "PRAKASH KUMAR"
date: "23 May 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```
```{r}
setwd('E:/Machine_Learning_AZ/Machine Learning A-Z/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)')
dataset = read.csv('Social_Network_Ads.csv')
dataset <- dataset[3:5]
head(dataset,15)
```
### Encoding the target feature as factor
```{r}
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))
```
### Splitting the dataset into the Training set and Test set
```{r}
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set = subset(dataset,split==T)
test_set = subset(dataset,split==F)
```
### Feature Scaling
```{r}
training_set[-3]= scale(training_set[-3])
test_set[-3]=scale(test_set[-3])
```
### Fitting K-NN to the Training set and Predicting the test set results
```{r}
library(class)
y_pred = knn(train = training_set[,-3],test = test_set[,-3],cl=training_set[, 3],k=5)
```
### Making the Confusion Matrix
```{r}
cm = table(test_set[,3],y_pred)
summary(cm)
```
### Visualising the training set results
```{r}
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[,1])-1,max(set[,1]) + 1, by=0.01)
X2 = seq(min(set[,2])-1,max(set[,2])+ 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set)= c('Age','EstimatedSalary')
y_grid = knn(train = training_set[,-3],test = grid_set,cl=training_set[, 3],k=5)

plot(set[,-3],
     main = 'K-NN (Training set)',
     xlab= 'Age', ylab = 'Estimated Salary',
     xlim = range(X1),ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1),length(X2)),add=T)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch=21, bg=ifelse(set[,3]==1,'green4','red3'))
```
### Visualising the Test set results
```{r}
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[,1])-1,max(set[,1]) + 1, by=0.01)
X2 = seq(min(set[,2])-1,max(set[,2])+ 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set)= c('Age','EstimatedSalary')
y_grid = knn(train = training_set[,-3],test = grid_set,cl=training_set[, 3],k=5)

plot(set[,-3],
     main = 'K-NN (Test set)',
     xlab= 'Age', ylab = 'Estimated Salary',
     xlim = range(X1),ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1),length(X2)),add=T)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch=21, bg=ifelse(set[,3]==1,'green4','red3'))
```











