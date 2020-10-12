#-------------------------------------------------------------------------------
# G e n e r a l   I n f o r m a t i o n
#-------------------------------------------------------------------------------
# Name: 
#
# Usage: 
#
# Description: 
#
# Inputs: 
#
# Outputs: 
#
# Auxiliary Files: 
#
# Special Instructions:  
#
#-------------------------------------------------------------------------------
# C o d e   H i s t o r y
#-------------------------------------------------------------------------------
# Version: 1.0
#
# Author(s): David Koster (djk)
#            davidkoster@mail.weber.edu
#            
#            Jacelyn Villalobos (jv)
#            jacelynvillalobos@mail.weber.edu

#            Kursten Reznik (kr)
#            Bryce
#            Jacob
#
# Modifications:
# 11 Oct 2020 - Added functions to generate data, create training and test data,
#               calculate the best k value, and calculate the accuracy of the 
#               kNN model. (djk) 
# 12 Oct 2020 - LDA and QDA (jv). 
# 12 Oct 2020 - Added code to loop through several variance values and to create 
#               a plot of accuracy vs variance. (djk)
# 
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# I m p o r t   L i b r a r i e s
#-------------------------------------------------------------------------------

library('MASS')
library('leaps')
library('class')
library(pROC)

#-------------------------------------------------------------------------------
# D e f i n e  F u n c t i o n s
#-------------------------------------------------------------------------------

# Data generation
# Syntax - 
# F(mean of 1s, mean of 2s, variance, 1/2 of data, predictors)

data.generate = function(mu1, mu2, s, n, p)
{
  y = as.factor(c(rep(1,n), rep(2,n)))
  M = matrix(NA, nrow = 2*n, ncol = p, byrow = 1)
  df = data.frame(y)
  
  for (i in c(1:p))
  {
    M[,i] = c(rnorm(n,mu1,s), rnorm(n,mu2,s))
  }
  
  df = cbind(M,y)
  df = data.frame(df)
  
  return(df)
}

# adding in the glm 01 column---------------------------------------------------

data.glm = function(df)
{
  newy = ifelse(df$y > median(df$y),1,0)
  df <- df[-ncol(df)]
  df$y <- newy
  
  return(df)
}

# add line to scale-------------------------------------------------------------
# scale(df[,-ncol(df)])

# Training and testing data-----------------------------------------------------

trainTest = function(df)
{
  test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
  
  assign("train.X", df[-test.index,], envir = .GlobalEnv)
  assign("test.X", df[test.index,], envir = .GlobalEnv)
  assign("train.Y", df[-test.index, ncol(df)], envir = .GlobalEnv)
  assign("test.Y", df[test.index, ncol(df)], envir = .GlobalEnv)
  
}

# kNN---------------------------------------------------------------------------

determineK = function(train.X,test.X,train.Y)
{
  k.values = c(1:20)
  error.stored = c()

  for (i in c(1:20))
  {
    knn.pred = knn(train.X,test.X,train.Y,k=i)
    error = sum(knn.pred != test.Y)/length(test.Y) #error rate
    error.stored = c(error.stored,error)
  }

  plot(k.values,error.stored, type = 'l')
}

kNNAccuracy = function(train.X,test.X,train.Y,kvalue)
{
  knn.pred = knn(train.X, test.X, train.Y, k = kvalue)
  table(knn.pred, test.Y)
  mean(knn.pred == test.Y)
}


# LDA---------------------------------------------------------------------------
lda.function = function(df,N)
{
  N2 = N*2
  m=20
  sum.accuracy.lda = 0
  for (i in c(1:m))
  {
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    
    lda.fit = lda(y~.-y, data = data,subset = -test.index)
    lda.fit
    
    lda.pred = predict(lda.fit,df[test.index,])
    lda.pred$class
    
    #mytable = table(lda.pred$class,df[test.index,'y'])
    #print(mytable)
    
    accuracy.lda = (1-(mean(lda.pred$class==df[test.index,'y']))) * 100
    #print(accuracy.lda)
    sum.accuracy.lda = (sum.accuracy.lda + accuracy.lda)
    #print(sum.accuracy.lda)
  }
  print('LDA: Average level of Accuracy = ')
  print(sum.accuracy.lda / (N2*m))
}


# QDA---------------------------------------------------------------------------
qda.function = function(df,N)
{
  N2 = N*2
  m=20
  sum.accuracy.qda = 0
  for (i in c(1:m))
  {
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    
    qda.fit = qda(y~.-y,data = data,subset = -test.index)
    qda.fit
    
    qda.pred = predict(qda.fit,df[test.index,])
    qda.pred$class
    
    #mytable = table(qda.pred$class,df[test.index,'y'])
    #print(mytable)
    
    accuracy.qda = (1-(mean(qda.pred$class==df[test.index,'y']))) * 100
    #print(accuracy.qda)
    sum.accuracy.qda = (sum.accuracy.qda + accuracy.qda)
    #print(sum.accuracy.qda)
  }
  print('QDA: Average level of Accuracy = ')
  print(sum.accuracy.qda / (N2*m))
}

# GLM---------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# M a i n  P r o g r a m 
#-------------------------------------------------------------------------------

# Define parameters

N = 50 # Number of data points times two
p = 2  # Number of predictors
s = cbind(seq(.1, 3, by = .1))

# kNN accuracy plot ------------------
data = data.generate(1,0,0.3,N,p)
determineK(train.X,test.X,train.Y)
# Running through several variance values, the plot shows that the best value of
# k is 3.

acckNN = c()
for (i in c(1:length(s)))
{
  data = data.generate(1,0,s[i],N,p)
  acc = c()
  for (i in c(1:300))
  {
    trainTest(data)
    a = kNNAccuracy(train.X,test.X,train.Y,3)
    acc = c(acc,a)
  }
  acckNN = c(acckNN, mean(acc))
}
plot(s, acckNN, type = 'l', main='kNN', xlab='Variance', ylab='Accuracy')


lda.function(data,N)
qda.function(data,N)













