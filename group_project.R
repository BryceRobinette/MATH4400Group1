#-------------------------------------------------------------------------------
# G e n e r a l   I n f o r m a t i o n
#-------------------------------------------------------------------------------
# Name: group_project.R
#
# Usage: To compare LDA, QDA, GLM, and kNN models using generated data sets.
#
# Author(s): David Koster (djk)
#            davidkoster@mail.weber.edu
#            
#            Jacelyn Villalobos (jv)
#            jacelynvillalobos@mail.weber.edu
#
#            Kursten Reznik (kr)
#            kurstenreznik@mail.weber.edu
#
#            Bryce Robinette (br)
#            brycerobinette@mail.weber.edu
#
#            Jacob Ruiz (jr)
#            jacobruiz@mail.weber.edu
#
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

# LDA---------------------------------------------------------------------------
s.values = seq(0.1, 2, 0.1)
lda.averages = c()
for(i in s.values){
  df = data.generate(1, 0, i, 500, 20)
  lda.accuracy = c()
  scale(df[,-ncol(df)])
  for (i in c(1:100)){
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    lda.fit = lda(y~.-y, data = df, subset = -test.index)
    lda.pred = predict(lda.fit, df[test.index,])
    lda.means = mean(lda.pred$class == df[test.index,'y'])
    lda.accuracy = c(lda.accuracy, lda.means)
  }
  lda.average = mean(lda.accuracy)
  lda.averages = c(lda.averages, lda.average)
}

lda.averages
lda.plot = plot(s.values, lda.averages, type = 'l', main='LDA', xlab='Variance', 
                ylab='Accuracy', col = "blue")

# QDA---------------------------------------------------------------------------
s.values = seq(.1, 2, 0.1)
qda.averages = c()
for(i in s.values){
  df = data.generate(1, 0, i, 500, 20)
  qda.accuracy = c()
  for (i in c(1:100)){
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    qda.fit = qda(y~.-y, data = df, subset = -test.index)
    qda.pred = predict(qda.fit, df[test.index,])
    qda.means = mean(qda.pred$class == df[test.index,'y'])
    qda.accuracy = c(qda.accuracy, qda.means)
  }
  qda.average = mean(qda.accuracy)
  qda.averages = c(qda.averages, qda.average)
}

qda.averages
qda.plot = plot(s.values, qda.averages, type = 'l', main='QDA', xlab='Variance', 
                ylab='Accuracy', col = "red")

#Combining QDA & LDA onto one plot----------------------------------------------
plot(s.values, lda.averages, type = 'l', main='LDA vs QDA', xlab='Variance', 
     ylab='Accuracy', col = "blue")
par(new=TRUE)
plot(s.values, qda.averages, type = 'l', main='', xlab='Variance', 
     ylab='Accuracy', col = "red", axes=FALSE)

# GLM---------------------------------------------------------------------------
s.values = seq(0.1, 2, 0.1)
glm.averages = c()

for(i in s.values){
  df = data.generate(1, 0, i, 50, 2)
  df2 = data.glm(df)
  glm.accuracy = c()
  for (i in c(1:100)){
    test.index = sample(c(1:dim(df2)[1]), size = floor(.3*dim(df2)[1]), replace = FALSE)
    glm.fits = glm(y~.-y, data = df2[-test.index,], family = binomial)
    glm.probs = predict(glm.fits, df2[test.index,], type="response")
    glm.pred = ifelse(glm.probs > .5 , "1", "0")
    glm.means = mean(glm.pred == df2[test.index,'y'])
    glm.accuracy = c(glm.accuracy, glm.means)
    
  }
  glm.average = mean(glm.accuracy)
  glm.averages = c(glm.averages, glm.average)
}
glm.averages
glm.plot = plot(s.values, glm.averages, type = 'l', main='GLM', xlab='Variance', 
                ylab='Accuracy', col = "blue")
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

#KNN New Code-------------------------------------------------------------------
s.values = seq(0.1, 2, 0.1)
knn.averages = c()
for (i in s.values){
  knn.accuracy = c()
  df = data.generate(1, 0, i, 50, 2)
  for (i in c(1:100)){
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    train.X = df[-test.index,]
    test.X = df[test.index,]
    test.Y = df$y[test.index]
    train.Y = df$y[-test.index]
    pred.knn = knn(train.X, test.X, train.Y, k = 5)
    knn.means = mean(pred.knn == test.Y)
    knn.accuracy = c(knn.accuracy, knn.means)  
  }
  knn.average = mean(knn.accuracy)
  knn.averages = c(knn.averages, knn.average)
}

knn.plot = plot(s.values, knn.averages, type = 'l', main='KNN', xlab='Variance', 
                ylab='Accuracy', col='green')

#Combining KNN & GLM onto one plot----------------------------------------------
plot(s.values, knn.averages, type = 'l', main='KNN vs GLM', xlab='Variance', 
     ylab='Accuracy', col = "blue")
par(new=TRUE)
plot(s.values, glm.averages, type = 'l', main='', xlab='Variance', 
     ylab='Accuracy', col = "red", axes=FALSE)


#Combining all models onto one plot----------------------------------------------
plot(s.values, knn.averages, type = 'l', main='All Models', xlab='Variance', 
     ylab='Accuracy', col = "blue")
par(new=TRUE)
plot(s.values, glm.averages, type = 'l', main='', xlab='Variance', 
     ylab='Accuracy', col = "red", axes=FALSE)
par(new=TRUE)
plot(s.values, lda.averages, type = 'l', main='', xlab='Variance', 
     ylab='Accuracy', col = "orange", axes=FALSE)
par(new=TRUE)
plot(s.values, qda.averages, type = 'l', main='', xlab='Variance', 
     ylab='Accuracy', col = "darkgreen", axes=FALSE)
legend("bottomleft", legend = c("KNN","GLM","LDA","QDA"),
       col=c("blue","red","orange","darkgreen"), lty=1:1, cex=0.8)

#-------------------------------------------------------------------------------
# M a i n  P r o g r a m 
#-------------------------------------------------------------------------------

# Define parameters

N = 50 # Number of data points times two (i.e. N=50 is really N=100)
p = 2  # Number of predictors
s = cbind(seq(.1, 3, by = .1))

# kNN accuracy plot -----------------------------------------------------------
# First we need to find the best value of k for our data sets.
k.values = c(1:20)
knn.averages = c()
for (i in k.values)
{
  error.stored = c()
  
  for (j in c(1:10000))
  {
    data = data.generate(1,0,0.3,50,2)
    scale(data[,-ncol(data)])
    test.index = sample(c(1:dim(data)[1]), size = floor(.3*dim(data)[1]), replace = FALSE)
    train.X = data[-test.index,]
    test.X = data[test.index,]
    test.Y = data$y[test.index]
    train.Y = data$y[-test.index]
    
    knn.pred = knn(train.X,test.X,train.Y,k=i)
    error = sum(knn.pred != test.Y)/length(test.Y) #error rate
    error.stored = c(error.stored,error)
  }
  knn.average = mean(error.stored)
  knn.averages = c(knn.averages, knn.average)
}

plot(k.values,knn.averages, type = 'l', xlab='k values', ylab='Error Rate')
# Running through several variance values, the plot shows that the best value of
# k is 3 or 4.

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

