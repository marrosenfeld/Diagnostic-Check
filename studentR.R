library(dplyr)
#load data
student_data <- read.csv(file="student/student-mat.csv",head=TRUE,sep=";")
hist(student_data$G3)
length(which(student_data$G3>10))
nrow(student_data)
#X = as.data.frame(student_data[,c("traveltime","studytime","goout","absences","famrel")])
X = select(student_data,traveltime, studytime,goout,absences)
y = 1*(student_data$G3>10)

#fit logistic regression
fit <- glm(y ~ X$studytime+X$absences,family=binomial(link='logit'))
summary(fit)

#bayesian approach
library(LearnBayes)
library(boot)
library(base)

logl <- function(theta,x,y){
  y <- y
  x <- as.matrix(x)
  
  beta <- t(theta)
  loglik <- exp(t(y) %*% (x%*%beta) - sum(log(1 + exp(x%*%beta))))
  return(loglik)
}

beta0<-seq(from=-0.4,to=-0.1,length=50)
beta1<-seq(from=0.1,to=0.4,length=50)
beta2<-seq(from=-0.02,to=0,length=50)
#beta3<-seq(from=-0.4,to=-0.1,length=50)
#beta4<-seq(from=-0.02,to=0,length=50)
parameter<-expand.grid(beta0,beta1,beta2)

X$intercept<-1
X = X[,c(5,2,4)]


B<-nrow(parameter)
post<-numeric(B)
for (i in 1:B) {
  post[i]<-logl(parameter[i,],X, y)
}
max(post)
parameter[which(post==max(post)),] # the mode of the estimate, should be approximately equal to the MLE.


#posterior check
library(boot)

L<-1000
grid_size <- length(beta0) * length(beta1) * length(beta2)
sample_size <- 395

beta <- NULL 

probability <- function(beta,X){
  inv.logit(as.matrix(X)%*%(as.matrix(t(beta))))
}

for (j in 1:L) {
  beta[[j]] <- parameter[sample(1:grid_size,1,prob=post),]
}

y_rep <-NULL
y_rep <- matrix(nrow=L, ncol=395)
for (j in 1:L){
  y_rep[j,]<-sapply(1:sample_size, 
                    function(i) 
                      rbinom(1, 1, probability(
                        beta[[j]],
                        X[i,]
                      ) ))
}

hist(rowMeans(y_rep), yaxt='n',ylab='',xlab='', main='Histogram of proportion of students passing')
abline(v = mean(y), col='blue', lwd=4)

