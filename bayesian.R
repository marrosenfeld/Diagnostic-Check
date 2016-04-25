library(LearnBayes)
library(boot)
library(base)

logl <- function(theta,x,y){
  y <- y
  x <- as.matrix(x)
  
  beta <- t(theta)
  
  # Use the log-likelihood of the Bernouilli distribution, where p is
  # defined as the logistic transformation of a linear combination
  # of predictors, according to logit(p)=(x%*%beta)
  loglik <- exp(t(y) %*% (x%*%beta) - sum(log(1 + exp(x%*%beta))))
  return(loglik)
}

beta0<-seq(from=1.60,to=2.00,length=50)
beta1<-seq(from=-0.55,to=-0.15,length=50)
beta2<-seq(from=-0.41,to=-0.01,length=50)
parameter<-expand.grid(beta0,beta1,beta2)

input$intercept<-1
X = input[,c(4,1,2)]
y = input[3]

B<-nrow(parameter)
print(B)
post<-numeric(B)
for (i in 1:B) {
  post[i]<-logl(parameter[i,],X, y)
}

max(post)
parameter[which(post==max(post)),] # the mode of the estimate, should be approximately equal to the MLE.


