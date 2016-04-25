
fit2 <- glm(input$y ~ 0 + input$shocks+input$avoidances,family=poisson(link=='log'), data=input)
summary(fit2)
predict.glm(fit2)

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
  #loglik <- t(y) %*% exp(x%*%beta) - exp(sum(x%*%beta))
  loglik <- sum(y*(x%*%beta) - exp(x%*%beta))
  return(loglik)
}

beta1<-seq(from=-0.35,to=-0.15,length=200)
beta2<-seq(from=-0.21,to=-0.01,length=200)
parameter<-expand.grid(beta1,beta2)

input$intercept<-1
X = input[,c(1,2)]
y = input[3]

B<-nrow(parameter)
print(B)
post<-numeric(B)
for (i in 1:B) {
  post[i]<-logl(parameter[i,],X, y)
}

max(post)
parameter[which(post==max(post)),] # the mode of the estimate, should be approximately equal to the MLE.

L<-1000
grid_size <- length(beta1) * length(beta2)
sample_size <- dim(X)[1]

beta <- NULL 

probability <- function(beta,X){
   exp(as.matrix(X)%*%(as.matrix(t(beta))))
}

for (j in 1:L) {
  beta[[j]] <- parameter[sample(1:grid_size,1,prob=exp(post)),]
}

betas = matrix(nrow=L, ncol=2)
for (j in 1:L){
  betas[j,] = c(beta[[j]][1,1],beta[[j]][1,2])
}

plot(betas[,1],betas[,2],xlim = c(-0.35,-0.15),ylim=c(-0.2,0), xlab = "beta1", ylab="beta2")

y_rep <-NULL
y_rep <- matrix(nrow=L, ncol=sample_size)
for (j in 1:L){
  y_rep[j,]<-sapply(1:sample_size, 
                    function(i) 
                      rbinom(1, 1, probability(
                        beta[[j]],
                        X[i,]
                      ) ))
}