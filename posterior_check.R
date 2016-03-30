library(boot)

n<-length(nrow)
L<-200
grid_size <- length(beta0) * length(beta1) * length(beta2)
sample_size <- dim(X)[1]
  
beta <- NULL 

probability <- function(beta,X){
  inv.logit(as.matrix(X)%*%(as.matrix(t(beta))))
}

for (j in 1:L) {
  beta[[j]] <- parameter[sample(1:125000,1,prob=post),]
}

# y_rep <- NULL
# for (j in 1:L) {
#   y_rep[[j]] <- sapply(1:sample_size, 
#                        function(i) 
#                          rbinom(1, 1, probability(
#                                beta[[j]],
#                                X[i,]
#                          ) ))
# }

y_rep <-NULL
y_rep <- matrix(nrow=200, ncol=sample_size)
for (j in 1:L){
  y_rep[j,]<-sapply(1:sample_size, 
                             function(i) 
                               rbinom(1, 1, probability(
                                 beta[[j]],
                                 X[i,]
                               ) ))
}

#number of schocks per replication
hist(apply(y_rep, 1, function(x) length(which(x==1)))/25)


