library(boot)

L<-1000
grid_size <- length(beta0) * length(beta1) * length(beta2)
sample_size <- dim(X)[1]
  
beta <- NULL 

probability <- function(beta,X){
  inv.logit(as.matrix(X)%*%(as.matrix(t(beta))))
}

for (j in 1:L) {
  beta[[j]] <- parameter[sample(1:grid_size,1,prob=post),]
}

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

#mean and stdstd of number of shocks
all_mean_shocks <- c()
all_std_shocks <- c()
for (j in 1:L){
  dogs = matrix(y_rep[j,], ncol=25)
  shocks = apply(dogs, 1, function(x) length(which(x==1)))
  avg = mean(shocks)
  std = sd(shocks)
  all_mean_shocks <- c(all_mean_shocks,avg)
  all_std_shocks <- c(all_std_shocks, std)
  
}
hist(all_mean_shocks, yaxt='n',ylab='',xlab='', main='Histogram of mean number of shocks')
abline(v = 7.8, col='blue', lwd=4)
hist(all_std_shocks, yaxt='n',ylab='',xlab='', main='Histogram of std of number of shocks')
abline(v = 2.58, col='blue', lwd=4)
#hist(apply(y_rep, 1, function(x) length(which(x==1)))/25)

prop_avoidances <- matrix(nrow=20, ncol=25)
for (j in 1:20){
  #print(dogs)
  dogs = matrix(y_rep[j,], ncol=25, byrow = TRUE)
  for (i in 1:25){
    avoidances = length(which(dogs[,i]==0))
    prop_avoidances[j,i] = avoidances / 30
  }

}
par(new=F)
plot(1:25, prop_avoidances[1,], type="l", ylim = c(0,1), xlab = 'Time', ylab = 'Avg avoidances', hei )
for (j in 1:20){
  lines(1:25, prop_avoidances[j,], type="l", ylim = c(0,1))
}
