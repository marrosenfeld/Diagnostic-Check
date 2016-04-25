L<-20
for (j in 1:L) {
  beta[[j]] <- parameter[sample(1:grid_size,1,prob=post),]
}


par(new=F)
plot(1:25, rep(0,25), type="l", xlab = 'Time', ylab = 'Avgres', ylim = c(-0.4,0.4) )
for (j in 1:L){
  avgs <- c()
  for (t in 1:25){
    sum <- 0
    for (dog in 0:29){
      # print(dog*25 + t)
      
      sum = sum + (y[dog*25 + t,1] - probability(beta[[j]], X[dog*25 + t,]))   
    }
    avg <- sum/30
    avgs <- append(avgs,avg)
  }
  lines(1:25, avgs, type="l" )
  
}

L<-1000
for (j in 1:L) {
  beta[[j]] <- parameter[sample(1:grid_size,1,prob=post),]
}

m = matrix(nrow = L, ncol = 25)
for (j in 1:L){
  avgs <- c()
  for (t in 1:25){
    sum <- 0
    for (dog in 0:29){
      # print(dog*25 + t)
      
      sum = sum + (y_rep[j,dog*25 + t] - probability(beta[[j]], X[dog*25 + t,]))   
    }
    avg <- sum/30
    avgs <- append(avgs,avg)
  }
  m[j,] = avgs
}

super = c()
sub = c()

for (i in 1:25){
  super = append(super,quantile(m[,i],c(0.95)))
  sub = append(sub,quantile(m[,i],c(0.05)))
}

lines(1:25, super, type="l", lty=2)
lines(1:25, sub, type="l", lty=2)
