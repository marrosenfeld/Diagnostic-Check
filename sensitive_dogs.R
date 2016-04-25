dfy <- data.frame(matrix(unlist(y), nrow=30, ncol=25, byrow=T))
stupid_dogs = 0
non_stupid_dogs = 0
sum_stupid_dogs = 0
sum_non_stupid_dogs = 0
for (i in 1:30){
  dog = dfy[i,]
  
  if (sum(dog[1:5]) == 5){
    #stupid
    stupid_dogs = stupid_dogs + 1
    sum_stupid_dogs = sum_stupid_dogs + sum(dog[6:25])
  }
  else{
    #no stupid
    non_stupid_dogs = non_stupid_dogs + 1
    sum_non_stupid_dogs = sum_non_stupid_dogs + sum(dog[6:25])
  }
}

t = (sum_stupid_dogs/stupid_dogs) - (sum_non_stupid_dogs/non_stupid_dogs)

all_t <- c()
for (j in 1:L){
  stupid_dogs = 0
  non_stupid_dogs = 0
  sum_stupid_dogs = 0
  sum_non_stupid_dogs = 0
  dogs = matrix(y_rep[j,], ncol=25, byrow = T)
  for (i in 1:30){
    dog = dogs[i,]
    
    if (sum(dog[1:5]) == 5){
      #stupid
      stupid_dogs = stupid_dogs + 1
      sum_stupid_dogs = sum_stupid_dogs + sum(dog[6:25])
    }
    else{
      #no stupid
      non_stupid_dogs = non_stupid_dogs + 1
      sum_non_stupid_dogs = sum_non_stupid_dogs + sum(dog[6:25])
    }
  }
  all_t <- c(all_t,(sum_stupid_dogs/stupid_dogs) - (sum_non_stupid_dogs/non_stupid_dogs))
}

hist(all_t, yaxt='n',ylab='',xlab='T1(y-rep)', main="Difference between stupid and smart dogs")
abline(v = t, col='blue', lwd=4)
