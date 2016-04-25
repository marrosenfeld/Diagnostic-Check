data <- read.csv(file="dog_data.csv",head=TRUE,sep=",")
data[is.na(data)] <- 0


xy <- c()
for(i in 1:nrow(data)) {
  dog <- data[i,1:25]
  previous_shocks <- 0
  previous_avoidances <- 0
  for(j in 1:length(dog)){
    xy <- append(xy, previous_avoidances)
    xy <- append(xy, previous_shocks)
    xy <- append(xy, dog[j][1,1])
    if(dog[j] == 1){
      previous_shocks <- previous_shocks + 1
    } else{
      previous_avoidances <- previous_avoidances + 1
    }
  }
}

xy <- matrix(xy,ncol = 3,byrow = TRUE)
input = as.data.frame(xy)
colnames(input)[1] <- 'avoidances'
colnames(input)[2] <- 'shocks'
colnames(input)[3] <- 'y'



