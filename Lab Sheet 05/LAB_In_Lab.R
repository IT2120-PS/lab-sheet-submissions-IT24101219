getwd()

data <- read.table("Data.txt", header = TRUE, sep = ",")

fix(data)

names(data) <- c("X1", "X2")

attach(data)

histogram <- hist(X2, main = "Histogram of X2", breaks = seq(130,270, length = 8))

histogram

breaks <- histogram$breaks
breaks

freq <- histogram$counts
freq

mid <- histogram$mids
mid

classes <- c()

for (i in 1:length(breaks) - 1) {
  classes[i] <- paste0("{", breaks[i], ",", breaks[i+1], "}")
}

cbind(classes = classes, frequency = freq)


lines(mid, freq)

cum.freq <- cumsum(freq)

new <- c()

for (i in 1:length(breaks)) {
  if(i == 1){
    new[i] = 0
  } else{
    new[i] = cum.freq[i-1]
  }
}

new

plot(breaks, new, type = "o", main = "freq polygon for no of shareholders", 
     ylab = "Cumulative Frequency", xlab = "Shareholder",
     ylim=c(0, max(cum.freq)))