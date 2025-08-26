setwd("C:\\Users\\it24101219\\Desktop\\IT24101219")

Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = "")

fix(Delivery_Times)

names(Delivery_Times) = "Time_in_mins"

attach(Delivery_Times)

summary(Delivery_Times)

histogram <- hist(Time_in_mins, main = "Histogram for Delivery Minutes", breaks = seq(20,70, length = 9), right = FALSE)
#Most of the delivery times are in between 30 - 60 minutes


freq <- histogram$counts
breaks <- histogram$breaks

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

plot(breaks, new, type = "o", main = "Cumulative Frequency Polygon For Delivery Times", 
     ylab = "Cumulative Frequency", xlab = "Delivery Time",
     ylim=c(0, max(cum.freq)))