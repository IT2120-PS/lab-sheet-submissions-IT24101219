setwd("C:\\Users\\dinin\\Downloads\\Lab 04-20250820")
getwd()

data <- read.table("DATA 4.txt", header = TRUE, sep = " ")


fix(data)

attach(data)

##Part 2
##Part (a)
##Obtaining Box Plots
boxplot (X1,main="Box plot for Team Attendence", outline=TRUE, outpch=8, horizontal=TRUE)
boxplot (X2, main="Box plot for Team Salary", outline=TRUE, outpch=8,horizontal=TRUE)
boxplot (X3, main="Box plot for Years", outline=TRUE, outpch=8, horizontal=TRUE)

##Obtaining Histogram
hist(X1,ylab="Frequency",xlab="Team Attendence",main="Histogram for Team Attendence")
hist(X2,ylab="Frequency",xlab="Team Salary", main="Histogram for Team Salary")
hist(X3,ylab="Frequency",xlab="Years",main="Histogram for Years")

#Stem & Leaf Plot
stem(X1)
stem(X2)
stem(X3)


##Part (b)
##Mean
mean (X1)
mean (X2)
mean (X3)

##Median
median (X1)
median (X2)
median (X3)

##Standard Deviation
sd(X1)
sd(X2)
sd(X3)


##Part (c)
##Getting five number summary along with mean value
summary(X1)
summary(X2)
summary(X3)

##Getting only five number summary for X1 variable
quantile(X1)

##Calling first Quartile of X1 using index value
quantile(X1) [2]

##Calling third Quartile of X1 using index value
quantile(X1) [4]


##Part (d)
##Obtaining Inter Quartile Range (IQR) of each variable
IQR (X1)
IQR(X2)
IQR (X3)


##Part 3
get.mode<-function(y){
  counts<<-table(y)
  names(counts[counts == max(counts)])
}
get.mode(X3)
table(X3)
max(counts)
counts == max(counts)
counts[counts == max(counts)]
names(counts[counts == max(counts)])

##Part 4
##Function to check the existence of outliers of a data set
get.outliers <- function(z){
  q1 <- quantile(z) [2]
  q3 <- quantile(z) [4]
  iqr <- q3 - q1
  
  ub <- q3 + 1.5*iqr
  lb <- q1 - 1.5*iqr
  
  print (paste("Upper Bound = ", ub))
  print (paste("Lower Bound = ", lb))
  print (paste("Outliers:", paste(sort(z[z<lb | z>ub]), collapse = ",")))
}
  ##Checking the outliers of a variable using the function defined above
  get.outliers (X1)
  get.outliers (X2)
  get.outliers (X3)