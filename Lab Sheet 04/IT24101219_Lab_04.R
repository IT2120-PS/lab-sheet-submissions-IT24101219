setwd("C:\\Users\\dinin\\OneDrive - Sri Lanka Institute of Information Technology\\Desktop\\IT24101219")
getwd()
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")

fix(branch_data)
typeof(branch_data)

attach(branch_data)

boxplot(branch_data['Sales_X1'], main = "Boxplot for Sales", ylab = "Sales")

summary(Advertising_X2)

IQR(Advertising_X2)

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

get.outliers(Years_X3)
