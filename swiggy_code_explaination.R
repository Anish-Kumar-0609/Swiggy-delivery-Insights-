
setwd("C:/Users/anish/OneDrive/Edvancer/projectdatasets")

library(ggplot2)
library(cluster)
library(factoextra)

## Objective
#strategies can Swiggy adopt to service the higher demand it faces during select hours of the day.
#List down various options and discuss one in detail, including execution and expected benefit
  
#Read raw input data
customer_order_summary_raw = read.csv("Swiggyassessment.csv", header=T)

#Rename colnames
colnames(customer_order_summary_raw) = c("Customer_Id","First_Order_DateTime","Recent_Order_DateTime","Orders_All","Orders_Last_7_days","Orders_Last_4_weeks","Amount_All","Amount_Last_7_days","Amount_Last_4_weeks","Avg_DistanceFromResturant","Avg_DeliveryTime")  

#Str data gives the type in which data is stored , this shows its treating Date as Factor so that needs to ve convereted to Date format
str(customer_order_summary_raw)

## Cleaning Data and creating new columns

customer_order_summary_raw$First_Order_Date = as.Date(customer_order_summary_raw$First_Order_DateTime ,format = "%m/%d/%y")
customer_order_summary_raw$Recent_Order_Date = as.Date(customer_order_summary_raw$Recent_Order_DateTime ,format = "%m/%d/%y")

customer_order_summary_raw$Current_Date = max(customer_order_summary_raw$Recent_Order_Date) + 1
customer_order_summary_raw$Days_Since_Last_Order = as.numeric(customer_order_summary_raw$Current_Date - 
                                                                customer_order_summary_raw$Recent_Order_Date)
customer_order_summary_raw$Days_Since_First_Order = as.numeric(customer_order_summary_raw$Current_Date - customer_order_summary_raw$First_Order_Date)

zero_order_7Days = customer_order_summary_raw[ is.na(customer_order_summary_raw$Orders_Last_7_days),]
zero_order_4Weeks = customer_order_summary_raw[ is.na(customer_order_summary_raw$Orders_Last_4_weeks),]
print(paste("For Users who had NA value in last 7 day orders, the minimum value for Recent Order placed is",min(zero_order_7Days$Days_Since_Last_Order),paste("Days"),sep = " "))
print(paste("For Users who had NA value in last 4 Week orders, the minimum value for Recent Order placed is",min(zero_order_4Weeks$Days_Since_Last_Order),paste("Days"),sep = " "))

#Avg resturant distance is negative in 44 cases, for simplicity I am calling them as 0.
#I also created Average order value (AOV) column will use this instead of total order value. 

customer_order_summary_raw$Orders_Last_7_days = ifelse(is.na(customer_order_summary_raw$Orders_Last_7_days),0,customer_order_summary_raw$Orders_Last_7_days) 

customer_order_summary_raw$Orders_Last_4_weeks = ifelse(is.na(customer_order_summary_raw$Orders_Last_4_weeks),0,customer_order_summary_raw$Orders_Last_4_weeks) 

customer_order_summary_raw$Avg_DistanceFromResturant = ifelse(customer_order_summary_raw$Avg_DistanceFromResturant<0,0,customer_order_summary_raw$Avg_DistanceFromResturant)

customer_order_summary_raw$AOV_All = round(customer_order_summary_raw$Amount_All/customer_order_summary_raw$Orders_All,0)

customer_order_summary_raw$AOV_Last_7_Days = round(ifelse(customer_order_summary_raw$Orders_Last_7_days ==0 ,0,customer_order_summary_raw$Amount_Last_7_days/customer_order_summary_raw$Orders_Last_7_days),0)

customer_order_summary_raw$AOV_Last_4_Weeks = round(ifelse(customer_order_summary_raw$Orders_Last_4_weeks ==0 ,0,customer_order_summary_raw$Amount_Last_4_weeks/customer_order_summary_raw$Orders_Last_4_weeks),0)


##Segmenting Customers

#All these have atleast 1 order but if we look at days since last order we can see then 

q1 = 100 - round(100*sum(customer_order_summary_raw$Orders_Last_7_days==0)/nrow(customer_order_summary_raw),0)


q2 = 100 - round(100*sum(customer_order_summary_raw$Orders_Last_4_weeks==0)/nrow(customer_order_summary_raw),0)


filter_data = customer_order_summary_raw[ ,c(1,4:6,10,11,15:19)]
set.seed(1234)

pca_data1 = prcomp(filter_data[,-1],center = T,scale. = T)

plot(pca_data1, type = "l",
main="Variance of PCA")

set.seed(00909)

normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x)))
}

filter_data_log = log(filter_data[,-1]+2)

filter_data_normal = as.data.frame(lapply(filter_data[,-1], normalize))


score_wss_log <- (nrow(filter_data_log)-1)*sum(apply(filter_data_log,2,var))
for (i in 2:15) score_wss_log[i] <- sum(kmeans(filter_data_log,
centers=i)$withinss)

plot(1:15, score_wss_log[1:15], type="b", xlab="Number of Clusters",
ylab="Within groups sum of squares",
main="Elbow method to look at optimal clusters for Log Data",
pch=20, cex=2)

score_wss_normal <- (nrow(filter_data_normal)-1)*sum(apply(filter_data_normal,2,var))
for (i in 2:15) score_wss_normal[i] <- sum(kmeans(filter_data_normal,
centers=i)$withinss)

plot(1:15, score_wss_normal[1:15], type="b", xlab="Number of Clusters",
ylab="Within groups sum of squares",
main="Elbow method to look at optimal clusters for Normalized Data",
pch=20, cex=2)


minvec <- sapply(filter_data[,-1],min)
maxvec <- sapply(filter_data[,-1],max)
denormalize <- function(x,minval,maxval) {
return(x*(maxval-minval) + minval)
}

set.seed(009)
km_3_cluster_normal = kmeans(filter_data_normal,3,nstart = 100)  

km_3_cluster_actual = NULL
test1 = NULL

for(i in 1:10)
{
  test1 = (km_3_cluster_normal$centers[,i] * (maxvec[i] - minvec[i])) + minvec[i]
  km_3_cluster_actual = cbind(km_3_cluster_actual,test1)
}

colnames(km_3_cluster_actual) = colnames(filter_data[-1])
print("Mean value of all variables in each cluster is given below")
km_3_cluster_actual
print("Numbers of customers in each cluster is given below")
km_3_cluster_normal$size
#km_3_cluster_normal$centers
#fviz_cluster(km_3_cluster_normal,data = filter_data_normal)

filter_order_data = filter_data[ filter_data$Orders_Last_4_weeks !=0,]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

filter_order_data_normal = as.data.frame(lapply(filter_order_data[,-1], normalize))

score_wss_order_normal <- (nrow(filter_order_data_normal)-1)*sum(apply(filter_order_data_normal,2,var))
for (i in 2:15) score_wss_order_normal[i] <- sum(kmeans(filter_order_data_normal,
                                                        centers=i)$withinss)

plot(1:15, score_wss_order_normal[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Active Users",
     pch=20, cex=2)


