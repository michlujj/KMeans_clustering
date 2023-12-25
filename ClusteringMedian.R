# K means (Partitioning) clustering on Organic Food purchase Indicator ANL305 assignment
# data source: https://www.kaggle.com/datasets/papercool/organics-purchase-indicator/code

rm(list = ls()) # to clear the environment on R console

library(ggplot2)
library(tidyverse) # for data manipulation
library(gridExtra)
library(GGally)
library(cluster) # clustering algorithms 
library(factoextra) # clustering algorithms & visualization
library(corrplot) # draw a correlation plot 

# to load dataset in
df <- read.csv('C:/Users/miche/Desktop/RDataAnalysis/Clustering/organics.csv')

# to view variables types(int or character) and total obs. in data
# we can see that Age, Loyalty card tenure, Affluence Grade are detected as character cols.
# we should change this character columns to numeric so that R can detect missing values
glimpse(df)  

# there are 3 types of values: '.', blanks and N/A, 'nA'
# R can only detect NA as missing value and not 'N/A' or 'nA' or 'na'
head(df, 15)

# Thus, need to replace both "." and blanks with "NA" to make sure that R recognizes all as missing values.
# to change Age, Loyalty card tenure & Affluence Grade to numeric type
df$Loyalty.Card.Tenure <- as.numeric(df$Loyalty.Card.Tenure)
df$Loyalty.Card.Tenure

df$Age <- as.numeric(df$Age)
df$Age

df$Affluence.Grade <- as.numeric(df$Affluence.Grade)
df$Affluence.Grade

# to double check that columns <chr> have been changed to numeric types
glimpse(df) 

# to display preview of data
# we see that Gender,Geographic region, Television region, Neighborhood Cluster 7 level contains 'blanks'
head(df, 10)

# to check if dataset contains any duplicated values
duplicated(df)
sum(duplicated(df)) # there are no duplicated values

# to generate descriptive statistics in each variable columns
summary(df) 

# to create a visual that represents the missing values
library(VIM)  
mice_plot <- aggr(df, col=c('darkblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# to see total no. of customers who purchase/do not purchase organic food
table(df$Organics.Purchase.Indicator)

# to generate probabilities of outcome variable 'Organic.Purchase.Indicator'
prop.table(table(df$Organics.Purchase.Indicator))

# to create a visual on customers buy/do not buy organic food
ggplot(df, aes(x=Organics.Purchase.Indicator, fill = Organics.Purchase.Indicator)) +
  geom_bar() + labs(title='Organic Purchase Indicator')

# to select out columns for k-means clustering into a new dataframe
df2 <- df[c('Affluence.Grade','Age','Loyalty.Card.Tenure','Organics.Purchase.Indicator','Total.Spend')]

# to display preview of new dataframe
head(df2, 10)

# To create new dataframe to replace all NAs with respective column Medians
df3 <- df2  

# Replace NAs with column Median in their respective columns columns (works when dataset is all numeric) Using for-Loop
for(i in 1:ncol(df2)) {                                   
  df3[ , i][is.na(df3[ , i])] <- median(df3[ , i], na.rm = TRUE)
}
df3

# to count total missing values in each column of dataset
# we see that all NAs have been replaced by respective column Medians
sapply(df3, function(x) sum(is.na(x)))

# to create two plots side by side
par(mfrow=c(1,2)) 
# to visualize on histograms numeric variables in dataset 
hist(df3$Affluence.Grade, main="Affluence Grade", col ="light blue")
hist(df3$Age, main="Age", col ="light blue")
hist(df3$Loyalty.Card.Tenure, main="Loyalty Card Tenure", col ="light blue")
hist(df3$Organics.Purchase.Indicator, main="Organics Purchase Indicator", col ="light blue")
hist(df3$Total.Spend, main="Total Spend", col ="light blue")

# to examine strong correlation between 2 variables (if there is any, unfort. there isn't)
#ggplot(df3, aes(x = Total.Spend, y = Age)) +
 # geom_point() +
 #geom_smooth(method = 'lm', se = FALSE) +
 # theme_bw()

# to create two plots side by side
par(mfrow=c(1,2))
# to check for Outlier values for each variable using boxplots
boxplot(df3$Affluence.Grade, col="pink", ylab="AffluenceGrade")
boxplot(df3$Loyalty.Card.Tenure, col="pink", ylab="Loyalty Card Tenure")
boxplot(df3$Total.Spend, col="pink", ylab="Total Spend")
boxplot(df3$Organics.Purchase.Indicator, col="pink", ylab="Organics Purchase Indicator")
boxplot(df3$Age, col="pink", ylab="Age")


# *To remove Outliers in Affluence Grade, Loyalty Card Tenure, Total Spend using Interquartile Range calculation
# Interquartile range for Affluence Grade (3rd Qu.- 1st Qu.)
summary(df3$Affluence.Grade)

IQR_Affluence.Grade= 10-6
Upfen_Affluence.Grade = 10+1.5*IQR_Affluence.Grade
Upfen_Affluence.Grade


summary(df3$Loyalty.Card.Tenure)
IQR_Loyalty.Card.Tenure= 8-4
Upfen_Loyalty.Card.Tenure = 8+1.5*IQR_Loyalty.Card.Tenure
Upfen_Loyalty.Card.Tenure


summary(df3$Total.Spend)
IQR_Total.Spend = 6000 - 0.01
Upfen_Total.Spend = 6000 + 1.5*IQR_Total.Spend
Upfen_Total.Spend

# to create df4, new dataframe with '&' function to join all variables in the dataset
summary(df3)
df4 <- subset(df3, Affluence.Grade <= 16 & Age <= 79 & Loyalty.Card.Tenure <= 14 & 
                Total.Spend <= 14999.99 & Age <= 79 & Organics.Purchase.Indicator <= 1)

# to check that all Outlier values have been removed from Affluence Grade, Loyalty Card tenure and Total Spend before Clustering
par(mfrow=c(1,2))
boxplot(df4$Affluence.Grade, col="orange", ylab="Affluence Grade")
boxplot(df4$Age, col="orange", ylab="Age")
boxplot(df4$Loyalty.Card.Tenure, col="orange", ylab="Loyalty Card Tenure")
boxplot(df4$Total.Spend, col="orange", ylab="Total Spend")
boxplot(df4$Organics.Purchase.Indicator, col="orange", ylab= "Organics Purchase Indicator")

# to build a correlation matrix to understand r/s between each attributes
corrplot(cor(df4), type = 'upper', method = 'number', tl.cex = 0.7)

# Now that dataset is clean with all Outlier values removed, new descriptive statistics values
summary(df4)

# from data summary and statistics, to use scale function to normalize data
organic <- as.data.frame(scale(df4))
head(organic)

# there are 3 methods used to determine Optimal number of clusters or k= ?
# Determining Optimal clusters (k) Using Elbow method (First method)
fviz_nbclust(x = organic, FUNcluster = kmeans, method = 'wss' )

# or 
  wssplot <- function(organic, nc = 15, set.seed = 1234){
    wss <- (nrow(organic) - 1)*sum(apply(organic, 2, var))
    for(i in 2:nc) {
      set.seed(1234)
      wss[i] <- sum(kmeans(x = organic, centers = i, nstart = 25)$withinss)
    }
    plot(1:nc, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within Group Sum of Square',
         main = 'Elbow Method Plot to Find Optimal Number of Clusters', frame.plot = T,
         col = 'blue', lwd = 1.5)
  }
  
  wssplot(organic)

# Determining Optimal clusters (k) Using Average Silhouette Method (2nd method)
fviz_nbclust(x = organic,FUNcluster = kmeans, method = 'silhouette' )

# To compute gap statistic, Gap-Static used for finding the optimal value K (3rd method)
# set.seed(123)
# gap_stat <- clusGap(x = organic, FUN = kmeans, K.max = 15, nstart = 25, B = 50 )

# Print the result
# print(gap_stat, method = "firstmax")


# plot the result to determine the optimal number of clusters (3rd method).
# fviz_gap_stat(gap_stat)

# when k= 2, by using the above 2 methods to determine
set.seed(123)
organic_K2 <- kmeans(organic, centers = 2, nstart = 25)
print(organic_K2)

# to visualise k=2 cluster created
fviz_cluster(organic_K2, data = organic)



# https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
# https://towardsdatascience.com/data-cleaning-with-r-and-the-tidyverse-detecting-missing-values-ea23c519bc62
# https://www.datacamp.com/community/tutorials/k-means-clustering-r
# https://www.guru99.com/r-k-means-clustering.html
# outlier detection: https://statsandr.com/blog/outliers-detection-in-r/
# https://rpubs.com/Mentors_Ubiqum/removing_outliers
# How to : https://rpubs.com/abdul_yunus/Kmeans_Clustering
# *Remove outliers multiple columns in R:https://quick-adviser.com/how-do-i-remove-multiple-outliers-in-r/
