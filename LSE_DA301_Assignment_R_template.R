## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
tutle<- read.csv(file.choose(), header=T)
head(tutle, 5)
# Install and import Tidyverse.
install.packages("tidyverse")
library('tidyverse')

# sense check the dataset.
summary(tutle)
#Check for NA values
sum(is.na(tutle))

#Replace NA values with 0
tutle[is.na(tutle)] = 0

#Validate the replaced values
sum(is.na(tutle))

# Print the data frame.
print(tutle)

# Create a new data frame from a subset of the sales data frame.
tut_sub<-select(tutle, -Ranking, -Year, -Genre, -Publisher)
# Remove unnecessary columns. 


# View the data frame.
head(tut_sub, 5)
View(tut_sub)

# View the descriptive statistics.
summary(tut_sub)
as_tibble(tut_sub)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
#EU_sales on products
qplot(Product, EU_Sales , 
      geom=c('point', 'smooth'), 
      data=tut_sub, 
      main= 'Europe Sales')

#NA_sales on products
qplot(Product, NA_Sales , 
      geom=c('point', 'smooth'), 
      data=tut_sub, 
      main= 'North America Sales')

## 2b) Histograms
# Create histograms.
#EU_sales on products
hist(tut_sub$Product, 
     main= 'Sales Performance', 
     xlab= 'Product', 
     ylab= 'Total count', 
     col= 'grey', 
     colours=tut_sub$EU_Sales)

#NA sales histogram
hist(tut_sub$Product, 
     main= 'Sales Performance', 
     xlab= 'Product', 
     ylab= 'Total count', 
     col= 'grey',
     colours=tut_sub$NA_Sales)


## 2c) Boxplots
# Create boxplots.
#Global sales performance on products and platform class
qplot(Platform, Product, fill=Global_Sales, data=tut_sub,geom='boxplot')

#North America sales performance on products and platform class
qplot(Platform, Product, fill=NA_Sales, data=tut_sub,geom='boxplot')

#North America sales performance on products and platform class
qplot(Platform, Product, fill=EU_Sales, data=tut_sub,geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

##There were a few spike in sales performance on a few products on The
##nintendo categories both in Europe and North America. Although, PC games experienced
##a random spike in sales performance. Futher analysis needs to be carried on factors
##influencing these sales such as age group of gamers, holiday period or marketing 
##impacts on those products.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(tutle)

# Check output: Determine the min, max, and mean values.
summary(tutle)

# View the descriptive statistics.
as_tibble(tutle)

###############################################################################

# 2. Determine the impact on sales per product_id.
#convert product from int to char
tutle2<- mutate(tutle, Product=as.factor(Product))
#confirm changes
as.tibble(tutle2$Product)

#Rename product to product_id
colnames(tutle2)[colnames(tutle2) =='Product']<-'Product_id'
head(tutle2, 2)

#Create a subset of the df
sale_prod<- subset(tutle2, select = 
                     c(Product_id, Platform, NA_Sales, 
                       EU_Sales, Global_Sales))
head(sale_prod, 3)
#Check for NA values
sum(is.na(sale_prod))

## 2a) Use the group_by and aggregate functions.
# Create insightful summaries of the data set.
library(tidyverse)
library(skimr)
# Create insightful reports on the data set.
library(DataExplorer)

DataExplorer::create_report(sale_prod)

# Group data based on Product and determine the sum per Product.
Prod_sum<- aggregate(Global_Sales~Product_id, sale_prod, sum)

# View the data frame.
View(Prod_sum)

# Explore the data frame.
#Round to 2 dec point
Prod_sum <- round(Prod_sum, 2)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Prod_sum$Product_id, Prod_sum$Global_Sales, geom = point)
# Create histograms.
hist(Prod_sum$Global_Sales, main= 'Global Sales Performance')

# Create boxplots.
boxplot(Prod_sum$Product_id, Prod_sum$Global_Sales, main= 'Sales Performance')

###############################################################################


# 3. Determine the normality of the data set.
# Call the function to calculate the mean.
mean(Prod_sum$Global_Sales) 

# Call the function to calculate the median.
median(Prod_sum$Global_Sales)


# Determine the minimum and maximum value.
min(Prod_sum$Global_Sales)  
max(Prod_sum$Global_Sales) 


# Range = Max - Min.
max(Prod_sum$Global_Sales)- min(Prod_sum$Global_Sales) 


# Calculate Q1 and Q3.
quantile(Prod_sum$Global_Sales, 0.25)  
quantile(Prod_sum$Global_Sales, 0.75)


# Use the summary() function.
summary(Prod_sum$Global_Sales)


# Calculate IQR.
IQR(Prod_sum$Global_Sales)  
# OR:
50.09 - 28.76


# Determine the variance.
var(Prod_sum$Global_Sales)  


# Return the standard deviation.
sd(Prod_sum$Global_Sales)  
## 3a) Create Q-Q Plots
# Create Q-Q Plots.
#North America sale plot
qqnorm(tut$NA_Sales,
       +        col='red',
       +        xlab="EU sale Value",
       +        ylab='Time')
#Europe sale
qqnorm(tut$EU_Sales,
       +        col='blue',
       +        xlab="EU sale Value",
       +        ylab='Time')

#Sale Performance with qline
qqline(tut$EU_Sales,
       +        col='black',
       +        lwd=2)

qqline(tut$NA_Sales,
       +        col='green',
       +        lwd=2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.
shapiro.test(tut$EU_Sales)
shapiro.test(tut$NA_Sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(tut$EU_Sales) 

skewness(tut$NA_Sales)

#kurtosis check
kurtosis(tut$NA_Sales)
kurtosis(tut$EU_Sales)



## 3d) Determine correlation
# Determine correlation.
cor(tut$NA_Sales, tut$EU_Sales)

round (cor(tut),
       +        digits=2)

###############################################################################

# 4. Plot the data
install.packages('corrplot')
library(corrplot)

#Create a df
cor_matrix<- round (cor(tut), digits=2)

#plot a corrplot chart

corrplot(cor_matrix, method = 'square')

# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
##There is a close relation between north america sales to eu sales because they 
##both have a positive correlation. The values are closer to zero than it is to one
##The correlation between both sales is 0.7055236 which means it is a positive one


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
tut_p<- read.csv(file.choose(), header=T)
summary(tut_p)

# View data frame created in Week 5.
View(tut_p)




# Determine a summary of the data frame.
#create a subset of the col needed
tut2<- subset(tut_p, select= c(Year, NA_Sales, EU_Sales, Global_Sales))
head(tut2, 3)



###############################################################################

# 2. Create a simple linear regression model
#check for correlation 


## 2a) Determine the correlation between columns
cor(tut2)
# Create a linear regression model on the original data.
mod1<- lm(NA_Sales~Year, data=tut2)
mod2<- lm(EU_Sales~Year, data=tut2)
mod3<- lm(Global_Sales~Year, data=tut2)

#View the summary data for relance
summary(mod1) #Year has a probability val of 95% impact on NA_sales
summary(mod2) #year is a less probablity value of 40% for EU_sales


summary(mod3) #Year has a decent probability val of 71% for
#globalsales



## 2b) Create a plot (simple linear regression)
# Basic visualisation.

#plot residuals
plot(mod1$residuals)
plot(mod3$residuals)

#plot NA_Sales
plot(tut2$Year, tut2$NA_Sales)
coefficients(mod1)

#plot EU_Sales

plot(tut2$Year, tut2$EU_Sales)
coefficients(mod2)

#plot Global_Sales
plot(tut2$Year, tut2$Global_Sales)
coefficients(mod3)

# 4. Compare the two models

# Arrange plot with the par(mfrow) function.
par(mfrow=c(2, 1))


# Compare both graphs (mod1 and mod2).
par(mfrow=c(2, 1))
plot(tut2$Year, tut2$NA_Sales)
abline(coefficients(mod1), col='red')
plot(tut2$Year, tut2$Global_Sales)
abline(coefficients(mod3), col='blue')
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
mod4 <- lm(tut2$NA_Sales ~ tut2$EU_Sales)
# View the summary stats.
summary(mod4)
# Multiple linear regression model.
# Create a visualisation to determine normality of data set.
qqnorm(residuals(mod4))
qqline(residuals(mod4), col='blue')
###############################################################################

# 4. Predictions based on given values

NA_Sales_sum<- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_sum<- c(23.80, 1.56, 0.65, 0.97, 0.52)
pred_d<- data.frame(NA_Sales_sum = c(34.02, 3.93, 2.73, 2.26, 22.08), 
                    EU_Sales_sum = c(23.80, 1.56, 0.65, 0.97, 0.52))

#create a df for top 5 rows from tut2
dup<- head(tut2, 5)

#Create a model based on the dup df
model5 <- lm(dup$NA_Sales ~ dup$EU_Sales)

#Apply the model to the estimated values provided
prediction<- predict(model5, newdata = pred_d)

#add values to the pred_d df
pred_d$Predicted_sale<- prediction

#view the data
pred_d

# Compare with observed values for a number of records.
#create a random selection from tut2 df
random_rows<-sample(nrow(tut2), 5)
#get the df
random_selection<- tut2[random_rows,]
#view the date. note: data changes each time code is passed
random_selection

#create a model for the random df
model7 <- lm(random_selection$NA_Sales ~ random_selection$EU_Sales)

#predict the new observation
prediction3<- predict(model7, newdata = random_selection)

random_selection$Random_pred_sale<- prediction3

#view the df
random_selection

#join the two df
obsv_pred<- cbind(pred_d, random_selection)
obsv_pred
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#plot observations
##predicted sales for observed data kept rising as 
##the year progressed.
##Although, there was a spike in sales performance 
##across the continents. To get the complete insight on factor
##contributing to the spike in sale, a time series analysis
##needs to be carried on the data. Accurate date format will be needed
##to understand the trends, season and external factors that might
##have contributed to the spikes in sales performance across the regions.

##As noticed, calender year did not have much impact on 
##North american sales.
##Further analysis and test data need to be carried out to 
##fully understand the sale pattern and predictions.



###############################################################################
###############################################################################




