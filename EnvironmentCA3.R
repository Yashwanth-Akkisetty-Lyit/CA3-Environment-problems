#Setting the working directory
setwd('C:/Users/yaswa/Documents/CA3-Environment-problems')
dataset1 <- read.csv("data1.csv",header=T,sep=",")
str(dataset1)
vehicle_usage <- data.frame(dataset1)
dataset2 <- read.csv("data2.csv",header=T,sep=",")
air_pollutants <- data.frame(dataset2)

#structure of vehicle usage and air pollutants datasets
str(vehicle_usage)
View(vehicle_usage)
str(air_pollutants)
View(air_pollutants)

year_quarter <- c('2009Q1', '2009Q2', '2009Q3', '2009Q4', 
                '2010Q1', '2010Q2', '2010Q3', '2010Q4', 
                '2011Q1', '2011Q2', '2011Q3', '2011Q4', 
                '2012Q1', '2012Q2', '2012Q3', '2012Q4', 
                '2013Q1', '2013Q2', '2013Q3', '2013Q4', 
                '2014Q1', '2014Q2', '2014Q3', '2014Q4', 
                '2015Q1', '2015Q2', '2015Q3', '2015Q4')

Public_Transport <- c(249774,	253075,	250744,	246068,	
                      238079,	237501,	241130,	236982,
                      234897,	230233,	232682,	226901,	
                      217131,	218130,	217916,	214984,
                      221368,	223317,	216958,	223716,
                      208912,	207854,	207573,	206872,
                      226347,	226190,	224651,	223490)

Private_Transport <- c(171496,	171659,	182730,	182131,
                        188121,	191361,	189757,	188652,	
                        245133,	244665,	248348,	248475,	
                        339507,	339298,	342363,	340240,	
                        398036,	394371,	396490,	390670,	
                        411261,	409098,	409311,	411054,	
                        445314,	447280,	451535,	445917)

Total_Pollutants <- c(136.77,	130.32,	139.15,	141.55,
                      127.115,	126.21,	129.04,	129.44,
                      119.27,	113.26,	121.73,	119.41,
                      118.26,	119.37,	122.47,	117.73,
                      112.73,	110.51,	108.81,	111.16,
                      112.99,	113.33,	111.4,	117.53,
                      113.305, 110.27,	112.79,	112.09)

final_dataset <- data.frame(year_quarter, Public_Transport, Private_Transport, Total_Pollutants)
View(final_dataset)

# Selecting the appropriate test
# checking whether the data is normally distributed or not
library(lattice)
histogram(~Total_Pollutants, data =  final_dataset, col='orange')

# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(final_dataset$Total_Pollutants)
normality_test

normality_test <- shapiro.test(final_dataset$Private_Transport)
normality_test

# Using a QQ plot to check for normality
# qqnorm function plots the sample 
# against a normal distribution
qqnorm(final_dataset$Total_Pollutants)
qqline(final_dataset$Total_Pollutants, col = 'orange')

install.packages("pwr")
library(pwr)

# checking the effective size
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

#power analysis is to check the oprtimal sample sizes
power_analysis <- pwr.r.test(n= NULL, r = 0.5, sig.level = 0.05, power = 0.95, alternative = "two.sided")
power_analysis

plot(power_analysis)

# to find the relationship between the two continous variables 
#the stastical method spearman test is used 
test <- cor.test(final_dataset$Private_Transport, 
                 final_dataset$Total_Pollutants, method = 'spearman', exact = FALSE)
test

# Here the p-value is less than 0.05 which shows that 
#there is a relation between vehicle usage rate and pollutants rate
#so I conclude my research question as 
"Vehicle usage has the effect on air quality of environment"


