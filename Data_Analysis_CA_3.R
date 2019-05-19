
# Name: Iarlaith McLaughlin
# Student Number: L00144319
# Date: 19/05/2019
# Course: MSc. in Big Data Analysis
# Assignment: CA3 - Data Analysis
# Title: Data Analysis and Correlation Testing 

################################### Residential Property Price Index ##############################################################

# Source for average property prices by year: https://data.gov.ie/dataset/annual-new-property-prices

# First, the Residential Property Price Index is extracgted from a .px file and saved it into
# a data frame called "property_data". This file has the monthly RPPI from Jan 2005 (which it uses as it's
# base percentage of 100% with all other months given a percentage according to the base month of Jan 2005).

#install.packages("pxR")
library(pxR)
property_data <- as.data.frame (read.px("HPM06.px"))
head(property_data)

# The zoo library is used to convert year and month data to Date format
library(zoo)

# The gsub function is used to substitute the 'M' in month field to "-". This will ensure the data is in 
# the correct format for passing to the yearmon function. 
property_data$Month <- gsub('M', '-', property_data$Month)
head(property_data$Month)

# Yearmon will now convert the data into a chron (chronologica) object that can be used to transform to
# date format. 
property_data$Month <- as.yearmon(property_data$Month)
head(property_data)

# Transfoming chron object to date object
property_data$Month <- as.Date(property_data$Month)
head(property_data$Month)

# The rows the report is interested in from the px file are the RPPI index values. These can be extracted from our
# dataframe by using the subset command and using the Statistic column. 
property_data <- subset(property_data, Statistic == "Residential Property Price Index (Base Jan 2005 = 100)")
head(property_data)

# Columns in the data can also be renamed to make it easier to run commands. For simplicity the 
# "Type.of.Residential.Property" column will be renamed to "Type".
names(property_data)[names(property_data) == "Type.of.Residential.Property"] <- "Type"
colnames(property_data)
head(property_data)

# Plot of the Residential Property Price Index
plot(property_data$Month, property_data$value,
     xlab = "Year",
     ylab = "Deviation from RPPI - Jan 2005",
     main = "RPPI from Jan 2005 to March 2019",
     col = property_data$Type
) 

# The gg plot can be used for a better graph of the RPPI for all different parts of 
# the country (Type).
p1 = ggplot(na.omit(property_data), aes(x=Month, y=value, color=Type)) +
  geom_line() +
  labs(title = "All Property Price Indexes 2005-2019", x = "Year", y = "Percentage Change") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")
print(p1)

# As the report is interested in determining if a correlation exists with the Cosumer Price Index (CPI), 
# most of the Types graphed above can be excluded with emphasis on the national RRPI rates as the CPI is
# also on a national level. This is done by calling the subset cammand and determing which Types to include:
property_data_updated <- subset(property_data, Type == "National - all residential properties" | 
                          Type == "National excluding Dublin - all residential properties" |
                          Type == "Dublin - all residential properties")
  
head(property_data_updated)

# The updated RPPI can now be plotted which should only have the National RPPIs.
p2 = ggplot(property_data_updated, aes(x=Month, y=value, color=Type)) +
  geom_line() +
  labs(title = "All Property Price Indexes 2005-2019", x = "Year", y = "Percentage Change") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")
print(p2)

################################################ Consumer Price Index #########################################################

# The below will read the Consumer Price Index CSV file into a dataframe called "consumer_data". This data set
# has also used Jan of 2005 as it's start point and all other values are marked as a percentage of this value e.g 
# Jan 2005 has a value of 100 (100%) and Jan 2019 was 113.8 (13.8% increase in comparison to Jan 2005). This data set
# has many more months of data that the RPPI however, with records from Nov 1975 to April 2019.

consumer_data <- read.csv('CPI.csv', header = TRUE, stringsAsFactors = FALSE)
head(consumer_data)
nrow(consumer_data)
summary(consumer_data)
str(consumer_data)

# Again the yearmon and as.Date functions are used to change the format of the date in the data frame.
consumer_data$Date <- as.yearmon(consumer_data$Date)
consumer_data$Date <- as.Date(consumer_data$Date)
str(consumer_data)
head(consumer_data)

# The name of the date column is also amended to "Month" to correspond with the property data data frame.
names(consumer_data)[names(consumer_data) == "Date"] <- "Month"
head(consumer_data)
tail(consumer_data)

# Plot of the CPI from 1975 to 2019.
p3 = ggplot(consumer_data, aes(x=Month, y=value, color=Type)) +
  geom_line() +
  labs(title = "All Consumer Price Indexes Nov 1975 - Apr 2019 (Base Jan 2005 = 100)", 
       x = "Year", y = "Percentage Change") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
print(p3)
# This graphy is almost linear from 1975 to around 2019 with inflation steadily increasing year
# on year. This changes from 2009 onwards which corresponds with the financial crisis. 

# The consumer_data dataframe is updated to have the same date range as that of the property_data 
# dataframe ie. the CPI from Jan 2005 to Mar 2019 corresponding to the RPPI records from Jan 2005 
# to Mar 2019.
consumer_data_updated <- consumer_data[consumer_data$Month >= "2005-01-01" & consumer_data$Month <= "2019-03-01",]
summary(consumer_data_updated)
head(consumer_data_updated)

# Combination of RPPI data with CPI data using rbind to make it easier to graph the two together. 
combined <- rbind(property_data_updated,consumer_data_updated)
head(combined)

# Graph of the RPPI indexes along with the CPI from 2005 to 2019.
p4 = ggplot(combined, aes(x=Month, y=value, color=Type)) +
  geom_line() +
  labs(title = "All Property Price Indexes 2005-2019", x = "Year", y = "Percentage Change") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")
print(p4)
# There doesn't appear to be much of a correlation but further investigationis carried out in the
# correlation section below. 

##################################### POWER ANALYSIS ####################################################################################

# This section will investigate what sample size is required for a medium size effect in the two-sided
# correlation test using the conventional power of 0.90. The power is the probability of correctly 
# rejecting the null hypothesis for the alternative hypothesis which is usually either 0.80 (80%) or 0.90
# (90%) but for this report 0.90 was chose this would minimise the chance of a type 1 error (incorrectly 
# rejecting the null hypotheisis). 

# First, the Cohen.ES command is run to determine what effect size is needed for the correlation
# testing. To detect a medium level correlation between the two indexes, "r" was input for correlation test
# and "medium" for size.
cohen.ES(test = "r", size = "medium") 
# The result determined that to detect a medium correlation, an effect size of 0.3 is required. 

library(pwr)
required_sample <- pwr.r.test(r = 0.3, sig.level = .01, p = 0.9, alternative = "two.sided")
required_sample
plot(required_sample)

# The results of this test shows that an optimal amount of 158 months of RPPI and CPI data are required
# to determine if there is either a positive or negative correlation between the two indexes. However, 
# as this report is testing for a positive correlation between the two indexes the alternative can be 
# amended to "greater" which will only test one side of the curve. This should reduce the amount of 
# samples needed for the report.

required_sample_positive_cor <- pwr.r.test(r = 0.3, sig.level = .01, p = 0.9, alternative = "greater")
required_sample_positive_cor
plot(required_sample_positive_cor)

# The result shows that the optimal amount of months required to detect a medium correlation with 90% probabilty 
# of finding that correlation (power) and 1% probabilty of finding an correlation that isn't there (sig. level) is
# 139 months. 

# The CSO reports have 171 months of data from Jan 2005 to Mar 2019 so this is above the 139 months required and 
# therefore can be equal to or over 90% confident of detecting a correlation if one exists. The fact that it is known
# how many months of data have been collected (n value), this allows the function to determine what % of power the report
# will have (p value) by running the test with n inserted and p ommitted:

study_sample <- pwr.r.test(n = 171, r = 0.3, sig.level = .01, alternative = "greater")
study_sample
plot(study_sample)

# This result shows that the report can be 95.48% confident of detecting a correlation if one exists between the two indexes. 

########################### Correlation Testing - RPPI v. CPI ########################################################

#Hypothesis

#H0: There is NO relationship between the Residential Property Price Index and 
# the Consumer Price Index (Property prices versus Inflation).

#H1: There IS a relationship between the Residential Property Price Index and 
# the Consumer Price Index.

head(combined)
head(property_data_updated)         
head(consumer_data_updated)

# First, a subset of the property data is created to just have the national monthly RPPI values and stored 
# in the a national_property_data data frame.
national_property_data <- subset(property_data_updated, Type == "National - all residential properties")
head(national_property_data)
summary(national_property_data)

# The Pearson test of correlation is used to see if we can detect a correlation between the RPPI and CPI.
pearson_test <- cor.test(x = national_property_data$value, y = consumer_data_updated$value)
pearson_test
# p value of -.000001425 which is much lower than .05 so we can confirm that there is statistical significance 
# between the national RPPI (Residential Property Price Index) and the national CPI (Consumer Price Index)
# However, the correlation result is -0.3252005 which tells us that there is a very weak negative correlation.
# The pearson test assumes that the data is normally distributed to another test will be required for non-paramteric
# data.

# Kendall and Spearman tests are more robust than the Pearson correlation test when data isn't normally distributed
# and are used for non-parametric testing
kendall_test <- cor.test(national_property_data$value, consumer_data_updated$value, method = "kendall")
spearman_test <- cor.test(national_property_data$value, consumer_data_updated$value, method = "spearman")
kendall_test
spearman_test
# Both results show a weak negative correlation between the RPPI and CPI

#####################################  Normality Tests  #####################################################

# The first test we will run is the Shapiro-Wilk test of normaility. This should enable us to determine if 
# our datasets are normally distributed or not. 

shapiro.test(national_property_data$value)
shapiro.test(consumer_data_updated$value)

# The null hypothesis here is that the data sets are normally distributed. However, our p-values for 
# both of our data sets (RPPI and CPI) are well below the chosen significance level of 0.05 which means
# we have rejected the null hypothesis and both sets of data are not normally distributed. We will run
# a QQ plot to investigate further. 

#install.packages("ggpubr")
library("ggpubr")
ggdensity(national_property_data$value, 
          main = "Density plot of RPPI values (Base Jan 2005 = 100) ",
          xlab = "Values",
          ylab = "Density",
          fill = "green")

ggqqplot(national_property_data$value)
# Result shows most of the data is normalised but some outliers are present.

# Here we will run a QQ plot (Quantile-Quantile) 
library("car")
qqPlot(national_property_data$value,
       main = "QQ plot of RPPI values (Base Jan 2005 = 100) ",
       xlab = "Theoretical Quantiles",
       ylab = "Sample")
# Alost a straight line which suggests normal distribution but again we can see the 
# outliers are present which means we cannot state that distribution is normal.

summary(consumer_data_updated)

qqPlot(consumer_data_updated$value,
       main = "QQ plot of RPPI values (Base Jan 2005 = 100) ",
       xlab = "Theoretical Quantiles",
       ylab = "Sample")
# Not a straight line and most data points outside of our quantiles so not normally distributed.

# The qqnorm plot is similiar to qqPlot and should show us the same result
qqnorm(national_property_data$value)
qqline(national_property_data$value)
# mostle follow the line with a few exceptions (this is known as having a fat tail)

qqnorm(consumer_data_updated$value)
qqline(consumer_data_updated$value)

# ggscatter plot can be used to determine if a relationship is linear or not (for e.g height v. weight)
ggscatter(national_property_data, x = "Month", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Year", ylab = "National RPPI values (Base Jan 2005 = 100)")
# Results declare that relationship is non linear 

# Same test for the CPI 
ggscatter(consumer_data_updated, x = "Month", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Year", ylab = "National RPPI values (Base Jan 2005 = 100)")
# Again, results declare that relationship is non linear 

# Histograms can also show if a data set appears normalised or not.
# If normalised, they should look similar to the  normal (gaussian) distribution also 
# knows as  the bell-shaped curve. 
histogram(~value | Type, data = property_data_updated)
histogram(~value | Type, data = combined)
histogram(national_property_data$value)
histogram(consumer_data_updated$value)
# We can clearly see that in all cases, data is not normally distributed.

# Boxplots can also visualise the data sets to show if the data 
# conatains outliers or not. 
boxplot(national_property_data$value,
        main = "National RPPI from Jan 2005 to Mar 2019",
        xlab = "Deviation from base value of 100 as of Jan 2005",
        ylab = "RPPI",
        col = "orange",
        border = "black",
        horizontal = TRUE
)
# In this case it can be seen that the mean is around the 92 mark and 
# the first and third quartiles around 76 and 107 marks. This can be shown
# accurately by printing the summary of the data.
summary(national_property_data$value)

# Boxplot of RRPI 
boxplot(consumer_data_updated$value,
        main = "CPI from Jan 2005 to Mar 2019",
        xlab = "Deviation from base value of 100 as of Jan 2005",
        ylab = "CPI",
        col = "orange",
        border = "black",
        horizontal = TRUE
)

# In this case, it's clear that many outliers are present. The first quartile
# is around the 109 mark but there are many points before this (between 100% and 109%
# in our data set).
summary(consumer_data_updated$value)

# If a boxplot is ran on the entire data set for CPI from 1975 to 2019 we might a different result. 
# might be observed.
boxplot(consumer_data$value,
        main = "CPI from Nov 1975 to Apr 2019",
        xlab = "Deviation from base value of 100 as of Jan 2005",
        ylab = "CPI",
        col = "orange",
        border = "black",
        horizontal = TRUE
)
# As predicted, no outliers are present between this time frame. 
summary(consumer_data$value)

# Boxplots can also be ran for all divisions of RPPI data:
ggboxplot(property_data, x = "Month", y = "value", 
          color = "Type", 
          order = "Statistic",
          xlab = "RPPI divisions",
          ylab = "Deviation from base of 100 at Jan 2005 to Mar 2019")


################################################ ISEQ ###############################################

# This section is not contained in the report as the report aimed to determine if a correlation was 
# present between the RPPI and the CPI. However, after no meaningful correlation could be determined between
# the two, the Irish Stock Exchange (ISEQ) data was also investigated to see if a correlation existed between
# the RPPI and the ISEQ. This was purely to practise more R and the theory was that stocks on the ISEQ 
# may conatain a high level of property and a correlation between property values and stock values may exist. 

# Data Source https://finance.yahoo.com/quote/%5EISEQ/history?period1=1104537600&period2=1558134000&interval=1mo&filter=history&frequency=1mo

stock_data <- read.csv('ISEQ - Jan 2005 to May 2019.csv', header = TRUE, stringsAsFactors = FALSE)
head(stock_data)
nrow(stock_data)
summary(stock_data)
str(stock_data)

# Again the yearmon and as.Date functions are utilised to change the format of the date in the data frame.
stock_data$Month <- as.yearmon(stock_data$Month)
stock_data$Month <- as.Date(stock_data$Month)
str(stock_data)
head(stock_data)

# Combination of the RPPI data with CPI data using rbind
combined_updated <- rbind(combined,stock_data)
head(combined_updated)

p6 = ggplot(combined_updated, aes(x=Month, y=value, color=Type)) +
  geom_line() +
  labs(title = "All Indexes 2005-2019", x = "Year", y = "Percentage Change") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")
print(p6)

########################################### Correlation Testing 2 ##################################################################

#Hypothesis

# H0: There is no relationship between the Residential Property Price Index and 
#     the Irish Stock Exchange Index (Irish stock prices versus Irish property prices)

# H1: There is a relationship between the Residential Property Price Index and 
#     the Irish Stock Exchange Index

# First, the stock_ data frame is updated to have the same date range as that of the property_data dataframe ie. the 
# ISEQ from Jan 2005 to Mar 2019 corresponding to the RPPI records from Jan 2005 to Mar 2019.
stock_data_updated <- stock_data[stock_data$Month >= "2005-01-01" & stock_data$Month <= "2019-03-01",]
summary(stock_data_updated)
head(stock_data_updated)

pearson_test2 <- cor.test(x = national_property_data$value, y = stock_data_updated$value)
pearson_test2
# p value of -.000001425 which is much lower than .05. This confirms that there is statistical significance 
# between the national RPPI (Residential Property Price Index) and the national CPI (Consumer Price Index)
# The cor rate this time is 0.64 which is on the upper scale of a moderate positive correlation between the 
# RPPI and the ISEQ but as the data is non-parametric the Kendall and Spearman tests can be used again.
 
kendall_test2 <- cor.test(national_property_data$value, stock_data_updated$value, method = "kendall")
spearman_test2 <- cor.test(national_property_data$value, stock_data_updated$value, method = "spearman")
kendall_test2
spearman_test2
# Both results show that there does appear to be a moderate positive correlation between the RRPI and the ISEQ
# and the null hypothesis that no correlation exists can be rejected. 

summary(consumer_data)