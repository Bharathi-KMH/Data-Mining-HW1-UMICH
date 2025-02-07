#Part 1

#Set the directory to retrieve the necessary data sets
getwd()
# Its in the correct directory

#(a) Load the data set and name it su 
su = read.delim('Su_raw_matrix.txt') 

#Read the data set
head(su) 

#(b) To get the mean of Liver_2.CEL column
mean(su$Liver_2.CEL, na.rm=TRUE) 
# The mean is 241.8246

# To get the standard deviation of Liver_2.CEL column
sd(su$Liver_2.CEL) 
# The standard deviation is 1133.352

#(c) To get the average and total values of each column.
mean_su_column = colMeans(su[sapply(su, is.numeric)], na.rm=TRUE)
mean_su_column

colSums(su,na.rm=TRUE)

#Part 2

# Generate 10000 numbers with mean=0, and Standard deviation=0.2
data = rnorm(10000, mean=0, sd=0.2)

#plot histogram with mean=0 & sigma=0.2
hist(data, breaks=40, col="red", xlim=c(-3,3), ylim=c(0,1000),
     main="Histogram_0.2", col.main="blue")

# Generate 10000 numbers with mean=0, and Standard deviation=0.5
data_1 = rnorm(10000, mean=0, sd=0.5)

#plot histogram with mean=0 & sigma=0.5
hist(data_1, breaks=40, col="green", xlim=c(-3,3), ylim=c(0,1000),
     main="Histogram_0.5", col.main="blue")

#Histogram for sigma 0.2 is much taller and narrower than the histogram for sigma 0.5 
#This difference is due to standard deviation being smaller which will cause the data points to cluster tightly around the mean. 


#Part 3

#Loading the required package
library(ggplot2)

#(a)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))

#(b) Overlaid Histogram
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

#(c) Interleaved histograms
ggplot(dat, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")

#(d) Density plots
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()

#(e) Density plots with semitransparent fill
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)

#(f) To plot all 4 types of histogram

# Load in the data set
data.file <- file.path('diabetes_train.csv')
diabetes <- read.csv(data.file, header = TRUE, sep = ',')
head(diabetes)

#Overlaid Histogram
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

#Interleaved Histogram
ggplot(diabetes, aes(x=mass, fill=class)) + geom_histogram(binwidth=.5, position="dodge")

#Density plots
ggplot(diabetes, aes(x=mass, colour=class)) + geom_density()

#Density plots with semitransparent fill
ggplot(diabetes, aes(x=mass, fill=class)) + geom_density(alpha=.3)

#Part 4

#Load in the data set
data_4 <- file.path('titanic.csv')
passengers <- read.csv(data_4, header = TRUE, sep = ',')
head(passengers)

#Load Library
library(tidyverse)

#(a)
passengers %>% drop_na() %>% summary()
#The %>% operator is used to pipe the passengers dataset into the drop_na() function, which removes any rows with missing values. 
#Then the passenger dataet is pipe to summary function to provide statistics for each column

#(b)
passengers %>% filter(Sex == "male")
# The filter function filters out a dataset which contains only male in the 'Sex' Column

#(c)
passengers %>% arrange(desc(Fare))
#The arrange function arranges the dataset in decending order according to the 'Flare' column

#(d)
passengers %>% mutate(FamSize = Parch + SibSp)
#The mutate function creates a new column called 'FamSize' by adding 'Parch' column and 'SibSp'
#Parch column represents the number of parents or children abroad 
#The 'SibSp' column represents the number of siblings or spouse abroad. 
#Combining these 2 columns will provide the family size for each passenger

#(e)
passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))
#The group_by function groups the data by Sex
#The summarise function calculates the mean fare for each sex and the total number of survivors for each sex.
#The mean fare for females and males were 44.5 and 25.5 respectively
#The number of survivors were higher in female(233) than in male(109)


#Part 5

quantile(diabetes$skin, probs = c(.1, .3, .5, .6))
#The skin attributes for 10% percentile is 0, 30% percentile is 10, 50% percentile is 23 and 60% percentile is 27
