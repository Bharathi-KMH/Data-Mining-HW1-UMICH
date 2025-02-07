#Part 1

#Set the directory to retrieve the necessary data sets

getwd()
su = read.delim('Su_raw_matrix.txt') 

head(su) 
Liver = su["Liver_2.CEL"] #to retrieve the dataframe column
library(dplyr)
summary(Liver)


mean(su$Liver_2.CEL, na.rm=TRUE) # mean is 241.8246
sd(su$Liver_2.CEL) # sd is 1133.352

mean_su_column = colMeans(su[sapply(su, is.numeric)], na.rm=TRUE)
mean_su_column
colSums(su,na.rm=TRUE)


# Generate 10000 numbers with mean=0, and Standard deviation=0.2
data = rnorm(10000, mean=0, sd=0.2)

#plot histogram with 40 bins
hist(data, breaks=40, col="red", xlim=c(-3,3), ylim=c(0,1000),
     main="Histogram for mean = 0 & sigma = 0.2", col.main="blue")

# Generate 10000 numbers with mean=0, and Standard deviation=0.5
data_1 = rnorm(10000, mean=0, sd=0.5)

#plot histogram with 40 bins
hist(data_1, breaks=40, col="green", xlim=c(-3,3), ylim=c(0,1000),
     main="Histogram for mean = 0 & sigma = 0.5", col.main="blue")

install.packages("ggplot2")

library(ggplot2)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))

ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

ggplot(dat, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")

ggplot(dat, aes(x=rating, colour=cond)) + geom_density()

ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)


# Load in the data set
data.file <- file.path('diabetes_train.csv')
diabetes <- read.csv(data.file, header = TRUE, sep = ',')
head(diabetes)

ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

ggplot(diabetes, aes(x=mass, fill=class)) + geom_histogram(binwidth=.5, position="dodge")

ggplot(diabetes, aes(x=mass, colour=class)) + geom_density()

ggplot(diabetes, aes(x=mass, fill=class)) + geom_density(alpha=.3)

#part 4 of hw1

# Load in the data set
data_4 <- file.path('titanic.csv')
passengers <- read.csv(data_4, header = TRUE, sep = ',')
head(passengers)

#Load Library
install.packages("tidyverse")
library(tidyverse)

passengers %>% drop_na() %>% summary()

passengers %>% filter(Sex == "male")

passengers %>% arrange(desc(Fare))

passengers %>% mutate(FamSize = Parch + SibSp)

passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))

#part 5

quantile(diabetes$skin, probs = c(.1, .3, .5, .6))

