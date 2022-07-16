# Statistics Project
#installing packages
options("install.lock"=FALSE)

install.packages("ggthemes")
install.packages("psych")
install.packages("relaimpo")
install.packages("tidyverse")
install.packages("funModeling")
install.packages("magitrr")
install.packages("skimr")
install.packages("caret")
install.packages("cowplot")

suppressMessages(library(tidyverse))        # data manipulation and plots
suppressMessages(library(funModeling))      # overview stats
library(magrittr)                           # to use pipes
library(skimr)                              # to get a quick summary table
library(caret)                              # to create the partition for training/test datasets
library(cowplot)
library(psych)
options(scipen = 999)                              # turn off scientific notation for numbers

options(repr.plot.width=12, repr.plot.height=8)    # set universal plot size


data = read.csv('C:\\Users\\Venky\\Dropbox (CSU Fullerton)\\PC\\Desktop\\CSUF\\ISDS 540 - Daoji Li - Mon -Wed - 5.30\\Project\\insurance.csv')
head(data)

# denote factor variables

data$sex <- factor(data$sex)
data$smoker <- factor(data$smoker)
data$region <- factor(data$region)
data$children <- factor(data$children)

# check for missing values
sum(is.na(data))     # there are no missing value

# check data types
str(data)           # all columns are assigned correct data types

#EDA

skim(data)


ggplot(data = data,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges per Region")

describeBy(data$charges,data$region)

# Smoking status
describeBy(data$charges,data$smoker)

ggplot(data = data,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Smoking Status")

# By gender
describeBy(data$charges,data$sex)

ggplot(data = data,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Gender")

# By number of children

ggplot(data = data,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children")
  ggtitle("Boxplot of Medical Charges by Number of Children")

#Smoking status: There are many more non-smokers (80%) than smokers (20%).
#Region of residence: Policyholders are evenly distributed across regions
  #with South East being the most populous one (27%) with the rest of regions 
  #containing around 24% of policyholders each.
#Sex: There are slightly more men (51%) than there are women (49%) in the sample.

#Smoking: There is a big difference in medians between smokers ($34,456) and non-smokers (\$7,345). Non-smokers show many outliers on the larger side, while the vast majority of charges are on the smaller side. Smokers show bimodal distribution and no outliers.
    #Larger charges for smokers are to be expected as smoking is a known serious health risk.
#Region of residence: There are slight differences in medians between all groups. All groups have outliers. The spread of values is fairly similar for all groups apart from South East which has a larger interquartile range (IQR).
#Sex: Males have a marginally larger median ($9,413) than females (\$9,370), a difference of just $43. Both groups show outliers on the larger side. The spread of values is fairly similar.
#Dependents: There are some differences in medians between the groups, but they are not drastic.

####  Hypothesis Testing ###

## Smoking
data %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order

wilcox.test(data$charges ~ data$smoker)

#H0: There is no difference in the distribution scores.
#HA: There is a difference in the distribution scores.
#The test indicated that there is a significant difference between the groups, W = 7403, p < 0.001. The null hypothesis is rejected.

data %>%
  group_by(region) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
kruskal.test(charges ~ region, data = data)

#H0: There is no difference between the medians.
#HA: There is a difference between the medians.
#The test showed that the difference between the median medical charges in different regions is not significant, H(3) = 4.73, p = 0.19. A significant level of 0.19 indicates a 19% risk of concluding that a difference exists when there is no actual difference. The null hypothesis is accepted.


#wilcox_test in package coin for exact, asymptotic and Monte Carlo conditional p-values, including in the presence of ties.
#Wilcoxon test is used to compare two groups and see whether they are significantly different from each other in terms of the variable of interest.
#kruskal.test for testing homogeneity in location parameters in the case of two or more samples; t.test for an alternative under normality assumptions [or large samples]
#The Kruskal-Wallis test is a nonparametric statistical test that assesses whether the mean rank scores of a categorical variable differ between more than two groups

#### Model Building


set.seed(123)

# Determine row to split on: split
split <- round(nrow(data)*0.80)
# Create train
train <- data[1:split, ]
# Create test
test <- data[(split + 1):nrow(data), ]

mod <- lm(charges ~ ., data = train)
summary(mod)

# train RMSE
sqrt(mean((train$charges - predict(mod, train)) ^ 2))

p <- predict(mod, newdata = test)
summary(p)

error = test$charges - p

# Calculate RMSE
RMSE <- sqrt(mean(error^2))
RMSE