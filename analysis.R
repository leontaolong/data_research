# Load libraries
library(ggplot2)
library(dplyr)

setwd("/Users/Leon/desktop")

## read in the data
academic_data <- read.csv(file='./student-mat.csv', sep=";")

## Data Exploration ##

# overview of the dataset
str(academic_data)
data_of_interests <- academic_data[,c("sex", "G1", "G2", "G3")]
View(head(data_of_interests, 20))

# completely_ok obvervation means rows that have no entries missing 
completely_ok <- complete.cases(data_of_interests)
print(paste("Number of completely ok obvervations: ", sum(completely_ok)))

# remove duplicates and missing values
clean_data <- data_of_interests[complete.cases(data_of_interests), ]
clean_data <- unique(clean_data)

# count the number of males and females
M <- filter(clean_data, sex == 'M')
F <- filter(clean_data, sex == 'F')


F <- F[sample(nrow(F), nrow(M)), ]
clean_data <- rbind(M, F)

## Data Preparation ##

# add new aggregated variables
clean_data$avg_G <- (clean_data$G1 + clean_data$G2 + clean_data$G3) / 3

# encode categorical variables into factors
clean_data$gender.code <- factor(clean_data$sex)

str(clean_data)
View(head(clean_data, 20))

# Using base R graphics
lmts <- range(M,F)

par(mfrow = c(1, 2))
boxplot(M,ylim=lmts)
boxplot(F,ylim=lmts)


ggplot(clean_data, aes(sex, G1)) + 
  geom_boxplot() +
  labs(title = "G1 (Math Grades for Period 1) across Gender") 

ggplot(clean_data, aes(sex, G2)) + 
  geom_boxplot() +
  labs(title = "G2 (Math Grades for Period 2) across Gender") 

ggplot(clean_data, aes(sex, G3)) + 
  geom_boxplot()  +
  labs(title = "G3 (Math Grades for Final) across Gender") 

ggplot(clean_data, aes(sex, avg_G)) + 
  geom_boxplot() +
  labs(title = "avg_G (Average Math Grades) across Gender") 

#Plot distribution of G1 cross gender 
ggplot(clean_data ,aes(x=G1)) + 
  geom_histogram(data=subset(clean_data, sex=='F'), 
                 aes(fill=sex), alpha=0.5, inwidth=0.8) +
  geom_histogram(data=subset(clean_data, sex=='M'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  labs(x = "G1", y = "Count", 
       title = "Distribution of G1 across Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) +
  ylim(0, 20) + xlim(0, 20)

#Plot distribution of G2 across gender 
ggplot(clean_data, aes(x=G2)) + 
  geom_histogram(data=subset(clean_data, sex=='F'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  geom_histogram(data=subset(clean_data, sex=='M'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  labs(x = "G2", y = "Count", 
       title = "Distribution of G2 across Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) +
  ylim(0, 20) + xlim(0, 20)

#Plot distribution of G3 across gender 
ggplot(clean_data, aes(x=G3)) + 
  geom_histogram(data=subset(clean_data, sex=='F'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  geom_histogram(data=subset(clean_data, sex=='M'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  labs(x = "G3", y = "Count", 
       title = "Distribution of G3 across Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) +
  ylim(0, 20) + xlim(0, 20)

#Plot distribution of avg_G across gender 
ggplot(clean_data, aes(x=(G1 + G2 + G3) / 3)) + 
  geom_histogram(data=subset(clean_data, sex=='F'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  geom_histogram(data=subset(clean_data, sex=='M'), 
                 aes(fill=sex), alpha=0.5, binwidth=0.8) +
  labs(x = "average grade", y = "Count", 
       title = "Distribution of Average Grade across Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) +
  ylim(0, 15) + xlim(0, 20)


## Statistical Modeling ##
gender.manova <- manova(cbind(G1, G2, G3, avg_G) ~ gender.code, data = clean_data)
summary(gender.manova, tol = 0)
summary.aov(gender.manova)


# run multiple tests
summary(gender.manova, test="H", tol = 0)
summary(gender.manova, test="R", tol = 0)
summary(gender.manova, test="P", tol = 0)
summary(gender.manova, test="W", tol = 0)



