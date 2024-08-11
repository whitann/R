#Load packages
library(tidyverse)
library(visdat)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(BSDA)
library(psych)
library(tidycomm)

#confirm working directory location
getwd()

#load file
df <- read_csv('medical_clean.csv')

#view data frame
spec(df)
head(df,5)

#verify no duplication
str(df)
duplicated(df)
sum(duplicated(df))

#verify no NA
colSums(is.na(df))

#Filter for readmission status
Readmit <- df %>% filter(ReAdmis == "Yes")
Noreadmit <- df %>% filter(ReAdmis == "No")


#Save ages of readmitted patients
x <- Readmit$Age

#Save Ages of patients not readmitted
y <- Noreadmit$Age

#count how many rows are in each group
nrow(Readmit)
nrow(Noreadmit)

#run paired ttest
t.test(x, y, var.equal=FALSE)

#Calculated t value
x1 <- mean(Readmit$Age)
x2 <- mean(Noreadmit$Age)
n1 <- nrow(Readmit)
n2 <- nrow(Noreadmit)
s1 <- sd(Readmit$Age)
s2 <- sd(Noreadmit$Age)

xv<- (x1-x2)
a <- s1^2/n1
b <- s2^2/n2

t<- xv/sqrt(a+b)
print(t)

#Density plot of Age based on Readmission status
ggplot(df, aes(x=Age, fill=ReAdmis))+ 
  geom_density(alpha=0.05)+
  labs(title = "Density plot of Age based on Readmission")+
  xlab("Age")

#QQ plot of Ages of readmitted patients
qqnorm(Readmit$Age, pch=1, frame=FALSE)
qqline(Readmit$Age, col="blue", lwd=2)

#QQ plot of Ages of patients not readmitted
qqnorm(Noreadmit$Age, pch=1, frame=FALSE)
qqline(Noreadmit$Age, col="blue", lwd=2)

z.test(Readmit$Age, Noreadmit$Age,
         alternative = "two.sided",
         mu = 0,
         sigma.x = sd(Readmit$Age),
         sigma.y = sd(Noreadmit$Age),
         conf.level = 0.95
)
         

#Histogram of Age
hist(df$Age,
     main="Patient Ages",
     xlab="Age",
     border="black",
     col="#203824",
     breaks= 5)

#Stats of Age
psych::describe(df$Age)
     
#Histogram of Initial Days
hist(df$Initial_days,
     main="Days Admitted",
     xlab="Days",
     ylab = "Frequency",
     border="black",
     col="#203824",
     breaks=20)

#Stats of Initial Days
psych::describe(df$Initial_days)

#Histogram of Item1
hist(df$Item1,
     main="Patient Ranking of Timely Admission",
     xlab="Ranking",
     border="black",
     col="#203824",
     ylab = "Frequencies",
     breaks=9)

print(table(df$Item1))
cat("The mode is:", names(sort(-table(df$Item1)))[1])

#Histogram of Item2
hist(df$Item2,
     main="Patient Ranking of Timely Treatment",
     xlab="Ranking",
     border="black",
     col="#203824",
     ylab = "Frequencies",
     breaks=8)

print(table(df$Item2))
cat("The mode is:", names(sort(-table(df$Item2)))[1])

#theme for plots
my.theme <- theme_classic()  + theme(aspect.ratio = 1)

#Bivariate density plot of Age/Initial_days 
ggplot(df, mapping = aes(x=Initial_days, y=Age)) + 
   geom_density2d() + 
  labs(x = "Days", y = "Age", title = "Relationship between Days Admitted and Patient Age")  +
   my.theme

#Bivariate Statistics
df %>% 
  regress(Age, Initial_days)


#labels for scatterplot
medical2.labels <- labs(x = "Timely Admission", y = "Timely Treatment",
                    title = "Patient Rankings of Aspects of Care")

#Scatter plot of Item1/Item2
ggplot(df)  + 
  geom_point(aes(x = Item1, y = Item2)) +
  medical2.labels

#Bivariate Statistics
df %>% 
  regress(Item1, Item2)
