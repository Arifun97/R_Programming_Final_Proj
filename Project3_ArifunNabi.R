#Group member - Arifun Nabi, Abdisha Musa

library(tidyverse)
library(tidyxl)
library(readxl)
library(ggplot2)
library(dplyr)
library(lmtest)
library(AICcmodavg)
library(ggcorrplot)
library(broom)

mat <- read.csv("student-mat.csv",sep=";",header=TRUE)
por <- read.csv("student-por.csv",sep=";",header=TRUE)

merged <- merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(merged)) # 382 students
write.csv(merged, "uci_student.csv")



##Section 1
#1 
#both the data looks slightly skewed with median data slightly lower. 
#There is also outliers in both the data.

boxplot(por$G1)
boxplot(por$G2)

hist(por$G1)
hist(por$G2)


#the both the data is not normally distributed since the p value is significantly less than the alpha. 
shapiro.test(por$G1)
shapiro.test(por$G2)


summary(por$G1)

#Added + 2 to get all the positive value 
#por$G1_new <- por$G1 +2

#summary(por$G1_new)
#Square root the new collumn
#por$G1_root <- sqrt(por$G1_new)

#Log of G1
#por$G1_log <- log(por$G1_root)

#hist(por$G1_log)

#Student t test
t.test(por$G1, por$G2,
       alternative = "two.sided",
       paired = TRUE,
       var.equal = FALSE)
##Report: t(648) = -2.9454, p < 0.05

#2 

#Looking at the boxplot both the data looks normally distributed
boxplot(mat$G1)
boxplot(mat$G2)

#Looking the at the Histogram both the data looks slightly skewed to the right.
hist(mat$G1)
hist(mat$G2)

#The shapiro test shows that both the data are significantly less than the alpha level
#which means they are not normally distributed
shapiro.test(mat$G1)
shapiro.test(mat$G2)

#Student t test
t.test(mat$G1, mat$G2,
       alternative = "two.sided",
       paired = TRUE,
       var.equal = FALSE)
#Report: t(394) = 1.9648, p<0.01

#3
#The shapiro test shows that the p value is significantly less than 0.05 which
#means its not normally distributed
shapiro.test(mat$G3)
shapiro.test(por$G3)

#Both the data looks slightly skewed to the right in the histogram
hist(mat$G3)
hist(por$G3)

summary (por$G3)


#por$G3_new <- por$G3 + 1 
#por$G3_sqrt <- por$G3_new
#por$G3_log <- log(por$G3_sqrt)

#shapiro.test(por$G3_log)
var.test(mat$G3, por$G3)

t.test(mat$G3, por$G3,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = FALSE)

#4
shapiro.test(merged$G3.x)
shapiro.test(merged$G3.y)

hist(merged$G3.x)
hist(merged$G3.y)

t.test(merged$G3.x, merged$G3.y,
       alternative = "two.sided",
       paired = TRUE,
       var.equal = FALSE)


##Section 2

#1
var.test(mat$Walc, mat$Dalc )
bartlett.test(mat$G3 ~ mat$Walc)
bartlett.test(mat$G3 ~ mat$Dalc)
#The var test and bartlett test shows that it does not have equal variance. 


#Conver Walc and Dalc 
mat$Walc <- as.factor(mat$Walc)
class(mat$Walc)

mat$Dalc <- as.factor(mat$Dalc)
class(mat$Dalc)

hist(mat$G3)
boxplot(mat$G3)



##Additive
mat_add <- aov(mat$G3 ~ mat$Walc + mat$Dalc)
summary(mat_add)
##with interaction 
mat_interact <- aov(mat$G3 ~ mat$Walc * mat$Dalc)
summary(mat_interact)


#Compare the models
model_set <- list(mat_add, mat_interact)
model_names <- c("mat_add", "mat_interact")
aictab(model_set,model_names)



#Best fit model is the additive model
#Summary of the model does not show statistical significance for weekday and
# weekend alcohol consumption

# Pr>F 0.0.568, 0.262 respectively

#There is significant difference in effect on G3 grade by weekend (f(4) = 0.736, p>0.05)
#and workday (f(4) = 1.319, p>0.05) alcohol consumption, though the interaction between these terms
#was not significant.

res2 <- resid(mat_add)
qqnorm(res2)
qqline(res2)

#The qq plot shows that the residuals are normally distributed


#2
hist (por$G3)
boxplot (por$G3)
var.test(por$Walc, por$Dalc )
bartlett.test(por$G3 ~ por$Walc)
bartlett.test(por$G3 ~ por$Dalc)
#The var test and bartlett test shows that it does not have equal variance. 

#Convert Walc and Dalc 
por$Walc <- as.factor(por$Walc)
class(por$Walc)

por$Dalc <- as.factor(por$Dalc)
class(por$Dalc)


##Additive
por_add <- aov(por$G3 ~ por$Walc + por$Dalc)
summary(por_add)

##Interactive
por_interact <- aov(por$G3 ~ por$Walc * por$Dalc)
summary(por_interact)

#Compare the models
model_set <- list(por_add, por_interact)
model_names <- c("por_add", "por_interact")
aictab(model_set,model_names)

#Best fit model is the additive model
# Summary of the mode show there is significant difference for weekend and weekday alcohol consumption
#Pr>F 0.000188, 0.006811  respectively

## TukeyHSD
TukeyHSD(por_add)

## There is significant difference in effect on G3 grade by weekend (f(4) = 5.624, p<0.001)
#and workday (f(4) = 3.573, p<0.001) alcohol consumption, though the interaction between these terms
#was not significant.

## 
### 
### 
###A TukeyHSD post-hoc test revealed significant pairwise differences between 
###weekend 4 and 1 (-1.32 under 3), weekend 5 and 1 (-1.80 under 1),
###  weekend 4 and 2 (-1.23 under 2), weekend 5 and 2 (-1.70 under 5) alcohol consumption,
### and workday alcohol consumption of 4 and 1 (-2.34 under 4). 

res3 <- resid(por_add)
qqnorm(res3)
qqline(res3)

#The qq plot shows that the residuals are normally distributed

###Section 3
##1
#Variables are quantitative
#Create histogram  to determine normality
hist(mat$age, main = "Age Normality Tets", xlab = "Age of Students", col = "red")
hist(mat$G3, main = "Histogram Normality Test", xlab = "G3 Mathematics Grade", col = "red")
plot (mat$age, mat$G3)
#It does not look there is any big outliers
#The scatter plot shows linear relationship
boxplot(mat$G3 ~ mat$age)
#The box plot shows equal variance
qqplot(mat$age, mat$G3)
#The qq plot shows that the samples are normally distributed
#The two pairs are also related pair as its in the same data set and two variables
#are from same individual student.

result = cor.test(mat$G3, mat$age, method = 'pearson')
print(result)
#We would reject the null since p<0.05
#this shows there is statistically significant negetive correlation between age and G3 grade
#cor -0.1615794 
#The two variables are strongly negative correlated, and the relationship was 
#significant, r(393) = -0.1615794,  p = 0.001271

#Correlation plot
Math <- data.frame(mat$age, mat$G3)
ggcorrplot(cor(Math))

##2
#Variables are quantitative
plot (por$age, por$G3)
hist(por$age, main = "Age Normality Tets", xlab = "Age of Students", col = "red")
hist(por$G3, main = "Histogram Normality Test", xlab = "G3 Portuguese Grade", col = "red")
#It does not look there is any big outliers
#The scatter plot shows linear relationship
boxplot(por$G3 ~ por$age)
#The box plot shows equal variance
qqplot(por$age, por$G3)
#The qq plot shows that the samples are somewhat normally distributed
#The two pairs are also related pair as its in the same data set and two variables
#are from same individual student.

result1 = cor.test(por$age, por$G3, method = 'pearson')
print(result1)
#We would reject the null since p<0.05
#this shows there is statistically significant negetive correlation between age and G3 grade
#cor -0.1065054 
#The two variables are strongly negative correlated, and the relationship was 
#significant, r(647) = -0.1065054,  p = 0.006612

#Correlation plot
Por_frame <- data.frame(por$age, por$G3)
ggcorrplot(cor(Por_frame))


##3
#Variables are quantitative
plot (merged$G3.x, merged$G3.y)
#The scatter plot shows somewhat linear relationship
#It does not look there is any big outliers
boxplot(merged$G3.x ~ merged$G3.y)
#The box plot shows equal variance
hist(merged$G3.x, main = "Histogram Normality Test", xlab = "G3 Mathematics Grade", col = "red")
hist(merged$G3.y, main = "Histogram Normality Test", xlab = "G3 Portuguese Grade", col = "red")
#Both the variables looks somewhat normally distributed in histogram
#The two pairs are also related pair as its in the same data set and two variables
#are from same individual student.
result2 <- cor.test(merged$G3.x, merged$G3.y, method = 'pearson' )
print(result2)
#We would reject the null since p<0.05
#this shows there is somewhat positive correlation between mathematics grade and
#Portuguese grades.
#cor 0.4803494 
#The two variables are somewhat positively correlated, and the relationship was 
#significant, r(380) = 0.4803494,  p < 2.2e-16

#Correlation plot
MergedFrame <- data.frame(merged$G3.x, merged$G3.y)
ggcorrplot(cor(MergedFrame))



##Section 4
#Predictor variable= age
#Response variable = Grade
plot (mat$age, mat$G3)
#The scatter plot shows linear relationship
#it does not looks there is any big outliers. 
#We are assuming that the variables are independence 
boxplot(mat$G3 ~ mat$age)
#The box plot shows equal variance
qqplot(mat$age, mat$G3)
#The qq plot shows that the samples are normally distributed
regression_model <- lm(mat$G3 ~ mat$age)
summary(regression_model)
#G3 = 20.1011 + (-0.5801)*(age)

ggplot(data = mat, aes(G3, age))+
  geom_point() +
  stat_smooth(method = lm) +
  ggtitle("Age VS Mathematics G3 Grade Simple Linear Regression") +
  xlab("G3 Grade for Math") + ylab("Age")

#Homoscedasticity check
res <- resid(regression_model)
plot (fitted(regression_model), res)
abline(0,0)

#The plot looks like it is random thus it has homoscedasticity

qqnorm(res)
qqline(res)

#QQ plot shows the residual are normally distributed



##Section 5 
boxplot (mat$G3)
bartlett.test(mat$G3 ~ mat$internet)
#Bartlett test shows that there is equal variance. 

#null hypothesis:Internet access has the same effect on  mathematics students G3 grade
#alternative hypothesis: At least one type of internet access has different effect on G3 grade
#effect on mathematics student's G3 grade

oneway <- aov(mat$G3 ~ mat$internet)

summary.aov(oneway)


#Pr(>F) > 0.05 We fail to reject the null
#We conclude there is not significance difference between student who has internet
#access and who doesn't. The effect on students Mathematics G3 grade is the same 
#for both student who does and who doesn't.

#F(1, 393) = 3.849, p = 0.0505

res1 <- resid(oneway)
plot (fitted(oneway), res1)
abline(0,0)


qqnorm(res1)
qqline(res1)

#The qq plot shows that the residual are normally distributed
