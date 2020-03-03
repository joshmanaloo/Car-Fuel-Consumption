setwd("C:/Users/Admin/Desktop/Data Science Projects/Fuel Consumption")
rm(list=ls())

library(ggplot2)

data <- read.csv("measurements.csv")
summary(data)

#Lets check regression model between consume and gas_Type

fit1 <- lm(consume ~ factor(gas_type),data=data)
summary(fit1)
#Fail to reject the null, no significant difference between gas type and consume

fit2 <- lm(as.numeric(consume) ~ gas_type + speed + distance +
             temp_inside + temp_outside + AC + rain + sun,data=data)
summary(fit2)

#It seems that the variable that matters are speed, distance, temp outside, and rain when considering
#all data


anova(fit1,fit2)

#Shows that fit 2 is a much better data

#####################################################################################
#####################################################################################
#####################################################################################

#Dividing data set between e10 and sp98

e10.data <- data[which(data$gas_type =="E10"),]
sp98.data <- data[which(data$gas_type =="SP98"),]

e10.fit <- lm(consume ~ speed+distance+temp_inside+temp_outside+AC+rain+sun,data=e10.data)
summary(e10.fit)

sp98.fit <- lm(consume ~ speed+distance+temp_inside+temp_outside+AC+rain+sun,data=sp98.data)
summary(sp98.fit)

#####################################################################################
#####################################################################################
#####################################################################################


e10 <- data.frame(group ="E10",consumption = e10.data$consume)
sp98 <- data.frame(group ="SP98" ,consumption =sp98.data$consum)

sub.data <- rbind(e10,sp98)
ggplot(sub.data, aes(x=group,y=consumption,fill=group))+
  ggtitle("Boxplot of Overall Gas Consumption by Gas Type")+
  geom_boxplot()

summary(e10.data$consume) 
summary(sp98.data$consume)

#We notice from the two regression models of 2 gas types, that Temperature outside is 
#a significant factor we must consider when comparing the two fuel types.

summary(e10.data$temp_outside)
summary(sp98.data$temp_outside)
t.test(e10.data$temp_outside,sp98.data$temp_outside,var.equal = T)
#We notice that there is a difference in means of temperatue outside.
#This means that both gas types have not been used in similar temperatures and can 
#impact gas consumption according to our regression models above.

#####################################################################################
#####################################################################################
#####################################################################################

summary(e10.data$distance)
summary(sp98.data$distance)
#We see that there is quite an outlier in the distance between gas types. This can 
#influence consume so we standardize the variable by determining the range of
#distance we will observe

#A form of standardizing data
norm.e10.data <- data[which(data$distance <= 18 & data$distance >= 11 & data$gas_type =="E10"),]
norm.sp98.data <- data[which(data$distance <= 18 & data$distance >= 11 & data$gas_type =="SP98"),]

e10 <- data.frame(group ="E10",consumption = as.numeric(norm.e10.data$consume))
sp98 <- data.frame(group ="SP98" ,consumption = as.numeric(norm.sp98.data$consume))

sub.data <- rbind(e10,sp98)
ggplot(sub.data, aes(x=group,y=consumption,fill=group))+
  ggtitle("Boxplot of Gas Consumption by Gas Type when Distance travelled is between 11 and 18")+
  geom_boxplot()

summary(norm.e10.data$consume)
summary(norm.sp98.data$consume)

#We see that E10 on average has a slightly higher consume (4.788L/100km) than SP98 (4.755L/100km)

#####################################################################################
#####################################################################################
#####################################################################################

#We check what happens to the consume data when temp outside is standardized
#Earlier we see that the mean of temperature outside between the fuel consumption were
#not equal and we are worried that this may affect our observations
summary(e10.data$temp_outside)
summary(sp98.data$temp_outside)

#A form of standardizing data
norm.e10.temp.data <- data[which(data$temp_outside <= 14 & data$temp_outside >= 7 & data$gas_type =="E10"
                                 & data$distance <= 18 & data$distance >= 11),]

norm.sp98.temp.data <- data[which(data$temp_outside <= 14 & data$temp_outside >= 7 & data$gas_type =="SP98"
                                  & data$distance <= 18 & data$distance >= 11),]

e10 <- data.frame(group ="E10",consumption = as.numeric(norm.e10.temp.data$consume))
sp98 <- data.frame(group ="SP98" ,consumption = as.numeric(norm.sp98.temp.data$consume))

sub.data <- rbind(e10,sp98)
ggplot(sub.data, aes(x=group,y=consumption,fill=group))+
  ggtitle("Boxplot of Gas Consumption by Gas Type when Temperature Outside is between 7c and 14c")+
  geom_boxplot()

summary(norm.e10.temp.data$consume)
summary(norm.sp98.temp.data$consume)

#We see that SP98 on average has a slightly higher consume (4.935L/100km) than E10 (4.758L/100km)
#When we only use data between 7c to 14c

#####################################################################################
#####################################################################################
#####################################################################################

#In our best model, we noticed that rain also made an impact on consume therefore we
#check consume when rain is present

#A form of standardizing data
norm.e10.temp.rain.data <- data[which(data$temp_outside <= 14 & data$temp_outside >= 7 & data$gas_type =="E10"
                                 & data$distance <= 18 & data$distance >= 11 & data$rain==0),]

norm.sp98.temp.rain.data <- data[which(data$temp_outside <= 14 & data$temp_outside >= 7 & data$gas_type =="SP98"
                                  & data$distance <= 18 & data$distance >= 11 & data$rain==0),]

e10 <- data.frame(group ="E10",consumption = as.numeric(norm.e10.temp.rain.data$consume))
sp98 <- data.frame(group ="SP98" ,consumption = as.numeric(norm.sp98.temp.rain.data$consume))

sub.data <- rbind(e10,sp98)
ggplot(sub.data, aes(x=group,y=consumption,fill=group))+
  ggtitle("Boxplot of Gas Consumption by Gas Type when Temperature Outside is between 7c and 14c
          and Rain is not present")+
  geom_boxplot()

#A form of standardizing data
norm.e10.temp.rain.data <- data[which(data$temp_outside <= 14 & data$temp_outside >= 7 & data$gas_type =="E10"
                                      & data$distance <= 18 & data$distance >= 11 & data$rain==1),]

norm.sp98.temp.rain.data <- data[which(data$temp_outside <= 14 & data$temp_outside >= 7 & data$gas_type =="SP98"
                                       & data$distance <= 18 & data$distance >= 11 & data$rain==1),]

e10 <- data.frame(group ="E10",consumption = as.numeric(norm.e10.temp.rain.data$consume))
sp98 <- data.frame(group ="SP98" ,consumption = as.numeric(norm.sp98.temp.rain.data$consume))

sub.data <- rbind(e10,sp98)
ggplot(sub.data, aes(x=group,y=consumption,fill=group))+
  ggtitle("Boxplot of Gas Consumption by Gas Type when Temperature Outside is between 7c and 14c
          and Rain is present")+
  geom_boxplot()

summary(norm.e10.temp.rain.data$consume)
summary(norm.sp98.temp.rain.data$consume)

#Please note the higher variance in the boxplot models as there are not many observations were recorded
#for when rain was present.

#####################################################################################
#####################################################################################
#####################################################################################

#Some extra analysis 
plot(e10.data$speed,e10.data$consume)
plot(sp98.data$speed,sp98.data$consume)

cor(e10.data$speed,e10.data$consume)
cor(sp98.data$speed,sp98.data$consume)