
#7.10 A survey is taken of 250 students, and a   of 0.45 is found. The same survey is
#repeated with 1,000 students, and the same   value is found. Compare the two 
#95% confidence intervals. What is the relationship? Is the margin of error for 
#the second one four times smaller? How much smaller is it?

n = 250
phat = .45
x = phat*n
test.1 = prop.test(x, n, conf.level = .95)
test.1

n.2 = 1000
x.2 = phat*n.2
test.2 = prop.test(x.2, n.2, conf.level = .95)
test.2


#7.18 The data set normtemp (UsingR) contains measurements of 130 healthy, 
#randomly selected individuals. The variable temperature contains normal body 
#temperature. Does the data appear to come from a normal distribution? Is so, 
#find a 90% confidence interval for the mean normal body temperature. 
#Does it include 98.6 °F?

data("normtemp")
attach(normtemp)
qqnorm(temperature, main = "temeprature"); qqline(temperature)
shapiro.test(temperature)

t.test(temperature, conf.level = 0.90, mu=98.6)
detach(normtemp)

#7.25 Two different AIDS-treatment “cocktails” are compared. For each, the time 
#it takes (in years) to fail is measured for seven randomly assigned patients. 
#The data is in Table 7.1. Find an 80% confidence interval for the difference of 
#means. What assumptions are you making on the data?

cocktail.1 = c(3.1, 3.3, 1.7, 1.2, 0.7, 2.3, 2.9)
cocktail.2 = c(1.8, 2.3, 2.2, 3.5, 1.7, 1.6, 1.4)

boxplot(cocktail.1, cocktail.2, names = c("Cocktail 1", "Cocktail 2"))
par(mfrow = c(1, 2))
qqnorm(cocktail.1, main = "Cocktail 1"); qqline(cocktail.1)
qqnorm(cocktail.2, main="Cocktail 2"); qqline(cocktail.2)
par(mfrow=c(1,1))
shapiro.test(cocktail.1)
shapiro.test(cocktail.2)

t.test(cocktail.1, cocktail.2, paired = F, conf.level = 0.80)


#7.28 or the babies (UsingR) data set, the variable age contains the mother’s 
#age and the variable dage contains the father’s age for several babies. 
#Find a 95% confidence interval for the difference in mean age. Does it 
#contain a? What do you assume about the data?

data("babies")
attach(babies)
age[age>98]<-NA
dage[dage>98]<-NA

boxplot(age,dage, names = c("Mother's Age", "Father's Age"), na.rm=T)
par(mfrow = c(1, 2))
qqnorm(age, main ="Mother's Age"); qqline(age)
qqnorm(dage, main ="Father's Age"); qqline(dage)
par(mfrow=c(1,1))
shapiro.test(age)
shapiro.test(dage)
sd(babies$age)
sd(babies$dage)

t.test(age, dage, var.equal=T, conf.level = .95)
t.test(age, dage, paired=T, conf.level = .95)
detach(babies)

#8.4 In the United States in 1998, the proportion of adults age 21–24 who 
#had no medical insurance was 34.4 percent, according to the Employee Benefit 
#Research Institute. A survey of 75 recent college graduates in this age range 
#finds that 40 are without insurance. Does this support a difference from the 
#nationwide proportion? Perform a test of significance and report the p-value. 
#Is it significant?

n = 75
x=40
binom.test(x, n, p = 0.344, alternative = "two.sided")


#8.12 In the babies (UsingR) data set, the variable dht contains the 
#father’s height. Do a significance test of the null hypothesis that the 
#mean height is 68 inches against an alternative that it is taller. 
#Remove the values of 99 from the data, as these indicate missing data.

data(babies)
attach(babies)
dht[dht>98]<-NA
t.test(dht, alternative =
         "greater", mu = 68)
detach(babies)

#8.22 In the year 2001, a poll of 600 people found that 250 supported gay 
#marriage. A 2003 poll of 500 found 250 in support. Do a test of significance 
#to see whether the difference in proportions is statistically significant.

x.2001=250
n.2001=600

x.2003=250
n.2003=500


prop.test(c(x.2001, x.2003), c(n.2001, n.2003),
          alternative = "two.sided")


#8.33 Water-quality researchers wish to measure biomass/chlorophyll ratio 
#for phytoplankton (in milligrams per liter of water). There are two possible 
#tests, one less expensive than the other. To see whether the two tests give 
#the same results, ten water samples were taken and each was measured both ways, 
#providing the data in Table 8.10. Do a t-test to see if there is a difference 
#in the means of the measured amounts. If you assume equal variances or a 
#paired test, explain why.

method.1 =c(45.9, 57.6, 54.9, 38.7, 35.7, 39.2, 45.9, 43.2, 45.4, 54.8)
method.2=c(48.2, 64.2, 56.8, 47.2, 43.7, 45.7,53.0, 52.0, 45.1, 57.5)
t.test(method.1, method.2, paired = T)
wilcox.test(method.1, method.2, paired = T, exact=F)
