library(MatchIt)
library(TDist)

options("scipen" =100, "digits" = 4)
mydata <- read.csv ("C:/Users/IBM_ADMIN/Documents/Learning/Propensity/Propensity Model/Data/AllPatientsSum.csv")
attach(mydata)
mydata[1:10,]

#-----------------------------------------------------
#  raw differences in means for treatments and controls using regression
#-----------------------------------------------------


summary(lm(AveragePayment~Outreached,data=mydata))
t.test(mydata$AveragePayment[mydata$Outreached==1],mydata$AveragePayment[mydata$Outreached==0])


var.test(mydata$AveragePayment[mydata$Outreached==1],mydata$AveragePayment[mydata$Outreached==0])



#-----------------------------------------------------
# nearest neighbor matching via MatchIt
#-----------------------------------------------------

m.out1 <- matchit(Outreached ~ YearsOld + Gender + Race,
data = mydata, method = "nearest",distance = "logit",
ratio = 1)
summary(m.out1)

m.data1 <- match.data(m.out1,distance ="pscore")
#show(m.data1)


hist(m.data1$pscore) # distribution of propenisty scores
summary(m.data1$pscore)

#-----------------------------------------------------
# perform paired t-tests on matched data
#-----------------------------------------------------
 
t.test(m.data1$AveragePayment[m.data1$Outreached==1],m.data1$AveragePayment[m.data1$Outreached==0],paired=TRUE)

#-----------------------------------------------------------------------------------------
# lets look at regression on the ps restricted data on the entire sample per MHE chapter 3
#-----------------------------------------------------------------------------------------
 
m.data2 <- mydata # copy lalonde for ps estimation
dim(m.data2)
 
# generate propensity scores for all of the data in Lalonde 
ps.model <- glm(Outreached ~  YearsOld + Gender + Race, data = m.data2, family=binomial(link="logit"),
 na.action=na.pass)
 
summary(ps.model)
 
# add pscores to study data
m.data2$pscore <- predict(ps.model, newdata = m.data2, type = "response")
 
hist(m.data2$pscore) # distribution of ps
summary(m.data2$pscore)
dim(m.data2)
 
# restrict data to ps range .010 <= ps <= .90
 
m.data3 <- m.data2[m.data2$pscore >= .010 & m.data2$pscore <=.90,]
summary(m.data3$pscore)
 
# regression with controls on propensity score screened data set
summary(lm(Outreached ~  YearsOld + Gender + Race, data = m.data3))
 
# unrestricted regression with controls
summary(lm(Outreached ~  YearsOld + Gender + Race, data = mydata))







m.out = matchit(Outreached ~ YearsOld + Gender + Race,
data = mydata, method = "nearest",
ratio = 1)
summary(m.out)
plot(m.out, type = "jitter")
plot(m.out, type = "hist")





#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------

x <- seq(-200, 200, length=100)
y <- dt(mydata$AveragePayment)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, y, type="l", lty=2, xlab="x value",
  ylab="Density", main="Comparison of t Distributions")
