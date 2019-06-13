
#Is there a linear relationship between  time to intervention and cases, admissions, days as shown in data file.
#would another model be better?

#require packages

require(readxl)
require(dplyr)
require(MASS)

#readdata
data=read_excel("RachelFile.xlsx")

#check data have read in correctly
summary(data)
names(data)

#rename variables for ease

names(data)=c("days","cases","admitted","deaths")

##scatter plots - with regression lines

plot(data$days,data$admitted)
abline(lm(data$admitted ~ data$days), col="red")


plot(data$days,data$cases)
abline(lm(data$cases ~ data$days), col="blue")

plot(data$days,data$deaths)
abline(lm(data$deaths ~ data$days), col="blue")



#problem is that the data are discreet...

#And they're heavily skewed towards 0...See the histograms

hist(data$days)
hist(data$admitted)
hist(data$cases)
hist(data$deaths, breaks=6)


# so normal regression not the right solution
# Also variance is much bigger than the mean

data%>% summarise_all(funs(mean))
data%>% summarise_all(funs(var))

#so negative binomial the best solution

m1.nb=glm.nb(deaths~days,data=data)
summary(m1.nb)

m2.nb=glm.nb(cases~days,data=data[which(data$cases<20 & data$days<20),])
summary(m2.nb)

m3.nb=glm.nb(admitted~days,data=data)
summary(m3.nb)

#Calculate pseudo R squares - to see how much the model is explaining

rsq_m1.nb=1-m1.nb$deviance/m1.nb$null.deviance
rsq_m2.nb=1-m2.nb$deviance/m2.nb$null.deviance
rsq_m3.nb=1-m3.nb$deviance/m3.nb$null.deviance


#highest pseudo r-squared is for m2.nb (cases), but still just 0.03
rsq_m1.nb
rsq_m2.nb
rsq_m3.nb


#try poisson regression -> similar results

m1=glm(deaths~days,family="poisson",data=data)
summary(m1)

m2=glm(cases~days,family="poisson",data=data)
summary(m2)

m3=glm(admitted~days,family="poisson",data=data)
summary(m3)

#Calculate pseudo R squares - to see how much the model is explaining
rsq_m1=1-m1$deviance/m1$null.deviance
rsq_m2=1-m2$deviance/m2$null.deviance
rsq_m3=1-m3$deviance/m3$null.deviance

#highest pseudo r-squared is for m2 (cases), but still just 0.10
rsq_m1
rsq_m2
rsq_m3



#what happens if we just do OLS? - You get a similar pattern (which is good)


m1.lm=lm(deaths~days,data=data)
summary(m1.lm)

m2.lm=lm(cases~days,data=data)
summary(m2.lm)

m3.lm=lm(admitted~days,data=data)
summary(m3.lm)

plot(m2.nb$linear.predictors)

# plots and lines

#create dataset to predict for lines on chart


newdata= rep(seq(from =0, to = 49, length.out = 50), 1)
newdata=data.frame(newdata)
names(newdata)=c("days")


# Deaths model - not significant

plot(data$days,data$deaths, xlab="Days from date intervention requested", ylab="Number of deaths",pch=16, col="black", xlim=c(0,35), ylim=c(0,10), yaxs="i",xaxs="i")
grid(7, 7, lwd = 2, col="lightgrey")

lines(predict(m1.nb,newdata, type="response"), col="forestgreen",lwd=2)
lines(predict(m1.lm,newdata, type="response"), col="firebrick",lwd=2)
lines(predict(m1,newdata, type="response"), col="darkorchid",lwd=2)
legend("topright", inset=.02, legend=c("Negative Binomial", "Ordinary OLS", "Poisson"), pch=15,
       col=c("forestgreen", "firebrick","darkorchid"),  cex=0.8, bg=NULL, bty="n")


#cases model - this in the only model with significant coefficients

plot(data$days,data$cases, xlab="Days from date intervention requested", ylab="Number of cases",pch=16, col="black", xlim=c(0,35), ylim=c(0,35), yaxs="i",xaxs="i")
grid(7, 7, lwd = 2, col="lightgrey")
#abline(v = 1:10,  lty = 2, col = "grey")
#abline(h = 1:10,  lty = 2, col = "grey")

lines(predict(m2.nb,newdata, type="response"), col="forestgreen",lwd=2)
lines(predict(m2.lm,newdata, type="response"), col="firebrick",lwd=2)
lines(predict(m2,newdata, type="response"), col="darkorchid",lwd=2)
legend("topleft", inset=.02, legend=c("Negative Binomial", "Ordinary OLS", "Poisson"), pch=15,
       col=c("forestgreen", "firebrick","darkorchid"),  cex=0.8, bg=NULL, bty="n")


#admissions model - not significant 

plot(data$days,data$admitted, xlab="Days from date intervention requested", ylab="Number of admissions",pch=16, col="black", xlim=c(0,35), ylim=c(0,35),yaxs="i",xaxs="i")
grid(7, 7, lwd = 2, col="lightgrey")
lines(predict(m3.nb,newdata, type="response"), col="forestgreen",lwd=2)
lines(predict(m3.lm,newdata, type="response"), col="firebrick",lwd=2)
lines(predict(m3,newdata, type="response"), col="darkorchid",lwd=2)
legend("topleft", inset=.02, legend=c("Negative Binomial", "Ordinary OLS", "Poisson"), pch=15,
       col=c("forestgreen", "firebrick","darkorchid"),  cex=0.8, bg=NULL, bty="n")




