## ---- echo=F, display=F, results="hide", warning=F, message=F----
#setwd("C:/Users/trussler-adm/PORES Dropbox/PORES/PSCI1801/Lectures/IIS")
knitr::purl("Introduction.Rmd")


## ---------------------------------------------------------
library(rio)
anes <- import(file="https://github.com/marctrussler/IIS/raw/main/Data/ANES2020Clean.csv")
#Examine the first 6 rows of data for these variables
head(anes[c("therm.trump","therm.biden")])


## ---------------------------------------------------------
#Plot the density of the Trump feeling thermometer
plot(density(anes$therm.trump,na.rm=T), xlim=c(0,100), main="Distribution of Trump Themometer Ratings")
abline(v=mean(anes$therm.trump,na.rm=T), lty=2)
legend("topright", c(paste("Mean = ",round(mean(anes$therm.trump,na.rm=T),2 )),
                     paste("SD = ",round(sd(anes$therm.trump,na.rm=T),2 )) ), lty=2, col=c("Black", "White"))



## ---------------------------------------------------------
#Generate 1000 at-bats for a .350 hitter
set.seed(19104)
hits <- rbinom(10000,1, .35)


## ---------------------------------------------------------
#Use a loop to calculate the moving 20-at-bat batting average for this batter
moving.average <- NA

for(i in 1:9981){
 moving.average[i] <-  mean(hits[i:(i+19)])
}

plot(1:500, moving.average[1:500], type="l", axes=F,
     xlab="", ylab="Batting Average for Previous 20 Games", cex=.5)
abline(h=.35, lty=2)
axis(side=2, las=2)
axis(side=1)


## ---------------------------------------------------------
#Plot Feeling Thermometer against age
plot(anes$age, anes$therm.trump)
abline(lm(anes$therm.trump ~ anes$age), col="firebrick", lwd=2)


## ---------------------------------------------------------
#Run a regression predicing feelings towards Trump by age
m <- lm(therm.trump ~ age, data=anes)
summary(m)


## ---------------------------------------------------------
#Creation of fake data:
set.seed(19104)
x.prime <- rnorm(1000,mean=3, sd=6)
y.prime <- 3*rnorm(1000) + 4*x.prime + rnorm(1000, 0, 30)
plot(x.prime,y.prime)
abline(lm(y.prime ~ x.prime), col="firebrick", lwd=2)
summary(lm(y.prime~x.prime))


## ---------------------------------------------------------
x <- rnorm(1000,mean=3, sd=6)
y <- 3*rnorm(1000) + 4*x + rnorm(1000, 0, 30)
plot(x.prime,y.prime, col="gray80")
abline(lm(y.prime ~ x.prime), col="gray30", lwd=2)
points(x,y, col="darkorange")
abline(lm(y~x), col="darkorange", lwd=2)
summary(lm(y~x))


## ---- cache=T---------------------------------------------
coefs <- rep(NA, 5000)

plot(x.prime,y.prime, type="n")
abline(lm(y.prime ~ x.prime), col="gray30", lwd=2)
for(i in 1:5000){
x <- rnorm(1000,mean=3, sd=6)
y <- 3*rnorm(1000) + 4*x + rnorm(1000, 0, 30)
m <- lm(y ~ x)
coefs[i] <- coefficients(m)["x"]
abline(m, col="gray60")
}
abline(lm(y.prime ~ x.prime), col="firebrick", lwd=2)


## ---------------------------------------------------------
plot(density(coefs), main="Sampling Distribution of Beta Coefficients")
abline(v=4, col="firebrick", lwd=2)


## ---------------------------------------------------------
summary(lm(y.prime ~ x.prime))


## ---------------------------------------------------------
plot(density(coefs), main="Sampling Distribution of Beta Coefficients")
x <- seq(3,5,.001)
#Note the use of  0.1557, which is from the regression table above!
points(x,dnorm(x, mean=4, sd= 0.1557), type="l", col="darkorange")

abline(v=4, col="firebrick", lwd=2)


