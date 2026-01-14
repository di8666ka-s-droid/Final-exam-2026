library(glmmTMB)
dat=read.table("penstemon-1.txt",header=T)
names(dat)
head(dat)
m1 <- glmmTMB(fitness~Pop*flwsize+FlwDate+(1|Block),
              data=dat,family=gaussian())
summary(m1)

dat$Pop <- factor(dat$Pop)
dat$Block <- factor(dat$Block)
#To calculate mean of covariates
mean_flwsize <- round(mean(dat$flwsize),2)
mean_flwdate <- round(mean(dat$FlwDate),0)
#Raw data
stripchart(fitness~Pop,data=dat,
           vertical=TRUE,method="jitter",
           pch=16,col="grey70",
           xlab="Population",
           ylab="Fitness",
           main="Fitness by Population",
           las=1)

         
#Population level predictions
MOD_Pop <- data.frame(
  Pop=levels(dat$Pop),
  flwsize=mean(dat$flwsize),
  FlwDate=mean(dat$FlwDate),
  Block=NA
)
MOD_Pop$pred <- predict(m1,newdata=MOD_Pop,
                           type="response",
                           re.form=NA)
#Predicted points
points(1:3,MOD_Pop$pred,pch=19,col="red")



newdat <- data.frame(
  flwsize=seq(min(dat$flwsize),max(dat$flwsize),length=20),
  Pop=levels(dat$Pop)[1],
  FlwDate=mean(dat$FlwDate),
  Block=NA
)
newdat$pred <- predict(m1,newdata=newdat,type="response",re.form=NA)
x_scaled <- 0.5 + (newdat$flwsize - min(dat$flwsize)) * 3 / (max(dat$flwsize) - min(dat$flwsize))
lines(x_scaled, newdat$pred, col="blue", lty=2, lwd=2)




legend("topright",
       legend=c("Raw data","Model predictions","Flower size effect(mm)"),
       pch=c(16,19,NA),
       lty=c(NA,NA,2),
       col=c("grey70","red","blue")
)
