dat <- read.table("mpg_data.txt",header=TRUE)

mod <- lm(MPG ~ HP, data=dat)
log_mod <- lm(log(MPG) ~ HP, data=dat)

op <- par(no.readonly=TRUE)
par(oma=c(5,0,2,0), mar=c(0,4,0,1))
par(mfrow=c(2,1))
  plot(dat$HP,dat$MPG,ylab="MPG",xaxt="n",pch=20,col="grey",cex=2,fg="grey")
  abline(mod,col="blue",lwd=3)

  plot(dat$HP, log(dat$MPG),ylab="log(MPG)",pch=20,col="grey",cex=2,fg="grey")
  abline(log_mod,col="blue",lwd=3)
par(mfrow=c(1,1))
title(xlab="HP",outer=TRUE,main="MPG v. HP")
par(op)

summary(mod)
summary(log_mod)

mod_full <- lm(MPG~HP+SP+VOL+WT, data=dat)
summary(mod_full)
