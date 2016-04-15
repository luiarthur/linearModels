x <- c(39, 30, 32, 34, 35, 36, 36, 30)
y <- c(71, 63, 63, 67, 68, 68, 70, 64)

mod <- lm(y~x)
y.hat <- predict(mod)

sum( (y.hat - mean(y))^2 )  / sum( (y - mean(y))^2 )
(cov(x,y) / sd(x) / sd(y))^2
