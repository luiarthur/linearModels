library(MASS)
y <- c(17,20,12,6,9,15,4,6,20,11,14,17,19)
n <- length(y)
X <- matrix(c(1,1,0,0,0,1,0,0,
              1,1,0,0,0,1,0,0,
              1,0,1,0,0,1,0,0,
              1,0,0,1,0,1,0,0,
              1,0,0,0,1,1,0,0,
              1,1,0,0,0,0,1,0,
              1,0,0,0,1,0,1,0,
              1,0,0,0,1,0,1,0,
              1,1,0,0,0,0,0,1,
              1,0,1,0,0,0,0,1,
              1,0,1,0,0,0,0,1,
              1,0,0,1,0,0,0,1,
              1,0,0,0,1,0,0,1),13,byrow=TRUE)
XTX <- t(X) %*% X
r <- qr(X)$rank
XXg <- ginv(XTX)

C <- matrix(c(0,0,0,0,0,1,-1,0,
              0,0,0,0,0,0,1,-1), 2,byrow=TRUE)
b <- XXg %*% t(X) %*% y
Cb <- C %*% b
sse <- sum((y - X%*%b)^2)
H <- C %*% XXg %*% t(C)
s <- qr(H)$rank

num <- t(Cb) %*% solve(H) %*% Cb / s
denom <- sse / (n-r)
f <- num / denom 

pf(f,s,n-r,lower.tail=FALSE) # .0198 => regect at alpha = .05. Conclude that the beta's are different
