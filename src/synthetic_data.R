## Sample one binary vector
sample1data <- function(n,a1=0.5) {
  sample(c(1,0),size=n,replace=TRUE,prob=c(1-a1,a1))
}

## Sample a correlated binary vector (r12=0) is equivalent to using
## sample1data, i.e., z1 is effectively ignored.
sample2data <- function(z1,a1=0.5,a2=0.5,r12=0) {
  n <- length(z1)
  i <- which(z1==0)
  n1 <- length(i)
  res <- rep(NA,n)
  res[i]  <- sample(c(1,0),size=n1,replace=TRUE,
                    prob=c(1-a2-r12/a1,a2+r12/a1))
  res[-i] <- sample(c(1,0),size=n-n1,replace=TRUE,
                    prob=c((1-a1)*(1-a2)/a1+r12/a1,(1-a1)*a2/a1-r12/a1))
  res
}

## Allowed range of correlations r12 for accuracies a1 and a2.
## We obtain the range by requiring that all probabilities stay in the
## interval [0,1].
ranger12 <- function(a1=0.5,a2=0.5) {
  (1-sqrt(.Machine$double.eps))*c(
    max(-a1*a2,a1*(1-a2)-1,(1-a1)*a2-1,-(1-a1)*(1-a2)),
    min(c(1-a1*a2,a1*(1-a2),(1-a1)*a2,1-(1-a1)*(1-a2))))
}

#' Toy data where model 1 is the best and there are some correlation
#' structures.
createdata <- function(n=100,a1=0.8,a2=0.7,a3=0.6,
                       r12=0.5*ranger12(a1,a2)[2],
                       r22=0.5*ranger12(a2,a2)[2]) {
  x1 <- sample1data(n=n,a1=a1)
  x2 <- sample1data(n=n,a1=a2)
  x3 <- sample2data(x2,a1=a2,a2=a2,r12=r22)
  x4 <- sample2data(x1,a1=a1,a2=a2,r12=r12)
  x5 <- sample1data(n=n,a1=a3)
  matrix(c(x1,x2,x3,x4,x5),nrow=n,ncol=5,
         dimnames=list(NULL,c("M1","M2","M3","M4","M5")))
}

plotdata <- function(data) {
  ## https://colorbrewer2.org/?type=qualitative&scheme=Set1&n=5
  cols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
  plot(c(1,nrow(data)),c(0,max(colSums(1-data))),type="n",
       xlab=expression(t),ylab=expression(N),main="")
  abline(a=0,b=0.8,col=cols[1],lty="dotted")
  abline(a=0,b=0.7,col=cols[2],lty="dotted")
  abline(a=0,b=0.6,col=cols[5],lty="dotted")
  for(i in 1:5) {
    lines(cumsum(1-data[,i]),col=cols[i])
  }
  legend("topleft",sapply(1:5,function(i) sprintf("model %d",i)),lty="solid",col=cols)
}
