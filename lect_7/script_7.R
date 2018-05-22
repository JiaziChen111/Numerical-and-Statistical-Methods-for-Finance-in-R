
                                        ## lect_7 ##

                          ## Generating Continuous Random Variables ##

##############################################


getwd()                                       # determine the working directory

setwd("./Documents/Teaching")                  
                                              # change the working directory
                                              # . refers to the current directory
getwd()

##############################################

## The Inverse Transform Method             

# Example 5a Ross (2006)

m<-5
F.inv<- function(u) u^(1/m)

curve(F.inv,0,1)
abline(v=0,h=0,lty=2)
abline(v=1,h=1,lty=2)

U<-runif(1)
X<-F.inv(U)
print(X)

                                              # Simulation study
N<-5000
set.seed(1)
U<-runif(N)
X<-F.inv(U)

# ?ecdf
cdf<-ecdf(X)
plot(cdf)

Fx<- function(x) x^(m)
curve(Fx,0,1,add=T,col="red")

# pdf("inv_cont1.pdf",paper="special")

# plot(cdf,xlim=c(0,1),lwd=1.5,main=expression(paste(F(x)==x^m,", ",N==5000)))
# curve(Fx,0,1,add=T,lwd=1.5,col="red")
# legend(0,0.99,c("empirical","theoretical"),lty=c(1,1),lwd=c(1.5,1.5),
       # col=c(1,"red"),bty="n")

# dev.off()

                                              # Using max of Unif(0,1) rv's

U<-runif(m)
X<-max(U)
print(X)

                                              # Simulation study
N<-5000
set.seed(1)
U<-matrix(runif(m*N),ncol=N)

X<-apply(U,2,max)

plot(ecdf(X))

Fx<-function(x) x^(m)
curve(Fx,0,1,add=T,col="red")


## triangualar distribution

# pdf("triangular1.pdf",width=12,height=6,paper="special")
# old.par <- par(no.readonly = TRUE)            
# par(mar=c(5, 4, 1.5, 1) + 0.1,mfrow=c(1,2))

plot(c(0,0,2,2),c(0,1,0,1),type="n",xlab="x",ylab="density")
lines(c(0,1),c(0,1))
lines(c(1,2),c(1,0))


Fx<-function(x) x^2/2
curve(Fx,0,1,xlim=c(0,2),ylim=c(0,1),ylab="cdf")
Fx<-function(x) 1-(2-x)^2/2
curve(Fx,1,2,add=T)

# par(old.par)
# dev.off()
                                              # as sum of 2 iid Unif(0,1)
m<-2
U<-runif(m)
X<-sum(U)
print(X)

                                              # Simulation study
N<-5000
set.seed(1)
U<-matrix(runif(m*N),ncol=N)

X<-apply(U,2,sum)

plot(ecdf(X))

Fx<-function(x) x^2/2
curve(Fx,0,1,add=T,col="red")
Fx<-function(x) 1-(2-x)^2/2
curve(Fx,1,2,add=T,col="red")

# pdf("triangular2.pdf",paper="special")

hist(X,prob=T,main=paste("Triangular distribution, N=",N))
lines(c(0,1),c(0,1),lwd=2,col="red")
lines(c(1,2),c(1,0),lwd=2,col="red")

legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()

                                              # inverse transform method
F.inv1<-function(u) sqrt(2*u)
F.inv2<-function(u) 2-sqrt(2*(1-u))
U<-runif(1)
if (U<0.5) {
	X<-F.inv1(U)
} else X<-F.inv2(U)
print(X)

                                              # Simulation study
N<-5000
set.seed(1)
U<-runif(N)
X<-rep(0,N)
for (i in 1:N){
	if (U[i]<0.5) X[i]<-F.inv1(U[i]) else X[i]<-F.inv2(U[i])
}

plot(ecdf(X))

Fx<-function(x) x^2/2
curve(Fx,0,1,add=T,col="red")
Fx<-function(x) 1-(2-x)^2/2
curve(Fx,1,2,add=T,col="red")

hist(X,prob=T,main=paste("Triangular distribution, N=",N))
#lines(density(X))
lines(c(0,1),c(0,1),lwd=2,col="red")
lines(c(1,2),c(1,0),lwd=2,col="red")

legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")


## Exponential rv
                                              # F^{-1}(u)
F.inv<- function(u) -log(1-u)

curve(F.inv,0,1)
abline(h=0,v=0,lty=2)
abline(v=1,lty=2)

U<-runif(1)
X<--log(1-U)
print(X)

                                              # Simulation study
N<-5000
set.seed(1)
U<-runif(N)
X<--log(1-U)

# pdf("exponential1.pdf",paper="special")

hist(X,prob=T,main=expression(paste(f(x)==e^-x,", ",N==5000)))
curve(dexp(x),add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()


## Poisson and exponential distribution

lambda<-3
log.U<-log(runif(1))
N<-0
while (log.U>= -lambda) {
	log.U<-log.U+log(runif(1))
	N<-N+1
}
print(N)

                                              # Simulation study
lambda<-3
N<-5000
set.seed(1)
X<-rep(0,N)
for (i in 1:N) {
	log.U<-log(runif(1))
	while (log.U>= -lambda) {
		log.U<-log.U+log(runif(1))
		X[i]<-X[i]+1
	}
}

x.max<-max(X)
freq<-rep(0,x.max+1)
for (i in 0:x.max) freq[i+1]<-sum(X==i)/N

plot(0:x.max,freq,type="h",lwd=3)
lines(0:x.max+0.05,dpois(0:x.max,lambda),type="h",col="red",lwd=3)

# pdf("pois2.pdf",paper="special")

# plot(0:x.max,freq,type="h",lwd=3,xlab="x",ylab="prob",
     # main=expression(paste("Pois(",lambda,"=3), ",N==5000)))
# lines(0:x.max+0.05,dpois(0:x.max,lambda),type="h",col="red",lwd=3)
# legend("topright",c("observed","theoretical"),lty=c(1,1),lwd=c(3,3),
       # col=c(1,"red"),bty="n")

# dev.off()

## Gamma rv

n<-5
lambda=4
U<-runif(n)
Y<-sum(-log(U)/lambda)
print(Y)

                                              # Simulation study
N<-5000
set.seed(1)
U<-matrix(runif(n*N),ncol=N)
Y<-apply(-log(U)/lambda,2,sum)
#print(Y)

# pdf("gamma1.pdf",paper="special")

#string<-expression(paste("gamma(",n,"=5, ",lambda,"=3), ",N==5000))
string<-substitute( paste('gamma( ',n==a,', ',lambda==b,' ), ',N==c),
                    list(a=n,b=lambda,c=N) )
hist(Y,prob=T,main=string)
curve(dgamma(x,shape=n,rate=lambda),add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()

                                              # Exponential through conditioning
                                              # k=2
U<-runif(2)
t<-sum(-log(U))                                              
X<-runif(1)*t
Y<-t-X
cat("X=",X,"\n")
cat("Y=",Y,"\n")

                                              # Simulation study
N<-5000
lambda<-4
set.seed(1)
X<-matrix(0,ncol=2,nrow=N)
for (i in 1:N) {
	U<-runif(2)
	t<-sum(-log(U)/lambda)
	X[i,1]<-runif(1)*t
	X[i,2]<-t-X[i,1]
}

#string<-expression(paste("Exponential(",lambda,"), ",N==5000))
string<-substitute( paste('Exponential( ',lambda==b,' ), ',N==c),
                    list(b=lambda,c=N) )
j<-2
hist(X[,j],prob=T,main=string)
curve(dexp(x,lambda),0,10,add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

                                              # k arbitray
k<-5
U<-runif(k)
t<-sum(-log(U))                                              
U.prime<-runif(k-1)
U.prime<-c(0,sort(U.prime),1)
U.prime<-U.prime[-1]-U.prime[-(k+1)]
X<-U.prime*t
cat("X=",X,"\n")

                                              # Simulation study
N<-5000
lambda<-4
set.seed(1)
X<-matrix(0,ncol=k,nrow=N)
for (i in 1:N) {
	U<-runif(k)
	t<-sum(-log(U)/lambda)
	U.prime<-runif(k-1)
	U.prime<-c(0,sort(U.prime),1)
	U.prime<-U.prime[-1]-U.prime[-(k+1)]
	X[i,]<-U.prime*t
}

#string<-expression(paste("Exponential(",lambda,"), ",N==5000))
string<-substitute( paste('Exponential( ',lambda==b,' ), ',N==c,', ',k==d),
                    list(b=lambda,c=N,d=k) )
j<-1
j<-j+1
hist(X[,j],prob=T,main=string)
curve(dexp(x,lambda),0,10,add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")


## beta rv

lambda<-4
n1<-2
n2<-3
U1<-runif(n1)
U2<-runif(n2)
Y1<-sum(-log(U1)/lambda)
Y2<-sum(-log(U2)/lambda)
X<-Y1/(Y1+Y2)
cat("X=",X,"\n")

                                              # Simulation study
N<-5000
lambda<-4
n1<-3
n2<-3
set.seed(1)
X<-rep(0,N)
for (i in 1:N) {
	U1<-runif(n1)
	U2<-runif(n2)
	Y1<-sum(-log(U1)/lambda)
	Y2<-sum(-log(U2)/lambda)
	X[i]<-Y1/(Y1+Y2)
}

# pdf("beta1.pdf",paper="special")

#string<-paste("beta(",n1,",",n2,"), N=5000")
string<-substitute( paste('beta( ',n[1]==a,', ',n[2]==b,' ), ',N==c),
                    list(a=n1,b=n2,c=N) )
hist(X,prob=T,main=string)
curve(dbeta(x,n1,n2),0,1,add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()

## Exchangeable random vector

lambda<-4
n<-2
k<-3

U<-matrix(runif(k*n),ncol=k)
Y<-apply(-log(U)/lambda,2,sum)
X<-Y/sum(Y)
cat("X=",X,"\n")

# n=1, increments of ordered uniform rv's

U<-runif(k-1)
U<-c(0,sort(U),1)
X<-U[-1]-U[-(k+1)]
cat("X=",X,"\n")

                                              # Simulation study
# N<-5000
# set.seed(1)
# X<-matrix(0,nrow=N,ncol=k)
# for (i in 1:N) {
	# U<-runif(k-1)
	# U<-c(0,sort(U),1)
	# X[i,]<-U[-1]-U[-(k+1)]
# }
n<-2
k<-3
N<-5000
lambda<-4
set.seed(1)
X<-matrix(0,nrow=N,ncol=k)
for (i in 1:N) {
	U<-matrix(runif(k*n),ncol=k)
	Y<-apply(-log(U)/lambda,2,sum)
	X[i,]<-Y/sum(Y)
}
X<-X[,-3]

i<-1
mean(X[,i])
1/k

var(X[,i])
(n^2*(k-1))/(n^2*k^2*(n*k+1))



nbins <- 20+1
x.bin <- seq(0, 1, length=nbins)
y.bin <- seq(0, 1, length=nbins)
dx<-diff(x.bin)[1]
dy<-diff(y.bin)[1]

freq <-  as.data.frame(table(findInterval(X[,1], x.bin),
                             findInterval(X[,2], y.bin)))
freq[,1] <- as.numeric(freq[,1])
freq[,2] <- as.numeric(freq[,2])

freq2D <- diag(nbins)*0
freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]/(N*dx*dy)

library(lattice)
g1<-data.frame(x=x.bin,y=y.bin,z=as.vector(t(freq2D)))


trellis.par.set("axis.line",list(col="transparent"))

# pdf("exch1.pdf",paper="special")

wireframe(z~x*y,g1,screen=list(z=-70,x=-60),shade=T,
         lwd=0.1,light.source=c(0,10,10),
         xlab=expression(x[1]),ylab=expression(x[2]),
         zlab="density",main="n=2, empirical",
         shade.colors = function(irr, ref, height, w = 0.4) grey(w*irr+(1-w)*(1 - (1 - ref)^0.4)) )

# dev.off()


f<-function(x) factorial(5)*x[1]*x[2]*(1-x[1]-x[2])*((x[1]+x[2])<=1)^2

nbins <- 50+1
x.bin <- seq(0, 1, length=nbins)
y.bin <- seq(0, 1, length=nbins)
grid<-expand.grid(x.bin,y.bin)

z.val<-apply(grid,1,f)
g2<-data.frame(x=x.bin,y=y.bin,z=z.val)

# pdf("exch2.pdf",paper="special")

wireframe(z~x*y,g2,screen=list(z=-70,x=-60),shade=T,
         lwd=0.1,light.source=c(0,10,10),
         xlab=expression(x[1]),ylab=expression(x[2]),
         zlab="density",main="n=2, theoretical",
         shade.colors = function(irr, ref, height, w = 0.4) grey(w*irr+(1-w)*(1 - (1 - ref)^0.4)) )

# dev.off()
