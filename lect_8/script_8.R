
                                        ## lect_8 ##

                          ## Generating Continuous Random Variables ##

##############################################

## Rejection method

# Beta with uniform envelop

# Example 5d, page 72, Ross (2006)

prob.accept<-0
iter<-0
U<-1
while (U>=prob.accept){
	Y<-runif(1)
	prob.accept<-(256/27)*Y*(1-Y)^3
	U<-runif(1)
	#cat("prob accept=",prob.accept,"\n")
	#cat("U=",U,"\n")
	iter<-iter+1
}
X<-Y
cat("X=",X,"\n")
cat("num iter=",iter)

                                              # Simulation study
set.seed(50)
N<-1000
num.iter<-rep(0,N)
X<-rep(0,N)

for (i in 1:N) {
	prob.accept<-0
	U<-1
	while (U>=prob.accept){
		Y<-runif(1)
		prob.accept<-(256/27)*Y*(1-Y)^3
		U<-runif(1)
		num.iter[i]<-num.iter[i]+1
	}
	X[i]<-Y
}

#print(X)
mean(num.iter)

# pdf("accept2.pdf",paper="special")

n1<-2;n2<-4
string<-substitute( paste('Rejection, beta( ',n[1]==a,', ',n[2]==b,' ), ',N==c),
                    list(a=n1,b=n2,c=N) )
hist(X,prob=T,main=string)
curve(dbeta(x,n1,n2),0,1,add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()


# Gamma with exponential envelop

# Example 5e, page 73, Ross (2006)

f<-function(x) (2/sqrt(pi))*x^(1/2)*exp(-x)

curve(f,0,10)
g<-function(x) (2/3)*exp(-2*x/3)
curve(g,add=T,col="red")

cg<-function(x) 1.2573*(2/3)*exp(-2*x/3)
curve(cg,add=T,col="blue",lwd=2)

ratio<-function(x) f(x)/g(x)

options(warn = -1)
out<-optim(1,ratio,control = list(fnscale = -1))
x.star<-out$par
x.star
3/2
c.const<-ratio(x.star)
c.const
out$value
3^(3/2)/(2*pi*exp(1))^(1/2)

lambda<-2/3

prob.accept<-0
iter<-0
U<-1
while (U>=prob.accept){
	Y<--log(runif(1))/lambda
	prob.accept<-f(Y)/(c.const*g(Y))
	U<-runif(1)
	iter<-iter+1
}
X<-Y
cat("X=",X,"\n")
cat("num iter=",iter)

                                              # Simulation study
set.seed(50)
N<-5000
num.iter<-rep(0,N)
X<-rep(0,N)
for (i in 1:N) {
	prob.accept<-0
	U<-1
	while (U>=prob.accept){
		Y<--log(runif(1))/lambda
		prob.accept<-f(Y)/(c.const*g(Y))
		U<-runif(1)
		num.iter[i]<-num.iter[i]+1
	}
	X[i]<-Y
}
mean(num.iter)
c.const

# pdf("accept3.pdf",paper="special")

string<-substitute( paste('Rejection, gamma( ',alpha==a,', ',lambda==2/3,' ), ',N==c),
                    list(a=1,b=lambda,c=N) )
hist(X,prob=T,main=string)
curve(dgamma(x,1,lambda),add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()


# Normal with exponential envelop

# Example 5f, page 75, Ross (2006)

Y1<--log(runif(1))
Y2<--log(runif(1))
iter<-1
while (Y2<=(Y1-1)^2/2) {
	Y1<--log(runif(1))
	Y2<--log(runif(1))
	iter<-iter+1
}
U<-runif(1)
if (U<=1/2) Z<--Y1 else Z<-Y1
Y1<-Y2-(Y1-1)^2/2
cat("Z=",Z,"\n")
cat("Y=",Y,"\n")
cat("num iter=",iter)

                                              # Simulation study
mu<-0;sigma<-1
set.seed(50)
N<-5000
num.iter<-rep(1,N)
Z<-rep(0,N)
Y<-rep(0,N)
Y1<--log(runif(1))

for (i in 1:N) {
	Y2<--log(runif(1))
	while (Y2<=(Y1-1)^2/2){
		Y1<--log(runif(1))
		Y2<--log(runif(1))
		num.iter[i]<-num.iter[i]+1
	}
	Y[i]<-Y2-(Y1-1)^2/2
	U<-runif(1)
	if (U<=1/2) Z[i]<-mu-Y1*sigma else Z[i]<-mu+Y1*sigma
	Y1<-Y2-(Y1-1)^2/2
}
Z<-mu+sigma*Z

mean(num.iter)
sqrt(2*exp(1)/pi)

# pdf("accept4.pdf",paper="special")

string<-substitute( paste('Rejection, N( ',mu==a,', ',sigma^2==b,' ), ',N==c),
                    list(a=mu,b=sigma,c=N) )
hist(Z,prob=T,main=string)
curve(dnorm(x,mean=mu,sd=sigma),add=T, col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()


# Gamma(2,1) conditioned to exceed 5

# Example 5g page 77, Ross (2006)

1-pgamma(5,2,1)
6*exp(-5)

ratio<-function(x) exp(5/2)*x*exp(-x/2)/3
curve(ratio,0,10)
abline(v=5)

c.const<-ratio(5)
c.const
5/3

prob.accept<-
    function(x) exp(5/2)*x*exp(-x/2)/5

iter<-0
U<-1
p<-0
while (U>p) {
	Y<-5-2*log(runif(1))
	p<-prob.accept(Y)
	U<-runif(1)
	iter<-iter+1
}
X<-Y
cat("X=",X,"\n")
cat("num iter=",iter,"\n")

                                              # Simulation study
set.seed(50)
N<-5000
num.iter<-rep(0,N)
X<-rep(0,N)

for (i in 1:N) {
	U<-1
	p<-0
	while (U>p){
		Y<-5-2*log(runif(1))
		p<-prob.accept(Y)
		U<-runif(1)
		num.iter[i]<-num.iter[i]+1
	}
	X[i]<-Y
}

mean(num.iter)
c.const

# pdf("accept5.pdf",paper="special")

string<-substitute( paste('Rejection ,', X,'|',X>=5, ', ',X, '~gamma( ',alpha==a,', ',lambda==b,' ), ',N==c),
                    list(a=2,b=1,c=N) )
hist(X,prob=T,main=string,xlim=c(0,max(X)))
f<-function(x) dgamma(x,shape=2,rate=1)/(1-pgamma(5,shape=2,rate=1))
curve(f,add=T, 5,max(X),col="red")
legend("topright",c("histogram","theoretical"),lty=c(1,1),
       col=c(1,"red"),bty="n")

# dev.off()

                                              # Inverse transform method, 
                                              # numerical inversion

Fx<-function(x) (6*exp(-5)-(x+1)*exp(-x))/(6*exp(-5))

curve(Fx,5,20)
Fx(5)

F.inv<- function(u) {
	f<-function(x) Fx(x)-u
	return(uniroot(f,lower=5,upper=40)$root)
}
F.inv(0.3)

abline(h=0.3,lty=2)
abline(v=F.inv(0.3),lty=2)

U<-runif(1)
X<-F.inv(U)
print(X)

                                              # Simulation study
N<-5000
set.seed(1)
X<-rep(0,N)

for (i in 1:N){
	U<-runif(1)
	X[i]<-F.inv(U)
}

hist(X,prob=T,main=string,xlim=c(0,max(X)))
f<-function(x) dgamma(x,shape=2,rate=1)/(1-pgamma(5,shape=2,rate=1))
curve(f,add=T, 5,max(X),col="red")

                                              # See Ex 14 page 89
f<-function(x) dgamma(x,2,1)/(1-pgamma(5,2,1))

curve(f,5,10,xlim=c(0,10))
lines(c(0,5),c(0,0))
lines(c(5,5),c(0,f(5)),lty=2)

curve(dgamma(x,2,1),add=T,col="red")


Y<-0
iter<-0
while (Y<5){
	Y<-rgamma(1,shape=2,rate=1)
	iter<-iter+1
} 
X<-Y
cat("X=",X,"\n")
cat("num iter=",iter,"\n")

                                              # Simulation study
set.seed(50)
N<-5000
num.iter<-rep(0,N)
X<-rep(0,N)

for (i in 1:N) {
	Y<-0
	while (Y<5){
		Y<-rgamma(1,shape=2,rate=1)
		num.iter[i]<-num.iter[i]+1
	}
	X[i]<-Y
}

mean(num.iter)
1/(6*exp(-5))


hist(X,prob=T,main=string,xlim=c(0,max(X)))
f<-function(x) dgamma(x,shape=2,rate=1)/(1-pgamma(5,shape=2,rate=1))
curve(f,add=T, 5,max(X),col="red")


## Box-Muller algorithm for Normal rv's

S.sq<-1
iter<-0
while (S.sq>=1){
	U<-2*runif(1)-1
	V<-2*runif(1)-1
	S.sq<-U^2+V^2
	iter<-iter+1
}
W<-sqrt(-2*log(S.sq)/S.sq)
X<-U*W
Y<-V*W
cat("X=",X,"\n")
cat("Y=",Y,"\n")
cat("num iter=",iter,"\n")

                                              # Simulation study
N<-5000
set.seed(1)
X<-rep(0,N)
Y<-rep(0,N)
num.iter<-rep(0,N)

for (i in 1:N){
	S.sq<-1
	while (S.sq>=1){
		U<-2*runif(1)-1
		V<-2*runif(1)-1
		S.sq<-U^2+V^2
		num.iter[i]<-num.iter[i]+1
	}
	W<-sqrt(-2*log(S.sq)/S.sq)
	X[i]<-U*W;Y[i]<-V*W
}

mean(num.iter)
4/pi

# pdf("accept6.pdf",height=12,width=6,paper="special")
par(mfrow=c(2,1))

hist(X,prob=T)
curve(dnorm(x),add=T,col="red")

hist(Y,prob=T)
curve(dnorm(x),add=T,col="red")

par(mfrow=c(1,1))
# dev.off()