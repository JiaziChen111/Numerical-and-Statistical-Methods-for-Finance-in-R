
                                 ## Random Numbers ##

##############################################


#getwd()                                       # determine the working directory
                                              
#setwd("./Documents/Teaching")                  
                                              # change the working directory
                                              # . refers to the current directory
#getwd()

##############################################

## Pseudorandom Number Generation             

??modulo
                                              # integer division and modulus 
x<-17
y<-4

x/y                                           # 4.25
x%/%y                                         # integer division, =4
x%%y                                          # x mod y = 1
(x%%y)/y                                      # =0.25
y*(x%/%y)+x%%y                                # =x

x == (x %% y) + y * ( x %/% y )

N<-101
x.0<-17
x<-array(0,N)
a<-2;m<-100

x[1]<-(a*x.0)%%m

for(i in 2:N){
	x[i]<-(a*x[i-1])%%m
}
u<-x/m

u                                             # pseudorandom numbers
                                              # approx realizations from Unif(0,1)
hist(u,prob=T)                                # looks uniformly distributed

table(u)                                      # however, there are ties!!
                                              # not consistent with Unif(0,1) being
                                              # a continuous distribution


x
                                              # each x[i] assumes one of the values 
                                              # 0,1,...,m-1 
                                              # after 20 iterations a value repeats 
                                              # itself, and once it happens, the whole
                                              # sequence begins to repeat. 

which(x==68)                                  # every 20 values
which(x==36)
which(x==72)

j<-19                                         # 19-th values 
ind<-j+(1:4)*20
ind
x[ind]

matrix(x[2:N],nrow=20)                        # a sequence of 20 numbers repeated 
                                              # 5 times (the first excluded)

                                              # a and m chosen such that the number of
                                              # variables that can be generated before 
                                              # this repetition occurs is large
                                              # in particular, m should be large 
                                              # prime number
?randu
hist(randu[,2],prob=T)
?RNGkind()

                                              # .Random.seed is an integer vector,
                                              # containing the random number generator
                                              # (RNG) state for random number 
                                              # generation in R. 
                                              # It can be saved and restored through
                                              # set.seed()
                                              # in cases when we want the results 
                                              # reproducible, use set.seed() before 
                                              # generate the number.

set.seed(1)                                    
runif(10)
set.seed(1)
runif(10)                                     # the same
runif(10)                                     # different


##############################################

## Monte Carlo Integration                    

# Exercise 3 page 46, Ross (2006)

set.seed(1)
N<-1000
u<-runif(N)

g<-function(x) exp(exp(x))

#g(u)
mean(g(u))

??integral                                    # search for keyword 'integral'
?integrate                                    # perform integration of one-dim funct

integrate(g,0,1)                              # numerical integration


# Ex 5 page 47, Ross (2006)

g<-function(x) exp(x+x^2)
h<-function(y) (2-(-2))*g(-2+(2-(-2))*y)

set.seed(3)
N<-10000
u<-runif(N)

mean(h(u))

integrate(g,-2,2)


# Exercise 7 page 47, Ross (2006)

g<-function(x) exp(-x^2)                      # non normalized 
                                              # Gaussian density with mu=0, var=1/2

h<-function(y) g(1/y-1)/(y^2)                 # substitution y=1/(x+1)
                                              # so to approximate the integral
                                              # int_0^infty g(x) dx

set.seed(1)
N<-100000
u<-runif(N)

2*mean(h(u))                                  # int_{-infty}^infty g(x) dx
                                              # =2*int_0^infty g(x) dx
                                              # since g(-x)=g(x)

sqrt(2*pi/2)                                  # normalizing constant of
                                              # Gaussian density with mu=0, var=1/2

integrate(g,-Inf,+Inf)                        # numerical integration


# Estimation of pi

set.seed(1)
N<-1000
u1<-runif(N);u2<-runif(N)
Indic<-( (2*u1-1)^2+(2*u2-1)^2 ) <= 1
mean(Indic)
pi/4


# Hit-and-miss method


# pdf("hit_and_miss.pdf",width=6,height=6,paper="special")
# old.par <- par(no.readonly = TRUE)            
# par(mar=c(5, 4, 1.5, 1) + 0.1)

# f<-function(x) x^3-7*x^2+1
# curve(f,0,1,xlim=c(-0.1,1.3),ylim=c(-7,3))

# x<-c(seq(0,1,length=101),1,0,0)
# y<-c(f(seq(0,1,length=101)),-6,-6,f(0))
# polygon(x,y,col="gray75")
# x.0<-uniroot(f,lower=0.3,upper=0.5)$root
# #points(x.0,0,pch=16,cex=1.2)
# x<-c(x.0,1,1,seq(1,x.0,length=101))
# #color()
# y<-c(0,0,f(1),f(seq(1,x.0,length=101)))
# polygon(x,y,col="pink")
# abline(h=0,v=0)
# abline(h=2,v=1)
# abline(h=-6)
# text(0,-6,"a=0",cex=0.9,adj=c(0,1))
# text(1,-6,"b=1",cex=0.9, adj=c(0,1))
# text(0,-6,"c=-6",cex=0.9,adj=c(1,0))
# text(0,2,"d=2",cex=0.9,adj=c(1,0))
# exp<-expression(y==x^3-7*x^2+1)
# mtext(exp,side=3,line=0)

# par(old.par)                             
# dev.off()

set.seed(1)
N<-10000
ftn<-function(x) x^3-7*x^2+1
a<-0;b<-1;c<--6;d<-2
X<-a+runif(N)*(b-a)
#X<-runif(N, a, b)                            # the same
Y<-c+runif(N)*(d-c)
#Y<-runif(N, c, d)                            # the same
Z<-(ftn(X) >= Y)
theta<-(b - a)*c + mean(Z)*(b - a)*(d - c)
cat("theta=",theta)
-13/12

                                              # Monte-Carlo integration using the 
                                              # hit-and-miss method
                                              # ftn is a function of one variable
                                              # [a, b] is the range of integration
                                              # f.min and f.max are bounds on ftn 
                                              # over the range [a, b], that is
                                              # f.min <= ftn(x) <= f.max, x in [a, b]
                                              # n is the number of samples used 
                                              # in the estimation, that is the
                                              # number of calls of the function ftn
hit_miss <- function(ftn, a, b, f.min, f.max, n) {
  X <- runif(n, a, b)
  Y <- runif(n, f.min, f.max)
  Z <- (sapply(X,ftn) >= Y)                   # see '?sapply'
  theta<-(b - a)*f.min + (cumsum(Z)/1:n)*(b - a)*(f.max - f.min)
  ind<-seq.int(1,n,by=10)
  plot(ind,theta[ind],type="l",xlab="Number of points",
                               ylab="Approximation to the integral")
  return(theta[n])
  #cat("theta=",theta[n])
}


#RNG.state<-.Random.seed

# pdf("hit_and_miss2.pdf",width=6,height=6,paper="special")
# old.par <- par(no.readonly = TRUE)            
# par(mar=c(5, 4, 1.5, 1) + 0.1)

set.seed(1)
ftn<-function(x) x^3-7*x^2+1
hit_miss(ftn,0,1,-6,2,10000)
                                              # accuracy improves as n increases
abline(h=-13/12)

# par(old.par)                             
# dev.off()
