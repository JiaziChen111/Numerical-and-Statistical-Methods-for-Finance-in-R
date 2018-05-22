
                                        ## lect_9 ##

                          ## Bayesian Inference with \R ##

##############################################

## Robust Bayesian estimation of a normal mean 

                                              # normal prior

normal.select<-                               # from package 'LearnBayes'
function (quantile1, quantile2)               # let x_a: P(X<=x_a)=a (quantile)
{                                             # X sim N(mu,sigma) -> x_a=mu+sigma z_a
    p1<- quantile1$p                          # for z_a quantile of level a of N(0,1)
    x1<- quantile1$x                          # p1=a -> x1=x_p1
    p2<- quantile2$p                          
    x2<- quantile2$x                          # p2=a -> x2=x_p2
    sigma<- (x1 - x2)/(qnorm(p1)-qnorm(p2))   # x1-x2=mu+sigma z_p1-(mu+sigma z_p2)
    mu<- x1 + sigma * qnorm(p1)               #       =sigma(z_p1-z_p2)
    return(list(mu = mu, sigma = sigma))      # hence sigma=(x1-x2)/(z_p1-z_p2)
}                                             # and mu=x1+sigma*z_p1

                                              # z_a is not the usual notation
                                              # of standard normal quantiles: 
                                              # in general z_a: P(Z>z_a)=a

                                              # prior beliefs are median=100 and 95th 
                                              # percentile is 120
quantile1<-list(p=.5,x=100);quantile2=list(p=.95,x=120) 
                                              # find the normal parameter that match
                                              # this prior information
normal.select(quantile1,quantile2) 
                                              # normal prior p(theta)=N(mu,tau)
mu<-100; tau<-12.16                           # prior hyperparameters
sigma<-15                                     # known variance in 
                                              # p(y|theta)\sim N(theta, sigma^2)
n<- 4                                         # data is ybar for n=4
se<- sigma/sqrt(n)                            # standard error of ybar


ybar<- c(110, 125, 140)                       # result on 3 different IQ tests 
tau1<- 1/sqrt(n/sigma^2 + 1/tau^2)            # posterior tau
mu1<- (ybar*n/sigma^2 + mu/tau^2) * tau1^2    # posterior mu
summ1<- cbind(ybar, mu1, tau1)                  
summ1                                         # results


                                              # t prior g_T(theta)

tscale = 20/qt(0.95, 2)                       # scale of t prior
tscale 

# pdf("fig3_3.pdf",paper="special")
                                              # graphical comparison
curve((1/tscale)*dt((x-mu)/tscale,2),         # t prior has heavier tails
      from=60, to=140, xlab=expression(theta), ylab="prior density") 
curve(dnorm(x,mean=mu,sd=tau), add=TRUE, lwd=3) 
legend("topright",legend=c("t density","normal density"), 
       lwd=c(1,3),bty="n") 

# dev.off()


##############################################

## Approximate Bayesian computation (ABC)
                                             # computation of posterior mean and sd
                                             # based on an approximation of 
                                             # posterior distribution 
                                             # i.e. on a finite grid of theta values
                                             # equally-spaced
                                             
summ2<-c()                                   # initialise output
th<-seq(60,180,length=501)                   # grid of theta values equally-spaced
prior<-(1/tscale)*dt((th-mu)/tscale,2)       # t prior evaluated on the grid
                                             # pi=P(th.i<theta<th.i+1)
                                             #   ~p(th.i)*(th.i+1-th.i)
                                             # we omit (th.i+1-th.i)=0.24
                                             # as it will cancel out later
                                             # when we renormalise

for (i in 1:3) {                             # loop for posterior computation
  like<-dnorm(th,mean=ybar[i],               # for the 3 data values ybar
             sd=sigma/sqrt(n))               # likelihood evaluated at the grid  
  post<-prior*like                           # likelihood*prior
  post<-post/sum(post)                       # renormalise
  m<-sum(th*post)                            # posterior mean
  s<-sqrt(sum(th^2*post)-m^2)                # posterior standard deviation
  summ2<-rbind(summ2,c(ybar[i],m,s))         # recall var(X)= E(X^2)-[E(X)]^2
  } 
dimnames(summ2)[[2]]=c("ybar","mu1t","tau1t") 
summ2

cbind(summ1,summ2) 


# pdf("fig3_4.pdf",paper="special")

                                              # graphical comparison of prior and 
                                              # posterior densities for both prior
                                              # choices.  
th<-seq(60, 180, length=501)                  # grid of theta values equally-spaced
bin<-diff(th)[1]                              # length of the bin
x.range<-c(60,180)                            # range x-axis
normpost<-dnorm(th, mu1[3], tau1)
y.range<-c(0,max(normpost))                   # range y-axis determined by highest
                                              # value of normal posterior density

plot(th,normpost,type="l",lwd=3,              # normal posterior
     xlim<-x.range,ylim<-y.range,             # range of the axes
     xlab=expression(theta),ylab="density")   # labels of the axes
title(main="ybar=140")
curve(dnorm(x,mu, tau),add=TRUE,              # normal prior
      col="red",lwd=3)
f<-function(x)                                # t prior, see formula on slide 46
   (1/tscale)*dt((x - mu)/tscale, 2)  
curve(f,add=TRUE,col="red")                   # plot

                                              # histogram-like approximation of 
                                              # t posterior density
like<-dnorm(th,mean=ybar[3],                  # likelihood for y.bar=140
               sd=sigma/sqrt(n))              
prior<-(1/tscale)*dt((th - mu)/tscale, 2)     # prior
tpost<-prior * like                           # posterior
tpost<-tpost/sum(tpost)                       # renormalise
tpost<-tpost/bin                              # divide by width of the bins
                                              # in order to plot density on 
                                              # a appropriate scale
lines(th,tpost)                               # linear interpolation       
legend("topright",
       legend=c("t prior","normal prior","t post","normal post"),
       lwd=c(1,3,1,3),col=c(2,2,1,1),bty="n") 

# dev.off()

##############################################

## Rejection sampling                         


                                              # t prior 
mu<-100;                                      # mean
tscale<- 20/qt(0.95, 2)                       # scale 

                                              # likelihood
ybar<-140                                     # data
n<-4                                          # sample size
sigma<-15                                     # known variance 
se<- sigma/sqrt(n)                            # standard error of ybar

	
f<-function(x,para){                          # compute log of post density
	ybar<-para[1]                             # up to normalizing constant
	se<-para[2]
	mu<-para[3]
	tscale<-para[4]
	return(dnorm(x,mean=ybar,sd=se,log=TRUE)+
	       dt((x-mu)/tscale, 2,log=TRUE))
	} 
	           
para<-c(ybar,se,mu,tscale)                    # additional arguments of f
                                              # to be used inside 'optim'

options(warn = -1)
mu1t<-optim(110,f,gr = NULL, para, control = list(fnscale = -1))$par
mu1t                                          # mode of posterior density
summ2[3,2]                                    # post expect via ABC, 
                                              # slightly different

# pdf("proposal.pdf",paper="special")
                                              # plot of posterior vs proposal density
plot(th,tpost,type="l",lwd=2,
     xlab=expression(theta),,main="ybar=140",
     ylab="posterior density")                # post density from ABC
curve((1/(2*tscale))*dt((x-mu1t)/(2*tscale),4),lwd=2,
      add=TRUE,col="red")                     # proposal density is t density
                                              # mu = to mode of post expect
                                              # scale  = 2 * t prior scale
                                              # heavier tails (Why?)
abline(v=mu1t,lty=2)
legend("topright",
       legend=c("t post","proposal density"),
       lwd=c(2,2),col=c(1,2),bty="n") 

# dev.off()

# tau1t<-summ2[3,3]                           # scale =  2 * t posterior scale 
# curve((1/(2*tau1t))*dt((x - mu1t)/(2*tau1t), 4),add=TRUE,col="red",lwd=2,lty=2)
                                              # less efficient, c larger

                                              # scale = sqrt{2} * t posterior scale 
# curve((1/(sqrt(2)*tau1t))*dt((x - mu1t)/(sqrt(2)*tau1t), 4),add=TRUE,col="green")
                                              # better?

                                              # scale = t prior scale 
# curve((1/tscale)*dt((x - mu1t)/tscale, 4),add=TRUE,col="blue")
                                              # difficult to find theta.m


dfun<-function(th,datapar){                   # compute ratio between post and 
	ybar<-datapar$data[1]                     # proposal density up to normalizing
	se<-datapar$data[2]                       # constant (at log scale)
	mu<-datapar$par[1]
	tscale<-datapar$par[2]
	mu1<-datapar$par[3]
	tscale1<-datapar$par[4]
	d<-dnorm(th,mean=ybar,sd=se,log=TRUE)+
	   dt((th-mu)/tscale, 2,log=TRUE)-
	   dt((th-mu1)/tscale1, 4,log=TRUE)
	return(d)
	}

datapar<-list(data=c(ybar,se),par=c(mu,tscale,mu1t,2*tscale))
                                              # additional arguments of dfun
                                              # to be used inside 'optim'
#dfun(120,datapar)

options(warn = -1)
fit<-optim(110,dfun,gr = NULL, datapar, control = list(fnscale = -1))
options(warn = 0)
fit$par                                       # theta value at which the difference
                                              # between post density and proposal
                                              # density (at log scale) is largest
mu1t                                          # equal to posterior mode

fit$value                                     # maximum ratio up to the norm 
                                              # constant (at log scale)
                                              
                                              
                                              # Simulation study proposal
                                              
N<-5000                                       # num of simulations from proposal
#Df<-4                                        # df of proposal distribution
#Tscale<-2*tscale                             # t scale of proposal distribution
#Mean<-mu1t                                   # mean of proposal distribution
x <- rchisq(N, 4)/4
z <- rnorm(N, 0, 2*tscale)
Theta<-mu1t + z/sqrt(x)                       # simulations from the proposal 
curve((1/(2*tscale))*
      dt((x - mu1t)/(2*tscale),4),            # theoretical distribution
      from=60,to=200,col="red")
lines(density(Theta))                         # density plot of simulations

2*tscale*sqrt(4/(4-2))                        # theoretical s.d.
sd(Theta)                                     # sample s.d. of simulations



rejectsampling<-                              # rejection sampling algorithm
function (df, dmax, n, data)                  # 'df' is degree of freedom of proposal
{                                             # 'dmax' is largest ratio between 
	                                          # posterior and proposal up to
	                                          # normalizing constant at log scale
	                                          # 'n' is the num of post simulations
	                                          # from the proposal distribution
	                                          # 'data' contains additional arguments
	mu1<-data$par[3]                          
	tscale1<-data$par[4]                      
    x <- rchisq(n, df)/df                     
    z <- rnorm(n, 0, tscale1)
    theta <- mu1 + z/sqrt(x)                  # n simulations from proposal 
    prob = exp(dfun(theta,data) - dmax)       # acceptance prob p(th*|bar.y)/cg(th*)
    return(theta[runif(n) < prob])            # accept th* if U<=p(th*|bar.y)/cg(th*)
}
                                              # note that in the computation of 
                                              # 'prob'=p(th*|bar.y)/cg(th*)
                                              # there is no need to compute the
                                              # normalizing constant
                                              

set.seed(1)
th.value<-rejectsampling(df=4,dmax=fit$value,n=5000,data=datapar)

length(th.value);5000                         # approximately 1 out of 2 of th* 
                                              # simulated from g(th) are 'rejected'
                                              # hence c is approximately =2, since
                                              # c = expected number of iterations
                                              # of the algorithm needed (for 1 value)

y.range<-c(0,max(tpost)+0.01)                      # 

pdf("rejection_tpost.pdf",paper="special")

hist(th.value,prob=TRUE,ylim=y.range,
    xlab=expression(theta),ylab="posterior density",
    main="ybar=140")
lines(density(th.value),lwd=2)
lines(th,tpost,lwd=3,col="red")               # theoretical posterior density 
                                              # from ABC
abline(v=mu1t,lty=2)
legend("topright",
       legend=c("Rejection","ABC"),
       lwd=c(2,2),col=c(1,2),bty="n") 

dev.off()

mean(th.value)

quantile(th.value,c(0.025,0.975))
