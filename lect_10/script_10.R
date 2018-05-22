
                                        ## lect_10 ##

                                   ## Poisson Process ##

##############################################

## Trajectory

lambda<-2
#set.seed(1)
#X<-rexp(10,rate=lambda)
X<-c(0.3776,0.5908,0.0729,0.0699,0.2180,
     1.4475,0.6148,0.2698,0.4782,0.0735)
S<-cumsum(X)
N<-length(X)

?stepfun

string<-substitute( paste('Poisson process, ',lambda==a),
                    list(a=lambda) )

# pdf("trajectory.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:N),pch=16,lwd=2,xlim=c(0,5),ylim=c(0,10.5),
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")

# dev.off()

## Generating a Poisson Process (1)

lambda<-2
T.fin<-6
t<-0
j<-0
S<-c()
set.seed(1)
while (t<T.fin){
	U<-runif(1)
	t<-t-log(U)/lambda
	j<-j+1
	S<-c(S,t)
}
S<-S[-j]
S
cat("N(T)=",j-1)

string<-substitute( paste('Poisson process up to ',T==b, ', ',lambda==a),
                    list(a=lambda,b=T.fin) )

# pdf("pois_proc1.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:(j-1)),pch=16,lwd=2,ylim=c(0,j-0.5),xlim=c(0,T.fin),
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")
     
# dev.off()     
 
## Generating a Poisson Process (2)

pois.sim <- function(l){
    X<-0
    px<-exp(-l)
    Fx<-px
    U<-runif(1)
    while (Fx<U) {
        X<-X+1
        px<-px*l/X
        Fx<-Fx+px
    }
    return(X)
}

lambda<-2
T.fin<-6
set.seed(1)
N<-pois.sim(lambda*T.fin)
U<-runif(N)
S<-U*T.fin
S<-sort(S)
S
cat("N(T)=",N)

string<-substitute( paste('Poisson process up to ',T==b, ', ',lambda==a),
                    list(a=lambda,b=T.fin) )

# pdf("pois_proc2.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:N),pch=16,lwd=2,ylim=c(0,N+0.5),xlim=c(0,T.fin),
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")
     
# dev.off()     
 
##############################################

## Nonohomogeneous Poisson process
 

# Thinning algorithm 

lambda.fun<-function(t) t
T.fin<-6
lambda<-6

set.seed(1)
t<-0
j<-0
S<-c()
iter<-0
while (t<T.fin){
	U<-runif(1)
	iter<-iter+1
	t<-t-log(U)/lambda
	U<-runif(1)
	iter<-iter+1
	if (U <= (lambda.fun(t)/lambda)){
		j<-j+1
		S<-c(S,t)
	}
}
S<-S[-j]
S
cat("N(T)=",j-1)
cat("num Unif generated=",iter)

string<-substitute( paste('Nonhom Pois proc up to ',T==b, ', ',lambda(t)==t),
                    list(a=lambda,b=T.fin) )

# pdf("non_pois_proc1.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:(j-1)),pch=16,lwd=2,ylim=c(0,j-0.5),xlim=c(0,T.fin),
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")
     
# dev.off()     
 
## Efficient thinning algorithm 

T.fin<-6
k<-11
#t.i<-(1:12)/2
#t.i
t.i<-seq(0,T.fin,length=k+2)[-1]
t.i
lambda.i<-lambda.fun(t.i)                          # lambda(t) is increasing

t<-0
j<-0
S<-c()
set.seed(1)
U<-runif(1)
X<--log(U)/lambda.i[1]
iter<-1
for (i in 1:(k+1)) {
	# cat("i=",i,"\n")
	# cat("t=",t,"\n")
	# cat("X=",X,"\n")
	# cat("t.i=",t.i[i],"\n\n")
	while (t+X<t.i[i]){
		t<-t+X
		U<-runif(1)
		iter<-iter+1
		if (U <= (lambda.fun(t)/lambda.i[i])){
			j<-j+1
			S<-c(S,t)
		}
		U<-runif(1)
		iter<-iter+1
		X<--log(U)/lambda.i[i]
		# cat("j=",j,"\n")
		# cat("S[j]=",t,"\n")
		# cat("t=",t,"\n")
		# cat("X=",X,"\n\n")
	}
	X<-lambda.i[i]*(X-(t.i[i]-t))/lambda.i[i+1]
	t<-t.i[i]
	# cat("t=",t,"\n")
	# cat("X=",X,"\n")
	# cat("t.i=",t.i[i],"\n\n\n")
}
S
cat("N(T)=",j)
cat("num Unif generated=",iter)


string<-substitute( paste('Nonhom Pois proc up to ',T==b, ', ',lambda(t)==t),
                    list(a=lambda,b=T.fin) )

# pdf("non_pois_proc2.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:j),pch=16,lwd=2,ylim=c(0,j+0.5),xlim=c(0,T.fin),axes=F,
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")
axis(2)
axis(1,at=c(0,t.i))
     
# dev.off()     
  
  
## Merging
                                                   # inefficient
set.seed(1)
lambda.fun<-function(t) t+10
T.fin<-1
lambda<-11

t<-0
j<-0
S<-c()
iter<-0
while (t<T.fin){
	U<-runif(1)
	iter<-iter+1
	t<-t-log(U)/lambda
	U<-runif(1)
	iter<-iter+1
	if (U <= (lambda.fun(t)/lambda)){
		j<-j+1
		S<-c(S,t)
	}
}
S<-S[-j]
S
cat("N(T)=",j-1)
cat("num Unif generated=",iter)

                                                   # efficient via merging
set.seed(1)
lambda.fun<-function(t) t+10
T.fin<-1
under.l<-10

t<-0
j<-0
S<-c()
iter<-0
while (t<T.fin){
	U<-runif(1)
	iter<-iter+1
	t<-t-log(U)/under.l
	j<-j+1
	S<-c(S,t)
}
S<-S[-j]
S
cat("N(T)=",j-1)
cat("num Unif generated=",iter)

t<-0
j<-j-1
lambda<-1
while (t<T.fin){
	U<-runif(1)
	iter<-iter+1
	t<-t-log(U)/lambda
	U<-runif(1)
	iter<-iter+1
	if (U <= ((lambda.fun(t)-under.l)/lambda)){
		j<-j+1
		S<-c(S,t)
	}
}
S<-S[-j]
S
S<-sort(S)
S
cat("N(T)=",j-1)
cat("num Unif generated=",iter)

string<-substitute( paste('Nonhom Pois proc up to ',T==b, ', ',lambda(t)==t),
                    list(a=lambda,b=T.fin) )

# pdf("non_pois_proc3.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:(j-1)),pch=16,lwd=2,ylim=c(0,j-0.5),xlim=c(0,T.fin),
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")
     
# dev.off()     


## Inverse transform algorithm

T.fin<-20
a<-0.1

set.seed(100)
t<-0
j<-0
S<-c(0)
while (t<T.fin){
	j<-j+1
	U<-runif(1)
	t<-(S[j]+a*U)/(1-U)
	S<-c(S,t)
}
S
j
S<-S[-c(1,j+1)]
S
cat("N(T)=",j-1)

string<-substitute( paste('Nonhom Pois proc up to ',T==b, ', ',lambda(t)==1/(t+a)),
                    list(a=a,b=T.fin) )

# pdf("non_pois_proc4.pdf",height=6,width=7,paper="special")

plot(stepfun(S,0:(j-1)),pch=16,lwd=2,ylim=c(0,j-0.5),xlim=c(0,T.fin),
     main=string,ylab="N(t)",xlab="time",bty="L",yaxs="i",xaxs="i")
     
# dev.off()     



