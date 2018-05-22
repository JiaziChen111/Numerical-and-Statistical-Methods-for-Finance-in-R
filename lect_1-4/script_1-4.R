
                                 ## INTRODUCTION TO R ##

##############################################

## Starting and Packages                      (slide 8)

getwd()                                       # determine the working directory
#[1] "/Users/pierpaolodeblasi"
setwd("C:/Users/HOM/Desktop/SIMULATION/De Blasi/")                  
                                              # change the working directory
getwd()
setwd("./Econ/Numerical_2013/lect 1-2-3/lect 1-2-3")
                                              # . refers to the current directory
getwd()
setwd("../slides")                            # .. refers to the parent directory   
getwd()

#setwd("../lect 1-2-3/data")                  # use the menu command instead

library()                                     # see packages currently installed
library(help="datasets")                      # Information on package 'datasets'
library(UsingR)                               # attach the package UsingR 
library()                                     # list of packages currently attached
data(package="datasets")                      # get info on data in 'dataset' package 
AirPassengers                                 # data set AirPassenger from the 'dataset'
                                              # package is immediately available
data(package=.packages(all.available = TRUE)) # get info on data from all the data sets 
                                              # in packages already installed
data()                                        # data sets (mostly data frames) in all 
                                              # packages attached to the session
demo(graphics)                                # demo of the possibilities that R offers
q()                                           # quit current session and save workspace
 

##############################################

## Simple Data Analysis                       (slide 12)

library(MASS)                                 # attach the package MASS 
                                              # all data set of MASS are now accessible
                                                                                  
library(help='MASS')

hills                                         # print data frame "hills"
names(hills)								 # access to "hills" variables name
nrow(hills); ncol(hills)					 # number of rows and columns (cols) of
                                              # data frame "hills"
average<-colMeans(hills)                      # arithmetic means of the three var (cols)
average                                       # print the object 'average'
range(hills$dist)                             # range of the values in the vector 'dist'
help(range)                                   # get help on the fucntion range
min(hills$dist); max(hills$dist)              # minimum and maximum value in hills$dist
plot(time ~ dist, data=hills, pch=16)         # plot the data

out<-lm(time ~ dist, data=hills)              # fit the simple linear model: 
                                              #    time=b_0+b_1*dist+error 
out                                           # show regression coefficients                                              
summary(out)                                  # show results of regression
plot(out,which=1)                             # display diagnostic of regression fit
b<-out$coeff                                  # estimates of regression coefficients
plot(time ~ dist, data=hills, pch=16)         # plot the data
abline(b[1],b[2],col=2)                       # actual data vs fitted regression line
                                              # first argument = intercept,
                                              # second argument = slope
points(average[1],average[3],
       pch=16,col=2,cex=1.5)                  # plot average values of 'dist' and 'time'
abline(v=average[1],lty=2)                    # vertical line at mean(dist)
abline(h=average[3],lty=2)                    # horizontal line at mean(time)

##############################################

## Vectors                                    (slide 16)

coeffs<-c(1,-3,2)                             # vector containing the coefficients of
                                              # equation x^2-3x+2=0
c(1,-3,2)->coeffs                             # the same			  

class(coeffs)                                 # class of the object "coeffs"
length(coeffs)                                # dimension of the vector "coeffs"
names(coeffs)                                 # names of the elements of the vector
names(coeffs)<-c("a","b","c")                 # modified names
names(coeffs)

class(c("a","b","c"))                         # class is "character"
class(names)                                  # class is "function"
class(coeffs)	                                 # class is "numeric"

coeffs                                        # display all elements of 'coeffs'
coeffs[2]                                     # display 2nd element of 'coeffs'
coeffs["b"]                                   # the same

discr<-coeffs[2]^2-4*coeffs[1]*coeffs[3]      # b^2-4ac is discriminant of quadratic eq 
discr>0                                       # b^2-4ac>0 gives a logical value
class(discr>0)                                # class is "logical"

seq(0,3,length=200)                           # regular spaced points
-2:2                                          # sequence of integers
x<--2:2                                       # create
c(x,x)                                        # combine two vectors    
y<- rev(4:10)                                 # countdown, 10:4 is the same
y
a<-3; b<-2; n<-5;                             # use ; to separate commands
w<- a+b*(0:(n-1))                             #sequence with 'a' starting point, 
                                              # 'b' step size and 'n' number of points
c(w,y)


## Arithmetic                                 (slide 20)

ax<- 1; bx<- -3; cx<- 2                       # assign coefficients to ax, bx and cx
ax-bx+2*cx                                    # [1] 8
bx/bx*cx/ax+bx                                # [1] -1
(bx/bx)*(cx/ax)+bx                            # better, use brackets
bx^cx                                         # -3 raised to 2

sqrt(bx)                                      # [1] NaN, since bx<0
ratio<-bx/0
ratio                                         # [1] -Inf
is.infinite(ratio)                            # [1] TRUE


x<-seq(0,3,length=200)                        # regular spaced points as values of x
y<-coeffs[1]*x^2+coeffs[2]*x+coeffs[3]        # images of x under the quadratic function
                                              #     y=f(x)=a*x^2+b*x+c
plot(x,y,type="l",ylab=expression(x^2-3*x+2)) # plot as a line, y label is an expression
abline(h=0,lty=2)                             # intersections with the horizontal axis
                                              # are solutions to the equation


## Simple Functions                           (slide 23)

log(cx);log(cx,exp(1));log(x=cx,base=exp(1))  # all the same, log(2)
x<-cx/(-bx)                                   # [1] 0.6666...
round(x,2); trunc(x); ceiling(x)
help(round)

x<-seq(1,3,length=21)                         
x                                             # [1] 1 1.1 1.2 ... 2.8 2.9 3
min(x); max(x); mean(x); range(x)       
sum(x); prod(x)
cumsum(x) 
cumprod(x)

sum(x)/length(x)                              # arithmetic (or sample) mean
mean(x)                                       # the same

prod(1:5)                                     # factorial
factorial(5)                                  # the same

sum((x-mean(x))^2)/(length(x)-1)              # sample variance
var(x)                                        # the same

y<-c(1,-3,2) 
sort(y)                                       # sort the vertor into ascending order
sort(y, decreasing=TRUE)                      #descending order
seq(1,9, by=2); seq(1,9, by=3)                # "by" determines the increment
seq(1,9, length=4)                            # desired "length" of the sequence


##############################################

## Logic                                      (slide 26)

x<- 1:5						    			 # [1] 1 2 3 4 5
A<-x < 5                                      # is x less than 5?
A
B<-x > 1                                      # is x bigger than 5?
B
x > 1 & x < 5                                 # is x bigger than 1 and less than 5?
x > 1 | x < 5                                 # is x bigger than 1 or less than 5?
x == 3                                        # is x equal to 3?
x != 3                                        # is x not equal to 3?
!x == 3                                       # not (x equal to 3)
!(x == 3)                                     # better

!( (!A) | (!B) )                              # De Morgan law
A & B                                         # check

A + 1                                         # logical vector in ordinary arithmetic
sum(A)                                        # count numbers of TRUE in A
any(A)                                        # is there any TRUE in A
sum(A) > 1                                    # the same
all(A)                                        # are all elements of A TRUE 
sum(A) == length(A)                           # the same
prod(A) == 1                                  # the same
which(B)                                      # which elements of B are TRUE
B[which(B)]                                   # select the TRUE elements of B

z<-c(1:3, NA); 	                             
ind<-is.na(z)                                 # logical vector with the same size of 'z'
ind                                           # value TRUE iif the corresponding 
                                              # element in 'z' is NA


##############################################

## Character vectors and factors              (slide 29)

A<-c("beer","beer","wine","water")           
B<-c("beer","wine")                           # two vectors of class character
union(A,B)                                    # no duplicates value
intersect(A,B)
setdiff(A,B)

drinks<-factor(A)                             # coerce a character vector to a factor
drinks                                        # it is used to store categorical data
class(A); class(drinks)                       # with a specific set of values called 
                                              # levels

table(drinks)                                 # finds the unique values and tabulates 
                                              # the frequencies of their occurrences
table(A)                                      # operates the same on character vectors
                                              # by default converts vectors to factors
unclass(drinks)                               # stored as integer vector
levels(drinks)                                # returns the unique values (useful)
names(table(A))                               # the same
alcoholic<-factor(B)
union(drinks,alcoholic)                       # operates the same as with character
                                              # vectors

beer<-c(3,4,1,1,3,4,3,3,1,2,1,2)              # daily consumption of 12 people  
wine<-c(2,0,1,4,2,0,1,2,1,2,0,2)              # of beer and wine glasses 
table(beer,wine)                              # two-ways contingency table
                                              
round( table(beer,wine)/length(beer),2 )      # divide by 12, round to the second digit

##############################################

## Data frames                                (slide 31)

getwd()                                       
setwd("./data")
getwd()                                                         

#write.table(Cars93.summary,file="Cars93.summary.txt",row.names=T,sep=" ")
Cars93.summary<-read.table("Cars93.summary.txt",header=TRUE) 

Cars93.summary                                

head(Cars93.summary, n=3)                     # display the first three rows
Cars93.summary[1:3, ]                         # the same
rownames(Cars93.summary)                      # row names
names(Cars93.summary)                         # variable (column) names
colnames(Cars93.summary)                      # the same
summary(Cars93.summary)                       # summary statistics: min, max, quartiles
                                              # and mean for numeric vectors; levels and
                                              # frequencies for factors.

data.frame( X1=1:10, X2=I(letters[1:10]),     # create a data frame
            X3=factor(letters[1:10]) )  
data.frame( 1:10, I(letters[1:10]),           # R attemps to pick row and column names
            factor(letters[1:10]) )           # from the constituent vectors
my.data<-data.frame( 1:10, I(letters[1:10]),
                     factor(letters[1:10]) )             
names(my.data)<-c("X1","X2","X3")             # assign the names to variables
my.data                                       

Min.passengers                                # error, object not found
Cars93.summary$Min.passenger                  # it is now accessed as a vector 

attach(Cars93.summary)                        # attach data frame
Min.passengers                                # now the object is accessed by its name
class(abbrev)                                 # 'abbrev' is a vector of class factor
detach(Cars93.summary)                        # detach data frame

library(MASS)                                 # attach MASS package
head(Cars93,n=3)                              # display the first three rows of 'Cars93'
Cars93[1:5,c(3,18)]                           # 'Type' and 'Passengers' are the 3rd and
                                              # 18th variables of the data frame Cars93

Cars93.summary                                # what we aim at reproducing each var
class(Cars93$Type)                            # factor

attach(Cars93)                                # attach data frame

names(Cars93)
Type
aggregate(Passengers,by=list(Type),min)       # reproduce first var of 'Cars93.summary'
aggregate(Passengers,by=list(Type),max)       # reproduce second var of 'Cars93.summary'
Cars93.summary                                # check
table(Type)                                   # reproduce third var of 'Cars93.summary'
levels(Type)                                  # check

detach(Cars93)                                # detach data frame

my.Cars<-stack(Cars93.summary, select=c(1,2)) # concatenates selected cols into a single 
my.Cars                                       # column named "values", and add a factor 
                                              # named "ind" that has names of the 
                                              # concatenated columns as levels
unstack(my.Cars)                              # reverse the stacking operation

library(MASS)                                 
head(hills,n=3)                               # back to 'hills' data from MASS package
hills$time<-round(hills$time*60)              # express time in minutes (no decimal)
hills$time                                    # display new version
lapply(hills,max)                             # maximum of each columns as a list
sapply(hills, max)                            # the same, but as vector, see 
                                              # help(lapply)

science<-read.table("science.txt",header=TRUE) 

head(science,n=3)

!complete.cases(science)                      # Which rows have missing values?
index<-which(!complete.cases(science))
index
dim(science)                                  # 1385 observations (rows)
Science<-science[-ind,]                       # Omit rows that contain missing values
dim(Science)                                  # 1383 observations
Science<- na.omit(science)                    # the same
dim(Science)


##############################################

## Lists                                      (slide 38)

x.lis<-list(a=1:10, b=letters[1:3],           # create a list
            d=factor(rep(letters[1:4],2)))    # 'rep(x,n)' repeat values of 'x' n times 
x.lis
x.lis$a                                       # object 1 is numeric
x.lis[[2]]                                    # object 2 is of class 'character'
                                              # note double square bracket
table(x.lis$d)                                # form a table for object 3, a factor

e<-list(e=10:1)
c(x.lis,e)                                    # combine two lists

                                              
##############################################

## Matrices                                   (slide 39)

x.mat<-matrix(1:12,nrow=3,ncol=4)             # create a matrix
x.mat
x.mat<-matrix(1:12, nrow=3)                   # the same 
x.mat                                         
dim(x.mat)                                    # 'x.mat' is a 3x4-matrix
dimnames(x.mat)<-list(paste("row",1:3,sep=""),# assign names to rows and columns
             paste("col",1:4,sep=""))         # 'paste(x,y)' concatenates elementwise 
                                              # 'x' and 'y' after converting them to
                                              # character
x.mat

matrix( paste(                                # a 4x2-matrix of character arguments
         paste("entry",rep(1:4, 2),sep=" "),  # combined use of 'paste' and 'rep'
        rep(1:2, each=4),sep=","),            # with 'rep(x,each=n)' each element of 'x'  
        nrow=4,ncol=2)                        # is repeated n times.

xx<-cbind(x.mat,x.mat)                        # combine x with itself by columns
xx                                            # 
xxx<-rbind(x.mat,x.mat)                       # combine x with itself by rows
xxx                                           # 
rbind(xx,xxx)                                 # Error: dimensions do not coincide

Year<-c(1800,1850,1900,1950,2000)             # create vector of time points (in years)
Carbon<-c(8,54,534,1630,6611)                 # production level for each year
plot(Year,Carbon,pch=16,cex=1.5)              # plot Carbon against Year 
                                              # 'pch' is the point character 
                                              # 'cex' is the size of the point

fossifuel<-data.frame(year=Year,carbon=Carbon)# create a dataframe
fossifuel                                     # 

fossifuel.mat<-matrix(c(Year,Carbon),nrow=5)  # create a matrix, 
colnames(fossifuel.mat)<-c("year","carbon")   # assign names to columns
fossifuel.mat                                 # compare to data frame (no rows names) 
cbind(Year,Carbon)                            # the same as 'fossifuel.mat', but
                                              # column names are automatically assigned                             


## Indexing                                   

x<- 10*(1:10)                                 # [1] 10, 20, ... ,100
names(x)<- letters[1:10]                      # assign names as "a", "b", ... , "j" 
x
x[1:3]                                        # elements 1,2,3
x[c(-1,-2)]                                   # remove 10 and 20
#x>50
x[x>50]                                       # elements > 50
#which(x>50)
x[which(x>50)]                                # the same
x[c("a","d")]                                 # elements 10 and 40
x[c(1,4)]                                     # the same

x.mat<- matrix(1:100, ncol=10)                # create a (10,10)-matrix
x.mat
x.mat[1:5,]                                   # first 5 rows, all columns
x.mat[1:4,x<30]                               # check this


## Matrix algebra

x.mat<- matrix(1:10, ncol=2)                  # create a (5,2)-matrix
x.mat
x.mat + 1                                     # sum by a scalar (recycling rule)
x.mat + x.mat                                 # sum of two matrices
t(x.mat)                                      # transpose of the matrix 
x.mat %*% t(x.mat)                            # matrix product, 
x.mat %*% x.mat                               # non comformable for multiplication

X<- matrix(sample(1:10,24,replace=T),ncol=6)  # sample 24 times from 1,...,10 
                                              # with replacement
X                                             # fill the values in a (4,6)-matrix 
matrix(sample(1:10, 24),ncol=3)               # not able to take sample larger then
                                              # population without replacement

y<-c(-1,0.5,0.3,0.2)                          # 4-dimensional vector
t(X) %*% y                                    # matrix product X'y, linear combination
                                              # of the rows of X
crossprod(X,y)                                # the same
crossprod(X)                                  # matrix product X'X
crossprod(t(X))                               # matrix product XX'

A<-crossprod(t(X))                            # symmetric (4,4)-matrix
v<-eigen(A)                                   # list with eigenvalues and eigenvectors
v                                             # 'v$values' and 'v$vectors'
det(A); prod(v$values)                        # det(A)=prod_i(eigenvalues(A)_i)
crossprod(v$vectors[,2])                      # eigenvectors are normalized
crossprod(v$vectors[,1],v$vectors[,2])        # the matrix formed by concatenating 
                                              # eigenvectors is orthogonal 
round(crossprod(v$vectors[,1],v$vectors[,2]),2)
                                              # rounding error, essentially = 0
solve(A)                                      # inverse of A
A%*%solve(A)                                  # AA^(-1)= identity matrix
round(A%*%solve(A),2)                         

P<-v$vectors                                  # orthonormal matrix with eigenvectors 
apply(P,1,sum)                                # sum row-wise
apply(P,2,sum)                                # sum column-wise
apply(P,2,crossprod)                          # check that eigenvectors are normalized

## More on Linear Algebra: decomposition of A
 
Lambda<-diag(v$values)                        # diagonal matrix with eigenvalues
Lambda
inv.Lambda<-diag(v$values^(-1))               # inverse of a diagonal matrix
inv.A<-P%*%inv.Lambda%*%t(P)                  # inverse(A)= P*Lambda^{-1}P'
inv.A;solve(A)                                # check

                                              # ATTENTION: A must be positive definite!
square.root.A<-P%*%sqrt(Lambda)%*%t(P)        # A^{1/2}=P*Lambda^{1/2}*P' 
                                              # 'sqrt' operates element-wise 
square.root.A%*%square.root.A;A               # check

B<-P%*%sqrt(Lambda)                           # decomposition A=BB'
crossprod(t(B)); A                            # check
C<-chol(A)                                    # choleski decomposition A=C'C 
C                                             # with C upper triangular
crossprod(C); A                               # check
help.search("Choleski")                       # search engine
?? choleski								 # the same

##############################################

## Functions in R                             (slide 46)

args(c)                                       # examine the arguments of 'c'
args(plot)                                    # examine the arguments of 'plot'
                                              # see 'help(plot)' or 'help(plot.default)'
                                              # for a complete list
args(plot.default)
args(lm)                                      # examine the arguments of 'lm'

Year<-c(1800,1850,1900,1950,2000)             
Carbon<-c(8,54,534,1630,6611)                 
plot(Year,Carbon,type="l")                    # call of 'plot' with arguments by order
                                              # and by name 
                                              # 'type="l"' produce a line instead of
                                              # a scatter plot ('type="p"' for points)

##############################################

## Graphics in R                              (slide 50)

primates<-read.table("primates.txt",header=TRUE) 

primates                                      # two variables, 5 observations
plot(Brainwt~Bodywt, data=primates)           # plot brain weight against body weight 
plot(primates$Bodywt,primates$Brainwt)        # the same

attach(primates)                              # attach data frame to the workspace
                                              # the column names are visible                             

                                              # Place labels on points and axes
plot(Brainwt~Bodywt, xlim=c(0, 300),          # 'xlim' sets the range of x-axis
     xlab="Body weight (kg)",                 # 'xlab' and 'ylab' set the axes labels
     ylab="Brain weight (g)", pch=16)         # 'pch' controls the plotting symbol
text(Bodywt,Brainwt,                          # pick labels from row names
     labels=row.names(primates), pos=4)       # 'pos=4', 1=below, 2=to the left, 
                                              # 3=above, 4=to the right
mtext("Primates data", 3, line=1, cex=1.5)    # add text to the margin, 
                                              # leaving 1-line space
                                              # 'side=3', where 1=x-axis, 2=y-axis, 
                                              # 3=top axis, 4=right-vertical axis
                                              
                                              # alternative way af adding labels
plot(Bodywt,Brainwt,xlim=c(0, 300),           
     xlab="",ylab="",pch=16)                  # 'xlab' and 'ylab' are left unspecified
#plot(Bodywt,Brainwt,xlim=c(0, 300),pch=16)
text(Bodywt,Brainwt,                          # flexibility in the way labels 
     labels=row.names(primates), pos=4)       # are displayed (graphical setting)
title(xlab=list("Body weight (kg)",cex=1.5),  # x-axis label specified as a list 
      ylab=list("Brain weight (g)",cex=1.5),  # containing the string and a selection
      main=list("Primates data",cex=1.5))     #  of optional par
                                              # 'main' is an argument of 'plot' used to 
                                              # specify the title on top, default  NULL 
mtext(side=3,line=0.75,"(a)",cex=1.5,adj=0)   # can be used instead of 'mtext' 
                                              # (the latter has greater applicability) 
                                              # 'adj' adjust the string in reading
                                              # direction default is center, 
                                              # 'adj=0' for left alignment, 
                                              # 'adj=1' for right alignement

detach(primates)                              

## The use of color 

theta <- (1:50)*0.92                          # 1st coordinate is in angle (radiants) 
plot(theta, sin(theta), col=1:50,             # plot *sin(theta)' (elementwise) against 
     pch=16, cex=4)                           # 'theta' with points of different colors
                                              # 'pch=16' set the symbol as a circle
points(theta, cos(theta), col=51:100,         # add points (theta,cos(theta)) of more  
	   pch=15, cex=4)                         # different colors
                                              # 'pch=15' set the symbol as a square

theta <- (1:400)*(pi/40)                      # 1st coordinate is in angle (radiants) 
plot(theta, sin(theta), type="l", col="red",  # plot 'sin(theta)' as a red line (col=2)
     lty=1, lwd=2)                            # 'lty=1' set the line type (solid)
                                              # 'lwd=2' set the line width, default is 1
lines(theta, cos(theta), col="green",         #  add a green line (col=3) for cos(theta)
     lty=2, lwd=2)                            # 'lty=2' set the line type as dashed

##############################################

## R Object Storage                           (slide 52)

ls(all=T)                                     # see the contents of the workspace
objects()                                     # objects created by the user
search()                                      # collection of databases (search path)  
                                              # used by R to store objects
attach(attitude)                              # 'attach' put a list in the second 
                                              # position of the path, so that its 
                                              #  elements areaccessed directly by name 
search()                                      # see the change
detach(attitude)

rm(list=ls())
ls()
X<- matrix(sample(1:10,24,replace=T),ncol=6)  
ls()
object.size(X)                                # bytes of the memory allocation
rm(X)                                         # remove object X
ls()                                          # check


##############################################

## Importing Data in R                        (slide 54)

my.hills<-read.table("hills.txt",header=TRUE) # read a text file into data frame
my.hills                                      # same as 'hills' from MASS

Year<-c(1800,1850,1900,1950,2000)             
Carbon<-c(8,54,534,1630,6611)                 
write.table(                                  # write a data frame into a csv file
  data.frame(year=Year,carbon=Carbon),        
  file="fuel.csv",row.names=F,sep=",")
fossifuel<-read.csv("fuel.csv")               # reverse action
fossifuel                                     # return a data frame storing the data
fossifuel<-read.table("fuel.csv",             # the same
                      header=TRUE,sep=",")    # read a text file with columns separated 
fossifuel                                     # by commas into a data frame

write(Carbon,file="Carbon.txt")               # write a vector into a text file
scan("Carbon.txt")                            # read a text file into a vector

matrix(1:10,ncol=2)
write(matrix(1:10,ncol=2),file="x.mat.txt")   # write a matrix into a text file, 
                                              # ATTENTION:columns become rows
scan("x.mat.txt")                             # ATTENTION: it creates a vector
matrix(scan("x.mat.txt"),ncol=2)              # to get back a matrix use 'matrix()' 


##############################################

## Getting help                               (slide 57)

help(mean)                                    # access to help document of 'mean'
?mean                                         # the same

help.search("regression")                     # if you do not remember the name of the 
                                              # desired function, it searches each entry 
                                              # in help system and returns any matches
                                              # with the word 'regression'
??regression                                  # the same

apropos("mean")                               # match just function names
help.start()                                  # explore the help pages in HMTL format,
                                              # it will open the web browser
example(mean)                                 # examples on how to use 'mean()'
                                              # copy and paste on the console
                                    
                                              
##############################################

## Portability                                (slide 58)

Year<-c(1800,1850,1900,1950,2000)             
Carbon<-c(8,54,534,1630,6611)                 
plot(Year,Carbon,type="l")
mtext("Fossifuel data", 3, line=1, cex=1.5)   
                                              # Copy on Clipboard from the File menu,
                                              # or save the file in a specific format, 
                                              # again from the File menu 


pdf("file.pdf")                               # pdf file saved in the directory
plot(Year,Carbon,type="l")
mtext("Fossifuel data", 3, line=1, cex=1.5)
dev.off()

postscript("file.ps")                         # Postscript file
plot(Year,Carbon,type="l")                    # as default a ps file is in landscape
mtext("Fossifuel data", 3, line=1, cex=1.5)
dev.off()

pdf("file.pdf",height=8,width=8)              # plot region specified by height & width
plot(Year,Carbon,type="l")
mtext("Fossifuel data", 3, line=1, cex=1.5)
dev.off()

pdf("file.pdf",paper="special",
    height=8,width=8)                         # paper size specified by height & width
plot(Year,Carbon,type="l")
mtext("Fossifuel data", 3, line=1, cex=1.5)
dev.off()


##############################################

## Basic programming                          (slide 60)

## program prog.R
                                              # find the zeros of a*x^2+b*x+c=0

rm(list=ls())                                 # clear the workspace 

a<-1                                          # input
b<-4
c<-2

root1<-(-b+sqrt(b^2-4*a*c))/(2*a)             # calculation
root2<-(-b-sqrt(b^2-4*a*c))/(2*a)

print(c(root1, root2))                        # output

                                              # while executing a program with
                                              # 'source("prog.R")' we can not display
                                              # the value of a variable by simply
                                              # typing its name
show(c(root1, root2))                         # the same

getwd()                                       # check the working directory
source("prog.R")


## Conditional Evaluation                     (slide 19)

                                              # simple example of 'if' statement
x<-sample(1:10,1)                             # x is a value sampled from 1,...,10
if (x>4) {                                    # is it larger than 4?
  y<-1                                        # if so set y=1
} else {                                      # otherwise (i.e. if x<=4)
  y<-0                                        # set y=2
}
cat("x=",x);cat(" y=",y)                      # 'cat' prints its arguments
                                              # the advantage of 'cat' over 'show'
                                              # is that it allows us to combine text
                                              # and variables together

x<-sample(1:10,1)                             # the same, no braces used
if (x>4)
	y<-1 else y<-0
cat("x=",x);cat(" y=",y)

x<-sample(1:10,1)                             # this leads to an error, 
if (x>4)                                      # there is a new line between true.branch
	y<-1                                      # and 'else'
	else y<-0
cat("x=",x);cat(" y=",y)


x<-sample(1:10,1)                             # example of two nested 'if' statements 
count<-0                                      # to get count=0 if x<=3
if (x>3) {                                    # count=1  if 3<x<6 and count=2 if x>=6
  if (x<6) {
  	count<-1
    } else count<-2
  }
cat("x=",x,"\n");cat("count=",count)          # only newline created explicitly by "\n"
                                              # are printed
 
count<-0                                      # this leads a syntax error: 
if (x>3)                                      # there is a new line between } closing
  if (x<6) {                                  # the true branch and 'else'
   	count<-1                     
  	}   
  	else count<-2

count<-0                                      # this is ok 
if (x>3)                                      # no use of {} to delimit branches
  if (x<6) 
    count<-1 else  count<-2
cat("x=",x,"\n");cat("count=",count)          

count<-0                                      # this is ok
if (x>3) {                                    # here we use {} to delimit both true
  if (x<6) {                                  # and false branches
    count<-1 
    } else {
      count<-2
      }
  }
cat("x=",x,"\n");cat("count=",count)          


x<-sample(1:10,1)
count<-0                                       
if (x>3 & x<6) {                              # compound condition by the use of 
  count<-1                                    # logic AND to check whether 3<x<6
  } else count<-2                             # condition has to be TRUE or FALSE
                                              # i.e. logic vector of length 1
cat("x=",x,"\n");cat("count=",count)          # count=1 if 3<x<6, count=2 otherwise



## program prog2.R
# find the zeros of a*x^2+b*x+c=0

rm(list=ls())                                 # clear the workspace 

a<-1                                          # input
b<-4
c<-2

discrim<-b^2-4*a*c                            # calculate the discriminant

                                              # calculate the roots depending on the
                                              # value of the discriminant
if (discrim > 0) {
    roots <- c( (-b+sqrt(discrim))/(2*a),
                (-b-sqrt(discrim))/(2*a) )
} else {
    if (discrim == 0) {
        roots <- -b/(2*a)
    } else {
        roots <- c()
    }
}

show(roots)                                   # output

source("prog2.R")

                                              # expressions grouped using {} are viewed
                                              # by R as a single expression. 
                                              # Similarly an if command is viewed as
                                              # a single expression.
                                              # Thus the previous code can be writtem
                                              # equivalently (and more clearly)  as
if (discrim > 0) {
    roots <- c( (-b+sqrt(b^2-4*a*c))/(2*a),
                (-b-sqrt(b^2-4*a*c))/(2*a) )
} else if (discrim == 0) {
        roots <- -b/(2*a)
} else {
        roots <- c()
}
show(roots)

# EXERCISE:
# try to use additional if statements to rewrite program prog2.R 
# so that it can deal with the case a=0


## Iteration                                  (slide 63)

## Looping with 'for'

                                              # summing a vector
x_list <- seq(1,9,by=2)
sum_x<-0
for (x in x_list) {
     sum_x<-sum_x+x
     cat("The current loop element is",x,"\n")
     cat("The cumulative total is",sum_x,"\n")
    }

sum(x_list)                                   # the same

## program nfact1.R
                                              # calculate n factorial

rm(list=ls())                                 # clear the workspace

n <- 6                                        # Input

                                              # Calculation
n_factorial <- 1
for (i in 1:n) {
    n_factorial <- n_factorial * i
    }

show(n_factorial)                             # Output

prod(1:n)                                     # the same
factorial(n)                                  # the same

                                              # loop used for indexing
                                              
m<-100
x<- (1:m)*(pi/40);                            # evaluate the sin of each element of 'x'
y<-rep(0,length=m)                            # and store them into the vector 'y' 
for(i in 1:m) {                               # loop, i serves as index for y[] and x[]
	y[i]<-sin(x[i]) 
	}     
y

for(i in 1:length(x)) y[i]<-sin(x[i])         # the same (without {...}, only 1 command)
y

y<- sin(x)                                    # the same but faster 
y                                             # try m=100000 instead of m=100

help(Control)                                 # help pages

                                              # the following two programs produce 
                                              # the same result, but the first is faster
## Program 1
n<-100000
x<-rep(0,n)
for (i in 1:n) {
     x[i]<-i
    }                                         # faster

## Program 2
n<-100000
x<-1
for (i in 1:n) {
     x[i]<-i
    }                                         # slower: changing the size of 'x' at each
                                              # iteration takes about as long as
                                              # creating a new vector does

## Looping with 'while'


## program fibonacci.R
                                              # calculate the first Fibonacci number
                                              # greater than 100

rm(list=ls())                                 # clear the workspace

                                              # initialise variables
Fib <- c(1, 1)                                # list of Fibonacci numbers
n <- 2                                        # length of F

                                              # iteratively calculate new Fibonacci num
while (Fib[n] <= 100) {
	# cat("n =", n, " Fib[n] =", Fib[n], "\n")
    n <- n + 1
    Fib[n] <- Fib[n-1] + Fib[n-2]
}

                                              # output
cat("The first Fibonacci number > 100 is F(", n, ") =", Fib[n], "\n")

Fib

## program compound.R
                                              # Duration of a loan under compound 
                                              # interest

rm(list=ls())                                 # clear the workspace

                                              # Inputs
r <- 0.11                                     # Annual interest rate
period <- 1/12                                # Time between repayments (in years)
debt_initial <- 1000                          # Amount borrowed
repayments <- 12                              # Amount repaid each period

                                              # Calculations
time <- 0
debt <- debt_initial
while (debt > 0) {
    time <- time + period
    debt <- debt*(1 + r*period) - repayments
}

                                              # Output
cat('Loan will be repaid in', time, 'years\n')

## Basic debugging

## program threexplus1.R

x <- 3
for (i in 1:3) {
  show(x)
  cat("i = ", i, "\n")
  if (x %% 2 == 0) {                            # '%%' for the modulus
    x <- x/2                                    # '%/%' for integer division
  } else {
    x <- 3*x + 1
  }
}
show(x)




