## 1 Getting Started
## if you haven't installed the relevant packages, do so with the following code:
## install.packages("network")
## install.packages("statnet")
## install.packages("sna")
## install.packages("numDeriv")

###########################################################
###########################################################
##2 A Brief R Tutorial
##2.1 Introduction to basic R syntax
1 + 3 # evaluation
a <- 3 # assignment
a # evaluation
a<-3 # spacing does not matter
a <- 3 # spacing does not matter
a

sqrt(a) # use the square root function
b <- sqrt(a) # use function and save result
b

d # evaluate something that is not there
a == b # is a equivalent to b?
a != b # is a not equal to b?
ls() # list objects in the global environment
help.start() # get help with R generally
?sqrt # get specific help for a function
??sqrt # looking for help pertaining to sqrt
apropos("sq") # it's on the tip of my tongue...
rm(a) # remove a single object
ls()
rm(list=ls()) # remove everything from the environment
ls()

###########################################################
##2.2 Vectors and matrices in R
#Creating vectors using the "combine" operator
c
?c
a <- c(1,3,5) # create a vector by combining values
a
a[2] # select the second element
b <- c("one","three","five") # also works with strings
b
b[2]
a <- c(a,a) # can apply recursively
a
a <- c(a,b) # mixing types---what happens?
a # all converted to the same type

#Sequences and replication
a <- seq(from=1,to=5,by=1) # from 1 to 5 the slow way
b <- 1:5 # a shortcut!
a==b # all TRUE
rep(1,times=5) # a lot of ones
rep(1:5,times=2) # repeat an entire sequence
rep(1:5,each=2) # same, but element-wise
rep(1:5,times=5:1) # can vary the count of each element

#any, all, and which (with vectors)
a <- -1:4 # create a vector
a
a>2 # some TRUE, some FALSE
any(a>2) # are any elements TRUE?
all(a>2) # are all elements TRUE?
which(a>2) # which indices are TRUE?

#From vectors to matrices
a <- matrix(data=1:25, nrow=5, ncol=5) # create a matrix the "formal" way
a
a[1,2] # select a matrix element (two dimensions)
a[1,] # just the first row
all(a[1,]==a[1,1:5]) # show the equivalence
a[,2] # can also perform for columns
a[2:3,3:5] # select submatrices
a[-1,] # nice trick: negative numbers omit cells!
a[-2,-2] # get rid of row two, column two

b <- cbind(1:5,1:5) # another way to create matrices
b
d <- rbind(1:5,1:5) # can perform with rows, too
d
cbind(b,d) # no go: must have compatible dimensions!
dim(b) # what were those dimensions, anyway?
dim(d)
nrow(b)
ncol(b)
cbind(b,b) # combining two matrices

t(b) # can transpose b
cbind(t(b),d) # now it works
rbind(t(b),d) # now it works

###########################################################
##2.3 Element-wise operations
a <- 1:5
a + 1 # addition
a * 2 # multiplication
a / 3 # division
a - 4 # subtraction
a ^ 5 # you get the idea...

a + a # also works on pairs of vectors
a * a
a + 1:6 # problem: need same length

a <- rbind(1:5,2:6) # same principles apply to matrices
b <- rbind(3:7,4:8)
a + b
a / b
a %*% t(b) # matrix multiplication

#logical operators (generally) work like arithmetic ones
a > 0 # each value greater than zero?
a == b # corresponding values equivalent?
a != b # corresponding values not equivalent?
!(a == b) # same as above
(a>2) | (b>4) # the OR operator
(a>2) & (b>4) # the AND operator
(a>2) || (b>4) # beware the "double-pipe"!
(a>2) && (b>4) # (and the "double-ampersand"!)

#ditto for many other basic transformations
log(a)
exp(b)
sqrt(a+b) # note that we can nest statements!
log((sqrt(a+b)+a)*b) # as recursive as we wanna be

###########################################################
##2.4 Data Frames
d <- data.frame(income=1:5,sane=c(T,T,T,T,F),name=LETTERS[1:5])
d
d[1,2] # acts a lot like a matrix!
d[,1]*5
d[-1,]
d$sane # can use dollar sign notation
d$sane[3]<-FALSE # making changes
d
d[2,3] # shows factors for string values

#if you want to do without factors
d$name <- LETTERS[1:5] # eliminate evil factors by overwriting
d[2,3]
d <- data.frame(income=1:5,sane=c(T,T,T,T,F),name=LETTERS[1:5],stringsAsFactors=FALSE)
d

d <- as.data.frame(cbind(1:5,2:6)) # can create from matrices
d
is.data.frame(d) # how can we tell it's not a matrix?
is.matrix(d) # the truth comes out

###########################################################
##2.5 Finding built-in data sets
#Many packages have built-in data for testing and educational purposes
data() # lists them all
?USArrests # get help on a data set
data(USArrests) # load the data set
USArrests # view the object

##2.6 Elementary visualization
#R's workhorse is the "plot" command
plot(USArrests$Murder,USArrests$UrbanPop) # using dollar sign notation
plot(USArrests$Murder,USArrests$UrbanPop,log="xy") # log-log scale

#Adding plot title and axis labels
plot(USArrests$Murder,USArrests$Assault,xlab="Murder",ylab="Assault",main="USArrests")

#Can also add text
plot(USArrests$Murder,USArrests$Assault,xlab="Murder",ylab="Assault", main="USArrests",type="n")
text(USArrests$Murder,USArrests$Assault,rownames(USArrests))

#Histograms and boxplots are often helpful
hist(USArrests$Murder)
boxplot(USArrests)
