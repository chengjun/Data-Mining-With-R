###################################################
### How to Read this Book?
###################################################

R.version


###################################################
### Starting with R
###################################################

install.packages('DMwR')

installed.packages()

library()

old.packages()

update.packages()

RSiteSearch('neural networks')


###################################################
### R Objects
###################################################
x <- 945

x


y <- 39
y
y <- 43
y


z <- 5
w <- z^2
w
i <- (z*2 + 45)/2
i


(34 + 90)/12.5


ls()
rm(y)
rm(z,w,i)

###################################################
### Vectors
###################################################
v <- c(4,7,23.5,76.2,80)
v
length(v)
mode(v)


v <- c(4,7,23.5,76.2,80,"rrt")
v


u <- c(4,6,NA,2)
u
k <- c(T,F,NA,TRUE)
k

v[2]


v[1] <- 'hello'
v


x <- vector()


x[3] <- 45
x


length(x)
x[10]
x[5] <- 4
x


v <- c(45,243,78,343,445,44,56,77)
v
v <- c(v[5],v[7])
v

###################################################
### Vectorization
###################################################
v <- c(4,7,23.5,76.2,80)
x <- sqrt(v)
x


v1 <- c(4,6,87)
v2 <- c(34,32.4,12)
v1+v2


v1 <- c(4,6,8,24)
v2 <- c(10,2)
v1+v2


v1 <- c(4,6,8,24)
2*v1


###################################################
### Factors
###################################################
g <- c('f','m','m','m','f','m','f','m','f','f')
g


g <- factor(g)
g 


other.g <- factor(c('m','m','m','m','m'),levels=c('f','m'))
other.g


table(g)
table(other.g)


a <- factor(c('adult','adult','juvenile','juvenile','adult','adult',
               'adult','juvenile','adult','juvenile'))


t <- table(a,g)
margin.table(t,1)
margin.table(t,2)


prop.table(t,1)
prop.table(t,2)
prop.table(t)


###################################################
### Generating sequences
###################################################
x <- 1:1000


10:15-1
10:(15-1)


5:0


seq(-4,1,0.5)


seq(from=1,to=5,length=4)
seq(from=1,to=5,length=2)
seq(length=10,from=-2,by=.2)


rep(5,10)
rep('hi',3)
rep(1:2,3)
rep(1:2,each=3)


gl(3,5)
gl(2,5,labels=c('female','male'))


rnorm(10)


rnorm(4,mean=10,sd=3)


rt(5,df=10)


###################################################
### Indexing
###################################################
x <- c(0,-3,4,-1,45,90,-5)
x > 0


x[x>0]


x[x <= -2 | x > 5]
x[x > 40 & x < 100]


x[c(4,6)]
x[1:3]
y <- c(1,4)
x[y]


x[-1]
x[-c(4,6)]
x[-(1:3)]


pH <- c(4.5,7,7.3,8.2,6.3)
names(pH) <- c('area1','area2','mud','dam','middle')
pH


pH <- c(area1=4.5,area2=7,mud=7.3,dam=8.2,middle=6.3)


pH['mud']
pH[c('area1','dam')]



###################################################
### Matrices and Arrays
###################################################
m <- c(45,23,66,77,33,44,56,12,78,23)
m
dim(m) <- c(2,5)
m


m <- matrix(c(45,23,66,77,33,44,56,12,78,23),2,5)


m <- matrix(c(45,23,66,77,33,44,56,12,78,23),2,5,byrow=T)
m


m[2,3]


m[-2,1]
m[1,-c(3,5)]


m[1,]
m[,4]


m[1,,drop=F]
m[,4,drop=F]


m1 <- matrix(c(45,23,66,77,33,44,56,12,78,23),2,5)
m1
cbind(c(4,76),m1[,4])
m2 <- matrix(rep(10,20),4,5)
m2
m3 <- rbind(m1[1,],m2[3,])
m3


results <- matrix(c(10,30,40,50,43,56,21,30),2,4,byrow=T)
colnames(results) <- c('1qrt','2qrt','3qrt','4qrt')
rownames(results) <- c('store1','store2')
results
results['store1',]
results['store2',c('1qrt','4qrt')]


a <- array(1:24,dim=c(4,3,2))
a


a[1,3,2]
a[1,,2]
a[4,3,]
a[c(2,3),,-2]


m <- matrix(c(45,23,66,77,33,44,56,12,78,23),2,5)
m
m*3
m1 <- matrix(c(45,23,66,77,33,44),2,3)
m1
m2 <- matrix(c(12,65,32,7,4,78),2,3)
m2
m1+m2


###################################################
### Lists
###################################################
my.lst <- list(stud.id=34453, 
                stud.name="John", 
                stud.marks=c(14.3,12,15,19))


my.lst


my.lst[[1]]
my.lst[[3]]


my.lst[1]


mode(my.lst[1])
mode(my.lst[[1]])


my.lst$stud.id


names(my.lst)
names(my.lst) <- c('id','name','marks')
my.lst


my.lst$parents.names <- c("Ana","Mike")
my.lst


length(my.lst)


my.lst <- my.lst[-5]


other <- list(age=19,sex='male')
lst <- c(my.lst,other)
lst


unlist(my.lst)



###################################################
### Data Frames
###################################################
my.dataset <- data.frame(site=c('A','B','A','A','B'),
 season=c('Winter','Summer','Summer','Spring','Fall'),
 pH = c(7.4,6.3,8.6,7.2,8.9))


my.dataset[3,2]


my.dataset$pH


my.dataset[my.dataset$pH > 7,]
my.dataset[my.dataset$site == 'A','pH']
my.dataset[my.dataset$season == 'Summer',c('site','pH')]


attach(my.dataset)
my.dataset[site == 'B',]
season


subset(my.dataset,pH > 8)
subset(my.dataset,season == 'Summer',season:pH)


my.dataset[my.dataset$season == 'Summer','pH'] <- 
     my.dataset[my.dataset$season == 'Summer','pH'] + 1


my.dataset$NO3 <- c(234.5,256.6,654.1,356.7,776.4)
my.dataset


nrow(my.dataset)
ncol(my.dataset)


names(my.dataset)
names(my.dataset) <- c("area","season","pH","NO3" )
my.dataset


names(my.dataset)[4] <- "PO4"
my.dataset


###################################################
### Creating New Functions
###################################################

se <- function(x) {
   v <- var(x)
   n <- length(x)
   return(sqrt(v/n))
}


se(c(45,2,3,5,76,2,4))


basic.stats <- function(x,more=F) {
   stats <- list()
 
   clean.x <- x[!is.na(x)]
 
   stats$n <- length(x)
   stats$nNAs <- stats$n-length(clean.x)
 
   stats$mean <- mean(clean.x)
   stats$std <- sd(clean.x)
   stats$med <- median(clean.x)
   if (more) {
     stats$skew <- sum(((clean.x-stats$mean)/stats$std)^3)/length(clean.x)
     stats$kurt <- sum(((clean.x-stats$mean)/stats$std)^4)/length(clean.x) - 3
   }
   unlist(stats)
}


basic.stats(c(45,2,4,46,43,65,NA,6,-213,-3,-45))
basic.stats(c(45,2,4,46,43,65,NA,6,-213,-3,-45),more=T)



f <- function(x) {
  for(i in 1:10) {
    res <- x*i
    cat(x,'*',i,'=',res,'\n')
  }
}

