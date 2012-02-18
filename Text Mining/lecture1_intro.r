####################################################
#Loren Collingwood, University of Washington	   #
#UNC RTextTools Workshop						   #
#Lecture 1 -- Introduction to R			   		   #
#NOTE: Much taken from CSDE Intro to R (Cori Mar)  #
####################################################

#R as calculator:
2+3
#Respects laws of precedence
2+3*4

#############################################
#			 ---- Assignment ----- 			#
#############################################

#operator is "<-". "=" also works but less common 
x <- 2

y = 2

z <- 4

x+y*z

result <- x+y*z

result

#Everything given a name is an object and is stored in the workspace.
ls()
objects()

######################################################
# 			  -----   Vectors -----   				 #
######################################################

# Creating vectors and c() function
x <- 1:5
x

y <- 11:15
y

z <- c(10,6,3,5,1)
z

#Indexing vectors
y[2]

z[4]

y[2:4]

#Subsetting vectors
z[c(5,1,2)]

z[(x==TRUE)] #Use logical

#seq() function; good for making maps and looping
help(seq)
a <- seq(10,20)
a

a <- seq(20,10)
a

b <- seq(from=17,to=30,by=2)
b

b <- seq(17,30,2)
b
 
#Some Functions that work on vectors
vec <- c(10,5,1,12,4)
vec

vec^2

vec + vec

sum(vec) # This is a common operation

mean(vec)

sum(vec)/length(vec) #Note R's flexibility

vec2 <- c(vec,10,10,4)
vec2

table(vec2)

#Counting things in vectors: use comparison operators (==, <, >, <=, >=, !=), boolean operators ( |, & )and sum() function
vec2 == 1

sum(vec2 == 1)

vec2 == 10

sum(vec2 == 10)

vec2 > 1 & vec2 < 12 #Use & Boolean operator

length(vec2) #Very useful function for writing loops

#Check to see whether an object is a vector
is.vector(vec2)
#Convert an object to a vector
vec3 <- as.vector(vec2)

####################################################
#					Matrices					   #
####################################################
 
#Creating arrays (matrices) by using the matrix function

?matrix
x <- matrix(1:12,nrow=3)
x

dim(x)

#Converting Vector to Matrix
temp1 <- c(5,10,12,24,42,60,63,72)
temp2 <- c(8,1,3,88,33,0,77,42)

#cbind -- Column Bind. Good for matrices, works with data frames but can cause muchos problemos.
#each vector must be of same length.
temp_mat <- cbind(temp1, temp2)
temp_mat

is.matrix(temp_mat)

#rbind -- Row Bind, same logic as cbind but with rows. Each vector must be of same length.

temp_mat2 <- rbind(temp1,temp2)

#Indexing to get particular value in an array
temp_mat2[1,1:3]

temp_mat2[2,4]

temp_mat2[1:2,1:2]

temp_mat2[,1:4] #All rows, but just columns 1:4. This approach is very handy.

temp_mat2[1,] #Just row 1

###########################################
#			    DIRECTORIES			      #
###########################################
#setwd, dir(), getwd(), list.files()
# In Windows, slashes need to be around the other way. Can copy/paste from 
# Explorer address bar, then switch the slashes

setwd("/Users/lorencollingwood/Documents/consulting/rtexttools/unc_workshop")

getwd() #Look at directory

dir() #See what is in directory
list.files() #does same as above but takes longer

######################################################################
#		 Reading and Writing Data (aka opening and saving data)		 #
######################################################################

#Write out the temp_mat data
write.csv(temp_mat, "temp_mat.csv", row.names=FALSE)
dir()

temp_mat_new <- read.csv("temp_mat.csv")
class(temp_mat_new)
temp_mat_new
system("rm temp_mat.csv") # in Windows it is: shell("del temp_mat.csv")
dir() #It is removed now