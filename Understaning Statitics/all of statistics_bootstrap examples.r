X = read.table("D:/github/Data-Mining-With-R/R Code of All of Statistics/nerve.txt")
X = as.numeric(X[,1])

#-----plot the PDF and CDF------#

t = as.data.frame(table(X))
pdf = cbind( as.numeric(as.vector(t[,1])), t[,2]/sum(t[,2]) )
cdf = cbind( pdf[,1], cumsum(t[,2])/sum(t[,2]) )
plot( pdf , xlab = "interval time of nerve firing", ylab = "Probability", main = "PDF", pch = 20)
plot(	cdf , xlab = "interval time of nerve firing", ylab = "Probability", main = "CDF", pch = 20)

#-----Use bootstrap method to calculate the se of the median------#

T = median(X)
Tboot = c(1:1000)
for (i in 1:1000){
	Xstar = sample( X, 1000, replace = T)
	Tboot[i] = median(Xstar)
	}
se = sd(Tboot)

#-----Use bootstrap method to calculate the CI of the skewness (Normal) ------#

S = ( sum((X - mean(X))^3)/length(X) )/ sd(X)^3
Sboot = c(1:1000)
for (i in 1:1000){
	Xstar = sample( X, 1000, replace = T)
	Sboot[i] = ( sum((Xstar - mean(Xstar))^3)/length(Xstar) )/ sd(Xstar)^3
	}
se = sd(Sboot)
Normal = c(S-1.96*se, S+1.96*se) #(when alpha = 0.05)

#-----Use bootstrap methond to calculate the CI of the skewness (Pecentile)------#

S = ( sum((X - mean(X))^3)/length(X) )/ sd(X)^3
Sboot = c(1:1000)
for (i in 1:1000){
	Xstar = sample( X, 1000, replace = T)
	Sboot[i] = ( sum((Xstar - mean(Xstar))^3)/length(Xstar) )/ sd(Xstar)^3
	}

alpha = 0.05
Percentile = c(as.numeric(quantile(Sboot, alpha/2)), as.numeric(quantile(Sboot, 1-alpha/2)))

#-----Use bootstrap methond to calculate the CI of the skewness (Pivotal)------#

S = ( sum((X - mean(X))^3)/length(X) )/ sd(X)^3
Sboot = c(1:1000)
for (i in 1:1000){
	Xstar = sample( X, 1000, replace = T)
	Sboot[i] = ( sum((Xstar - mean(Xstar))^3)/length(Xstar) )/ sd(Xstar)^3
	}

alpha = 0.05
Pivotal = c(2*S-as.numeric(quantile(Sboot, 1-alpha/2)),2*S-as.numeric(quantile(Sboot, alpha/2))) # or

cdfSboot = cbind(sort(Sboot),c(1:1000)/1000)
a = subset(cdfSboot[,1],cdfSboot[,2] == alpha/2) 
b = subset(cdfSboot[,1],cdfSboot[,2] == 1-alpha/2)
Pivotal = c(2*S-b, 2*S-a)

plot(cdfSboot, xlab = "bootstrap skewness", ylab = "Probability", 
	main = "CDF of bootstrap skewness", pch = 20)
lines( rbind(c(0,alpha/2), c(a,alpha/2)) ,col = "red", lwd = 2)
lines( rbind(c(0,1-alpha/2), c(b,1-alpha/2)) ,col = "red", lwd = 2)
text(1.6, 0.025, expression(alpha/2 == 1.45 ) )
text(2.2, 0.95, expression(1-alpha/2 == 2.04) )  

#----summary-----#

Normal
Percentile
Pivotal


