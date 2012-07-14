t=read.csv("D:/soft/forum.csv")
t=subset(t,t[,3]<500000)
dim(t)
t1=subset(t[,1:3],t[,1]==1)
max(t[,3])
max(t[,2])
ct=t1[,3]/max(t1[,3])
cf=t1[,2]/max(t1[,2])
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency")

##------------------all curves-----------------##

plotCDFs=function(n){
	t1=subset(t[,1:3],t[,1]==n)
	ct=t1[,3]/max(t1[,3])
	cf=t1[,2]/max(t1[,2])
	points( ct,cf,col=sample(c(1:151),1),pch=20)

}

sapply(c(1:110),plotCDFs)

##--------for logistic function estimation------##

#--------------------test for case id =1-----------------------#
nls(cf~1/(1+1/(exp(a*ct))),start=list(a=1)) 
nls(cf~1/(b+1/(exp(a*ct))), control=list(maxiter = 1000),start=list(a=1,b=1) )
coes=nls(cf~c/(b+1/(exp(a*ct))), control=list(maxiter = 1000),start=list(a=1,b=1,c=1) )

lines(  ct,  1/(1+1/(exp(1.893*ct))) ,col="red" )
lines(  ct,  1/(1.23+1/(exp(3.06*ct))) ,col="green" )
lines(  ct,  0.01853/(0.02200+1/(exp(26.24536*ct))) ,col="purple" )

#---------------------------------------------------------------------#


fitCDFs=function(n){
  t1=subset(t[,1:3],t[,1]==n)
  ct=t1[,3]/max(t1[,3])
  cf=t1[,2]/max(t1[,2])
  out=try(suppressWarnings(
nls(cf~c/(b+1/(exp(a*ct))), control=list(maxiter = 1000),start=list(a=1,b=1,c=1))
  ),  silent=T)
  if (class(out)=="try-error"){return(c(0,0,0,0))}
  else {
  coeffs=as.data.frame(coef(out))[,1]
  rsquare=cor(cf,coeffs[3]/(coeffs[2]+1/(exp(coeffs[1]*ct))))^2
  n= length(ct)
  adjustedRsquare=1-(1-rsquare)*(n-1)/(n-2) 
  coees=(c(coeffs,adjustedRsquare))
return(coees)}
}	

tc=t(sapply(c(1:110),fitCDFs))

addTheoreticalCDFs=function(n){
	if (tc[n,1]!=0){
	t1=subset(t[,1:3],t[,1]==n)
	ct=t1[,3]/max(t1[,3])
	cf=t1[,2]/max(t1[,2])
	c=sample(c(1:151),1)
	points( ct,cf,col=c,pch=20)
      lines( ct,tc[n,2]/(tc[n,2]+1/(exp(tc[n,1]*ct))),col=c)
	}
}

sapply(c(1:110),addTheoreticalCDFs)

#---------observe the distribution of estimated parameters------------#
tc2=subset(tc,tc[,1]!=0)
a=tc2[,1]
b=tc2[,2]
c=tc2[,3]
adr2=tc2[,4]
plot(density(adr2),col="blue",main="PDF of adjusted r-squre")
polygon(density(adr2), col="blue", border="blue")
plot(density(a),col="red",main="PDF of parameter a")
polygon(density(a), col="red", border="red")
plot(density(b),col="gold",main="PDF of parameter b")
polygon(density(b), col="gold", border="gold")
plot(density(c),col="green",main="PDF of parameter c")
polygon(density(c), col="green", border="green")


library(scatterplot3d)
attach(mtcars)
scatterplot3d(b,c,a, pch=16, highlight.3d=TRUE,
  type="h")

library(Rcmdr)
attach(mtcars)
scatter3d(b, c, a)

#---------cluster analysis:Hierarchical Agglomerative----------------------#

tc3=tc2[,1:3] 

tc3<- na.omit(tc3) # listwise deletion of missing
tc4 <- scale(tc3) # standardize variables 

# Ward Hierarchical Clustering
d <- dist(tc4, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit,xlab="thread IDs",ylab="Ward's similarity") # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red") 

ids=subset(cbind(c(1:110),tc)[,1],cbind(c(1:110),tc)[,2]!=0)
clusterIds=cbind(ids,groups)

addTheoreticalCDFs=function(n){
	if (tc[n,1]!=0){
	t1=subset(t[,1:3],t[,1]==n)
	ct=t1[,3]/max(t1[,3])
	cf=t1[,2]/max(t1[,2])
	c=sample(c(1:151),1)
	points( ct,cf,col=c,pch=20)
      lines( ct,tc[n,2]/(tc[n,2]+1/(exp(tc[n,1]*ct))),col=c)
	}
}

ids1=subset(clusterIds[,1],clusterIds[,2]==1)
ids2=subset(clusterIds[,1],clusterIds[,2]==2)
ids3=subset(clusterIds[,1],clusterIds[,2]==3)

t1=subset(t[,1:3],t[,1]==1)
ct=t1[,3]/max(t1[,3])
cf=t1[,2]/max(t1[,2])
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type1")
sapply(ids1,addTheoreticalCDFs)
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type2")
sapply(ids2,addTheoreticalCDFs)
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type2")
sapply(ids3,addTheoreticalCDFs)

#-----------another way of clustering for comparison:k-means------#

# K-Means Clustering with 5 clusters
fit1 <- kmeans(tc3, 3)
groups2=fit1[1]$cluster
clusterIds2=cbind(ids,groups2)

nids1=subset(clusterIds2[,1],clusterIds2[,2]==1)
nids2=subset(clusterIds2[,1],clusterIds2[,2]==2)
nids3=subset(clusterIds2[,1],clusterIds2[,2]==3)


plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type1")
sapply(nids1,addTheoreticalCDFs)
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type2")
sapply(nids2,addTheoreticalCDFs)
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type2")
sapply(nids3,addTheoreticalCDFs)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(tc3, fit1$cluster, color=TRUE, shade=TRUE,
   labels=2, lines=0,main="cluster plot according to first two components ")

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster) 
pvrect(fit, alpha=.95)

#----------comparison between the results of two cluster analysis-----#

library(fpc)
cluster.stats(d, groups2, groups) 
groups3=sample(c(1:3),replace =T,length(groups))
cluster.stats(d,groups2,groups3)$ch
cluster.stats(d,groups,groups3)$ch
cluster.stats(d,groups,groups2)$ch
cluster.stats(d,groups2,groups2)$ch




