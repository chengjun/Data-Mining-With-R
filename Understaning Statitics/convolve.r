# Simulate the convolving process
# Chengjun WANG
# 20120109@Lab
#~~~~~~define a piecewise function in R~~~~~~~~~~~~~#
x<-seq(-5, 5, by=0.01)
fx <- function(x) {
    res <- rep(NA, length(x))
    res[(-0.326 < x) & (x < 0.652)] <- 0.632
    res[(-1.793<x) & (x < -1.304)] <- 0.454  
    res[(1.630<x) & (x <2.119)] <- 0.227  
    return(res)
}
length(fx(x))
data<-cbind(x, fx(x))
names(data)<-c("x", "fx")
plot(data)
hist(data[,2])
barplot(data[,2], col=c(1,2))
# or, using ifelse
fx <- ifelse(x > -0.326 & x <0.625, 0.632,
   ifelse(x > -1.793 & x < -1.304,  0.454,
   ifelse(x > 1.630 & x < 2.119, 0.227, NA)))
#~~~~~~~~~~convolve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
fx2<-convolve(data[,2], data[,2])
subdata<-subset(data, is.na(data[,2])==FALSE)
plot(convolve(subdata[,1],subdata[,1]))
write.csv(subdata, "d:/github/Data-Mining-With-R/Understaning Statitics/piecewise.csv")
#



