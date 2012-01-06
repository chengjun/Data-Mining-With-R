# wang chengjun
# web mining lab
# 20120105
#~~~~~~~~~~~~~~~~~~~~entropy~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# thre are blacks (20), white (0.3), and yelow people (0.5).
# micro-state: individual category
# macro-state: number of categories
# s= plogp
s1=-(0.2*log(0.2)+0.3*log(0.3)+0.5*log(0.5))
s2=-1/3*log(1/3)*3
# the case of deliberation in a village over time
#~~~~~~~~~~~~~~~~~~~~~~~~Convolution~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# http://en.wikipedia.org/wiki/Convolution_theorem
# This is known as the Convolution Theorem, where the italic F represents 
# the Fourier transform, and the splat, convolution. 
# convolve(x, y, conj = TRUE, type = c("circular", "open", "filter"))

f<- function(x) {x+x^2}
result<-integrate(f,0,50) # Integrate with R

x <- rnorm(50)
y <- rnorm(50)
par(mfrow=c(2,4))  # to compare the multiple figures
hist(x)
hist(y)
hist(xy<-convolve(x,y)) # hist(yx<-convolve(y,x))
hist(xy2<-convolve(xy, xy))# hist(yx2<-convolve(yx, yx))
hist(xy4<-convolve(xy2, xy2))
hist(xy8<-convolve(xy4, xy4))
hist(xy16<-convolve(xy8, xy8))
hist(xy32<-convolve(xy16, xy16))




