# nonparameter inference
# chengjun wang
#


# ks.test
# If y is numeric, a two-sample test of the null hypothesis 
# that x and y were drawn from the same continuous distribution is performed.
require(graphics)

x <- rnorm(50)
y <- runif(30)
hist(x)
hist(y)
# Do x and y come from the same distribution?
ks.test(x, y)


2*exp(-2*2*0.5^2)
# [1] 0.7357589
2*exp(-2*2*0.1^2)
# [1] 1.921579
(1/(2*2)*log(2/0.05))^(1/2)
# [1] 0.9603228
(1/(2*2)*log(2/0.01))^(1/2)