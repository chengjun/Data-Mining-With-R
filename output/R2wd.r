require(R2wd)
 
# install packages required
# install software RCOM, RDCOMClient 
# (I had to restart the R-Session after the above step to get it work)
 
wdGet()
 
# Regression:
data(iris)
mod <- lm(Sepal.Length ~ Species, data = iris)
regr_tab <- data.frame(summary(mod)$coefficients)
colnames(regr_tab) <- colnames(summary(mod)$coefficients)
regr_tab[ ,4] <- ifelse(regr_tab[ ,4] < .001, "< 0.001", 
                        ifelse(regr_tab[ ,4] < .01, "< 0.01", 
                        round(regr_tab[ ,4], 3)))
 
# print table to doc in word-default format:
wdTable(format(regr_tab), autoformat = 1)
 
wdSave("Regression.doc")     # save file 
wdQuit()                     # close file
