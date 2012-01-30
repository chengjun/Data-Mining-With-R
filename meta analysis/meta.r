# meta analysis with R
# Chengjun wang @weblab 20120130

## install.packages("meta")
library(meta)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~Meta-analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Meta-analysis on Aspirin in Preventing Death after Myocardial Infarction
data(Fleiss93) # Loads specified data sets 
metabin(event.e, n.e, event.c, n.c,
    data=Fleiss93,
    studlab=paste(study, year),
    sm="OR", comb.random=FALSE)

# Meta-analysis on the Effect of Mental Health Treatment on Medical Utilisation
data(Fleiss93cont)
Fleiss93cont # see the data
# metacont Meta-analysis of continuous outcome data
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, study,
        data=Fleiss93cont, sm="SMD")

as.data.frame(ci(170, 10))  # ci:Calculation of con?dence intervals

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c,
                 data=Olkin95, subset=c(41,47,51,59),
                 sm="RR", meth="I",
                 studlab=paste(author, year))
# forest plot 
forest(meta1) 
forest(meta1, xlim="s")
forest(meta1, col.square="black", hetstat=FALSE)
forest(meta1, comb.random=FALSE,leftcols="studlab")
forest(meta1, comb.random=FALSE,leftlabs=c("Author", NA, NA, NA, NA))
forest(meta1, rightcols=c("effect", "ci"))


# Radial plot
radial(meta1, level=0.95)

# Funnel plot 
oldpar <- par(mfrow=c(2, 2))
funnel(meta1)
funnel(meta1$TE, meta1$seTE, sm="RR")
funnel(meta1, comb.fixed=TRUE,
     level=0.95, contour=c(0.9, 0.95, 0.99))$col.contour
     legend(0.05, 0.05,
     c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill=cc)
funnel(meta1$TE, meta1$seTE, sm="RR")

funnel(meta1, comb.fixed=TRUE,
        level=0.95, contour=c(0.9, 0.95, 0.99))$col.contour
        legend(0.05, 0.05,
        c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill=cc)

# L¡¯Abbe plot
labbe(meta1)


# Test for funnel plot asymmetry
metabias(meta1)
metabias(meta1, correct=TRUE)
metabias(meta1, method="linreg")
metabias(meta1, method="linreg", plotit=TRUE)
metabias(meta1, method="count")


