How to Make HTML5 Slides with knitr
========================================================
% Refer to http://yihui.name/en/2012/05/how-to-make-html5-slides-with-knitr/
% Writing beautiful and reproducible slides quickly
% Chengjun Wang
% 2012/06/6

```{r setup, include=FALSE}
# set global chunk options
opts_chunk$set(fig.path='D:/github/Data-Mining-With-R/knitr/figure/slides-', cache.path='D:/github/Data-Mining-With-R/knitr/cache/slides-', cache=TRUE)
# upload images automatically
opts_knit$set(upload.fun = imgur_upload)
```
# Why

- after you finished typing `\documentclass{beamer}` and `\title{}`, I have finished my first slide with markdown
- much less commands to remember, e.g. to write bullet points, just begin with a dash "`-`" instead of `\begin{itemize}` and `\item`; how things can be simpler?
- I know you want math to show you are a statistician, e.g. $f(k)={n \choose k}p^{k}(1-p)^{n-k}$
- you do not need to maintain output -- only maintain a source file
- HTML5/CSS3 is much more fun than LaTeX

# A bit R code

```{r computing}
head(cars)
cor(cars)
```

Page 1 R markdown
========================================================
First, install Rstudio and r 2.14 or higher
Second, New --> R Markdown

1. click the **MD** toolbar button for help on Markdown).

2. When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r fig.width=7, fig.height=6}
plot(cars)
```
******
------
Page2 Insert figures
========================================================

![alt text](C:/Users/chengjun/Desktop/demiseOfBurst/demiseOfBurst.png)