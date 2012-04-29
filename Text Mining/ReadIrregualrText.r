# How to read irregular text using R
# Chengjun WANG
# 20120322

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# this is the function

read.irregular <- function(filenm) {
  fileID <- file(filenm,open="rt")
  nFields <- count.fields(fileID, skip = 10, blank.lines.skip = TRUE,)
  mat <- matrix(nrow=length(nFields),ncol=max(nFields))
  invisible(seek(fileID,where=0,origin="start",rw="read"))
  for(i in 1:nrow(mat) ) {
    mat[i,1:nFields[i]] <-scan(fileID,what="",nlines=1,quiet=TRUE)
  }
  close(fileID)
  df <- as.data.frame(mat)
  df[] <- lapply(df,type.convert,as.is=TRUE)
  return(df)
}

# call function with this line
df <- read.irregular("D:/FXQ(2)0709/SIG1000001.D/report.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

n=-1, encoding="UTF-8"
readLines("D:/FXQ(2)0709/SIG1000001.D/report.txt")  
con <- file("D:/FXQ(2)0709/SIG1000001.D/report.txt")
readLines(con, encoding="Latin-1") 
scan(con, sep=" ", nlines = 10)
results.list <- lapply(strsplit(readLines(con),"", encoding=""), as.integer)

read.table("D:/FXQ(2)0709/SIG1000001.D/report.txt", encoding="UTF-8")

inputFile <- "D:/FXQ(2)0709/SIG1000001.D/report.txt"
con  <- file(inputFile, open = "r")

dataList <- list()
ecdfList <- list()

while (length(oneLine <- readLines(con, n = 1, encoding="UTF-8",warn = FALSE)) > 0) {
    myVector <- (strsplit(oneLine, " "))
    myVector <- list(as.numeric(myVector[[1]]))
    dataList <- c(dataList,myVector)

    myEcdf <- ecdf(myVector[[1]])
    ecdfList <- c(ecdfList,myEcdf)

  } 

close(con)

#
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file="ex.data",
    sep="\n")
readLines("ex.data", n=-1)
unlink("ex.data") # tidy up



## test fixed-length strings
zz <- file("testchar", "wb")
x <- c("a", "this will be truncated", "abc")
nc <- c(3, 10, 3)
writeChar(x, zz, nc, eos=NULL)
writeChar(x, zz, eos="\r\n")
close(zz)

zz <- file("testchar", "rb")
readChar(zz, nc)
readChar(zz, nchar(x)+3) # need to read the terminator explicitly
close(zz)
unlink("testchar")
