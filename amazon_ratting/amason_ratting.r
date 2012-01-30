# Amazon ratting data analysis
# chengjun wang
# 20120121
# @ home bed
#~~~~~~~~~~~~combine 1500 files~~~~~~~~~~~~~~~~~~~~~~~~~#
func<-function(n){
  dat<-read.delim(paste("D:/Micro-blog/Amazon Rating Data/txt/", n, ".txt",sep=""), header = FALSE, sep=">",  quote="")
  dat$time <- cumsum(dat$V1=="<UserId" )
  df<-reshape(dat, idvar = "time",timevar = "V1", direction = "wide",
        varying = list(as.character(unique(dat[,1]))))[-1]

    return(df)
  }


 re<-lapply(c(31:54), func)
 re1<-as.data.frame(do.call("rbind", re));dim(re1)
 write.csv(re1, "D:/Micro-blog/Amazon Rating Data/amasonrating_data_clean31-54.csv")
 # 
#~~~~~~~~~~~~~~
func<-function(n){
  dat<-read.delim(paste("D:/Micro-blog/Amazon Rating Data/txt/", n, ".txt",sep=""), sep=">", quote="")
  # dim(dat)
  dat$time <- cumsum(dat$V1=="<UserId" )
  df <- as.data.frame(t(reshape(dat,idvar=c('V1'),timevar='time',direction='wide') ))
  colnames(df) <-c('UserId','UserName','ReviewsUrl','ProductLink','ProductName', 'Price',
                  'Rating','Title','Date', 'Review' ,'Anonymous'  )
  df<- df[-1, ]
  return(df)
  }


 re<-lapply(c(1:50), func)
 re1<-as.data.frame(do.call("rbind", re));dim(re1)
 write.csv(re1, "D:/Micro-blog/Amazon Rating Data/amasonrating_data_clean1-50.csv")
 # 
#~~~~convert-column-values-into-row-names-using-r~~~~~~~~#
var<-c("Id", "Name", "Score", "Id", "Score", "Id", "Name")
num<-c(1, "Tom", 4, 2, 7, 3, "Jim")
format1<-data.frame(var, num);format1


Id<-c(1, 2, 3)
Name<-c("Tom", NA, "Jim")
Score<-c(4, 7, NA)
format2<-data.frame(Id, Name, Score); format2

# http://stackoverflow.com/questions/9051714/convert-column-values-into-row-names-using-r

# method 1
format1$ID <- cumsum(format1$var=="Id")
reshape(format1, idvar = "ID",timevar = "var", direction = "wide",
        varying = list(as.character(unique(format1[,1]))))[-1]

list(unique(format1[,1]))
format2 <- reshape(format1, idvar="ID",timevar="var", direction="wide")[-1]
names(format2) <- gsub("num.", "", names(format2)

# method 2
format1$pk <- cumsum( format1$var=="Id" )
df <- as.data.frame(t(reshape(format1,idvar=c('var'),timevar='pk',direction='wide') ))
colnames(df) <- df[1, ]
df<- df[-1, ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## an example that isn't longitudinal data
state.x77 <- as.data.frame(state.x77)
long <- reshape(state.x77, idvar="state", ids=row.names(state.x77),
                times=names(state.x77), timevar="Characteristic",
                varying=list(names(state.x77)), direction="long")

reshape(long, direction="wide")
