# Reposting in R
# by Yibo Chen
# 20120313
# please refer to:http://chen.yi.bo.blog.163.com/blog/static/150621109201221222853282/


#貌似新浪已经到V1.3.19了，所以我这个登录过程有可能又快不行了。。。
#例子是 @求是设计会 的一条微博 http://weibo.com/1782871497/y2Z1WtI7D

# 首先还是微博登录的函数：
f_weibo_login <- function(name="wangchj04", pwd="chengwang6"){
memory.limit(4000)
library(RCurl)
library(digest)

# 对ID的预处理
name <- gsub('@', '%40', name)
name <- base64(name)[1]

# 常规的打包，具体没仔细研究
myH <- c("Host"="login.sina.com.cn",
"User-Agent"="Mozilla/5.0 (Windows NT 5.1; rv:2.0.1) Gecko/20100101 Firefox/4.0.1",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
"Accept-Language"="zh-cn,zh;q=0.5",
"Accept-Encoding"="gzip, deflate",
"Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7",
"Keep-Alive"="115",
"Connection"="keep-alive",
"Referer"="http://weibo.com/",
"Content-Type"="application/x-www-form-urlencoded; charset=UTF-8")
d <- debugGatherer()
cH <- getCurlHandle(debugfunction=d$update, verbose=T,
ssl.verifyhost=F, ssl.verifypeer=F, followlocation=T, cookiefile="cc.txt")

# 预登录的页面。这里貌似应该用一些正则匹配的，也没有仔细研究
preurl <- paste("http://login.sina.com.cn/sso/prelogin.php?entry=miniblog&callback=sinaSSOController.preloginCallBack&su=", 
name, "&client=ssologin.js(v1.3.18)", sep='')
prelogin <- readLines(preurl, warn=F)
servertime <- strsplit(prelogin, '\"servertime\":')[[1]][2]
servertime <- strsplit(servertime, ',\"pcid\"')[[1]][1]
pcid <- strsplit(prelogin, '\"pcid\":\"')[[1]][2]
pcid <- strsplit(pcid, '\",\"nonce\"')[[1]][1]
nonce <- strsplit(prelogin, '\"nonce\":\"')[[1]][2]
nonce <- strsplit(nonce, '\"}')[[1]][1]
servertime
pcid
nonce
# 加密的过程
pwd1 <- digest(pwd, algo='sha1', seria=F)
pwd2 <- digest(pwd1, algo='sha1', seria=F)
pwd3 <- digest(paste(pwd2, servertime, nonce, sep=''), algo='sha1', seria=F)
getCurlInfo(cH)[["cookielist"]]
pinfo=c(
"service"="miniblog",
"client"="ssologin.js(v1.3.18)",
"entry"="weibo",
"encoding"="UTF-8",
"gateway"="1",
"savestate"="7",
"from"="",
"useticket"="1",
"su"=name,
"servertime"=servertime,
"nonce"=nonce,
"pwencode"="wsse",
"sp"=pwd3,
"vsnf"="1",
"vsnval"="",
"pcid"=pcid,
"url"="http://weibo.com/ajaxlogin.php?framelogin=1&callback=parent.sinaSSOController.feedBackUrlCallBack",
"returntype"="META",
"ssosimplelogin"="1",
"setdomain"="1"
)
# 登录
ttt <- postForm("http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.3.18)", 
httpheader=myH, .params=pinfo, curl=cH, style="post")
getCurlInfo(cH)[["cookielist"]]

newurl <- strsplit(ttt[1], 'location.replace[(]\'')[[1]][2]
newurl <- strsplit(newurl, '\');')[[1]][1]
newurl
getURL(newurl, curl=cH, .encoding="gbk")
getCurlInfo(cH)[["cookielist"]]
return(cH)
}


# 原微博的全部转发
f_weibo_path_root <- function(cH=ch0, rooturl='http://weibo.com/1726276573/y8v9vob0V'){
library(RCurl)
library(rjson)
memory.limit(4000)

dataframe <- data.frame(rootmid=NULL,rootname=NULL,rootuid=NULL,
reposturl=NULL,repostmid=NULL,repostname=NULL,repostuid=NULL)

# 先看看有多少转发
the1url <- paste(rooturl, '?type=repost&page=1', sep='')
the1get <- getURL(the1url, curl=cH, .encoding="gbk")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

infoi <- grep('\"pid\":\"pl_content_weiboDetail\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1)
a1 <- fromJSON(a1)$html
write(a1, 'a1.txt')
a1 <- readLines("a1.txt")[-1]

pagesid <- grep('node-type=\"forward_counter\">转发\\(', a1)

if(length(pagesid)>0){

pages <- 1
pagesid2 <- grep('下一页', a1)
if(length(pagesid2)>0){
c1 <- a1[max(pagesid2)-2]
c1 <- strsplit(c1, 'feed_list_page\">')[[1]][2]
c1 <- as.numeric(strsplit(c1, '</')[[1]][1])
pages <- c1
}

for(pagei in 1:pages){

the1url <- paste(rooturl, '?type=repost&page=', pagei, sep='')
the1get <- getURL(the1url, curl=cH, .encoding="gbk")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

infoi <- grep('\"pid\":\"pl_content_weiboDetail\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1)
a1 <- fromJSON(a1)$html
write(a1, 'a1.txt')
a1 <- readLines("a1.txt")[-1]

b1 <- a1[grep('&rooturl=', a1)]

# 被转发微博的微博ID
rootmid1 <- unlist(strsplit(b1, '&rootmid='))[seq(2,length(b1)*2,2)]
names(rootmid1) <- NULL
rootmid2 <- unlist(strsplit(rootmid1, '&rootname='))[seq(1,length(rootmid1)*2,2)]
names(rootmid2) <- NULL

# 被转发对象的name
rootname1 <- unlist(strsplit(b1, '&rootname='))[seq(2,length(b1)*2,2)]
names(rootname1) <- NULL
rootname2 <- unlist(strsplit(rootname1, '&rootuid='))[seq(1,length(rootname1)*2,2)]
names(rootname2) <- NULL

# 被转发对象的id
rootuid1 <- unlist(strsplit(b1, '&rootuid='))[seq(2,length(b1)*2,2)]
names(rootuid1) <- NULL
rootuid2 <- unlist(strsplit(rootuid1, '&rooturl='))[seq(1,length(rootuid1)*2,2)]
names(rootuid2) <- NULL

# 转发微博的url
url1 <- unlist(strsplit(b1, '&url='))[seq(2,length(b1)*2,2)]
names(url1) <- NULL
url2 <- unlist(strsplit(url1, '&mid='))[seq(1,length(url1)*2,2)]
names(url2) <- NULL

# 转发微博的微博ID
mid1 <- unlist(strsplit(b1, '&mid='))[seq(2,length(b1)*2,2)]
names(mid1) <- NULL
mid2 <- unlist(strsplit(mid1, '&name='))[seq(1,length(mid1)*2,2)]
names(mid2) <- NULL

# 转发者的name
name1 <- unlist(strsplit(b1, '&name='))[seq(2,length(b1)*2,2)]
names(name1) <- NULL
name2 <- unlist(strsplit(name1, '&uid='))[seq(1,length(name1)*2,2)]
names(name2) <- NULL

# 转发者的id
uid1 <- unlist(strsplit(b1, '&uid='))[seq(2,length(b1)*2,2)]
names(uid1) <- NULL
uid2 <- unlist(strsplit(uid1, '&domain='))[seq(1,length(uid1)*2,2)]
names(uid2) <- NULL

if(length(unique(length(rootmid2),length(rootname2),length(rootuid2),length(mid2),length(name2),length(uid2),length(url2)))==1){
data_new <- data.frame(rootmid=rootmid2,rootname=rootname2,rootuid=rootuid2,
repostmid=mid2,repostname=name2,repostuid=uid2,reposturl=url2)
}
else{
data_new <- dataframe
print(c(pagei, 'wrong'))
}
dataframe <- unique(rbind(dataframe, data_new))
gc()
}
}
return(dataframe)
}


# 转发带来的转发
f_weibo_path_repost <- function(cH=ch0, rooturl=dataframe1$reposturl[6], 
rootuid=dataframe1$repostuid[6], rootname=dataframe1$repostname[6], rootmid=dataframe1$repostmid[6]){
library(RCurl)
library(rjson)
memory.limit(4000)

dataframe <- data.frame(rootmid=NULL,rootname=NULL,rootuid=NULL,
reposturl=NULL,repostmid=NULL,repostname=NULL,repostuid=NULL)

# 先看看有多少转发
the1url <- paste(rooturl, '?type=repost&page=1', sep='')
the1get <- getURL(the1url, curl=cH, .encoding="gbk")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

infoi <- grep('\"pid\":\"pl_content_weiboDetail\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1)

if(length(a1) > 0){
a1 <- fromJSON(a1)$html
write(a1, 'a1.txt')
a1 <- readLines("a1.txt")[-1]

pagesid <- grep('node-type=\"forward_counter\">转发\\(', a1)

if(length(pagesid)>0){

pages <- 1
pagesid2 <- grep('下一页', a1)
if(length(pagesid2)>0){
c1 <- a1[max(pagesid2)-2]
c1 <- strsplit(c1, 'feed_list_page\">')[[1]][2]
c1 <- as.numeric(strsplit(c1, '</')[[1]][1])
pages <- c1
}

for(pagei in 1:pages){

the1url <- paste(rooturl, '?type=repost&page=', pagei, sep='')
the1get <- getURL(the1url, curl=cH, .encoding="gbk")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

infoi <- grep('\"pid\":\"pl_content_weiboDetail\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1)
a1 <- fromJSON(a1)$html
write(a1, 'a1.txt')
a1 <- readLines("a1.txt")[-1]

repis <- grep('&rooturl=', a1)

if(length(repis)>0){
b1 <- a1[repis]

# 转发微博的url
url1 <- unlist(strsplit(b1, '&url='))[seq(2,length(b1)*2,2)]
names(url1) <- NULL
url2 <- unlist(strsplit(url1, '&mid='))[seq(1,length(url1)*2,2)]
names(url2) <- NULL

# 转发微博的微博ID
mid1 <- unlist(strsplit(b1, '&mid='))[seq(2,length(b1)*2,2)]
names(mid1) <- NULL
mid2 <- unlist(strsplit(mid1, '&name='))[seq(1,length(mid1)*2,2)]
names(mid2) <- NULL

# 转发者的name
name1 <- unlist(strsplit(b1, '&name='))[seq(2,length(b1)*2,2)]
names(name1) <- NULL
name2 <- unlist(strsplit(name1, '&uid='))[seq(1,length(name1)*2,2)]
names(name2) <- NULL

# 转发者的id
uid1 <- unlist(strsplit(b1, '&uid='))[seq(2,length(b1)*2,2)]
names(uid1) <- NULL
uid2 <- unlist(strsplit(uid1, '&domain='))[seq(1,length(uid1)*2,2)]
names(uid2) <- NULL

if(length(unique(length(mid2),length(name2),length(uid2),length(url2)))==1){
data_new <- data.frame(rootmid=rootmid,rootname=rootname,rootuid=rootuid,
repostmid=mid2,repostname=name2,repostuid=uid2,reposturl=url2)
}
else{
data_new <- dataframe
print(c(pagei, 'wrong'))
}
dataframe <- unique(rbind(dataframe, data_new))
gc()
}
}
}
}
return(dataframe)
}


# 把上面仨函数拼在一起
f_weibo_path <- function(name0="****@****", pwd0="********",
rooturl0='http://weibo.com/1726276573/y8v9vob0V'){
ch0 <- f_weibo_login(name0, pwd0)
dataframe1 <- f_weibo_path_root(cH=ch0, rooturl=rooturl0)
dataframe2 <- dataframe1
print(nrow(dataframe2))
for(rowi in 1:nrow(dataframe1)){
data_new <- f_weibo_path_repost(cH=ch0, rooturl=dataframe1$reposturl[rowi], 
rootuid=dataframe1$repostuid[rowi], rootname=dataframe1$repostname[rowi], rootmid=dataframe1$repostmid[rowi])
data_old <- dataframe2[!(dataframe2$repostmid %in% data_new$repostmid), ]
dataframe2 <- unique(rbind(data_old, data_new))
print(c(rowi,nrow(data_new),date()))
gc()
}
rm(rowi)
dataframe6 <- unique(dataframe2[c('rootname','rootmid','repostname','repostmid')])
return(dataframe6)
}


# 画图
f_weibo_path_plot <- function(datainput=mydataframe){
library(igraph)

for(i in 1:ncol(datainput)) datainput[,i] <- as.character(datainput[,i])
rm(i)
people <- unique(data.frame(id=c(datainput$rootmid,datainput$repostmid), name=c(datainput$rootname,datainput$repostname)))
gg <- graph.data.frame(d=datainput[c('rootmid','repostmid')], directed=T, vertices=people)
is.simple(gg)
gg2 <- simplify(gg, remove.loops=T, remove.multiple=F)
is.simple(gg2)

# 图形的参数，这个需要设计一下  ="=
V(gg2)$degree <- degree(gg2, mode='out')
V(gg2)$betweenness <- betweenness(gg2)
top_d <- quantile(V(gg2)$degree, (length(V(gg2))-5)/length(V(gg2)))
top_b <- quantile(V(gg2)$betweenness, (length(V(gg2))-5)/length(V(gg2)))
V(gg2)$size <- 1
V(gg2)$label <- NA
V(gg2)$labelcex <- 2
V(gg2)$framecolor <- 'SkyBlue2'
V(gg2)$vertexcolor <- 'SkyBlue2'
V(gg2)[degree>=top_d | betweenness>=top_b]$framecolor <- 'gold'
V(gg2)[degree>=top_d | betweenness>=top_b]$vertexcolor <- 'gold'
V(gg2)[degree>=top_d | betweenness>=top_b]$size <- 6
V(gg2)[degree>=top_d | betweenness>=top_b]$label <- V(gg2)[degree>=top_d | betweenness>=top_b]$name
V(gg2)[degree>=top_d | betweenness>=top_b]$color <- 'gold'

png(gsub('[: ]', '_', paste('weibo_path_', format(Sys.time()), '.png', sep='')),width=800,height=800)
par(mar=c(0,0,0,0))
set.seed(14)
plot(gg2,
layout=layout.fruchterman.reingold,
vertex.size=V(gg2)$size,
vertex.label=V(gg2)$label,
vertex.label.cex=V(gg2)$labelcex,
vertex.color=V(gg2)$vertexcolor,
vertex.frame.color=V(gg2)$framecolor,
edge.color=grey(0.5),
edge.arrow.size=0,
edge.arrow.width=0
)
dev.off()
}






# 一个例子
mydataframe <- f_weibo_path(name0="wangchj04", pwd0="chengwang6",
rooturl0='http://weibo.com/1726276573/y8v9vob0V')

dim(mydataframe)

save.image(gsub('[: ]', '_', paste('weibo_path_', format(Sys.time()), '.RData', sep='')))

f_weibo_path_plot(mydataframe)

