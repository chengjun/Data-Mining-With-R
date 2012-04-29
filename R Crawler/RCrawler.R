# R code for data crawling on weibo
# 20120311
# author: Bo CHEN
# http://chen.yi.bo.blog.163.com/blog/static/150621109201221055135686/

# 首先还是微博登录的函数。
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



# 然后是抓取数据的函数。目前只写了feeds部分的抓取，其他是类似的，而且会更简单一点，不需要刷新页面。
f_weibo_get <- function(cH=ch0, N=200, hisnick='frankwang'){
# 参数N是想要获取的微博条数。参数hisnick是对方的ID
library(rjson)
memory.limit(4000)

# 先看一下有多少页
pg=1
the1url <- paste('http://weibo.com/', hisnick, '/profile?page=', pg, sep='')
the1get <- getURL(the1url, curl=cH, .encoding="gbk")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

idi <- grep('\\[\'oid\'\\]', the1get)
oid <- strsplit(the1get[idi], '\\[\'oid\'\\] = \'')[[1]][2]
oid <- strsplit(oid, '\';')[[1]][1]
idi <- grep('\\[\'uid\'\\]', the1get)
uid <- strsplit(the1get[idi], '\\[\'uid\'\\] = \'')[[1]][2]
uid <- strsplit(uid, '\';')[[1]][1]

# 微博信息
infoi <- grep('\"pid\":\"pl_content_litePersonInfo\"', the1get)
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[infoi])
a1 <- gsub('\\)</script>','',a1)
a1 <- fromJSON(a1)$html
write(a1, 'a1.txt')
a1 <- readLines("a1.txt")
numberi <- max(grep('node-type=\"weibo\">', a1))
number <- strsplit(a1[numberi], 'node-type=\"weibo\">')[[1]][2]
number <- strsplit(number, '</strong>')[[1]][1]
pages <- ceiling(min(as.numeric(number), N)/45)

weibo_data <- c()

# 循环读取页面
for (pg in 1:pages){

# 第一屏
the1url <- paste('http://weibo.com/', hisnick, '/profile?page=', pg, sep='')
the1get <- getURL(the1url, curl=cH, .encoding="gbk")
write(the1get, "temp.txt")
the1get <- readLines("temp.txt")

# 看别人的时候是hisFeed，看自己的时候是myFeed(后面的url也略有差异，主要是刷新的时候需要用到uid)
if(uid == oid){
myfeedi <- grep('\"pid\":\"pl_content_myFeed\"', the1get)
}
if(uid != oid){
myfeedi <- grep('\"pid\":\"pl_content_hisFeed\"', the1get)
}
a1 <- gsub('<script>STK && STK.pageletM && STK.pageletM.view\\(','',the1get[myfeedi])
a1 <- gsub('\\)</script>','',a1)
a1 <- fromJSON(a1)$html
write(a1, 'a1.txt')
a1 <- readLines("a1.txt")

# 最后一条微博的ID
lastmidi <- max(grep('mid=\"', a1))
lastmid <- strsplit(a1[lastmidi], 'mid=\"')[[1]][2]
lastmid <- strsplit(lastmid, '\"')[[1]][1]

# 于是第二屏
the2url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
'&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=0&uid=', oid, sep='')
the2get <- getURL(the2url, curl=cH, .encoding="gbk")
write(the2get, "temp.txt")
the2get <- readLines("temp.txt")
a2 <- fromJSON(the2get)$data
write(a2, 'a2.txt')
a2 <- readLines("a2.txt")

# 最后一条微博的ID
lastmidi <- max(grep('mid=\"', a2))
lastmid <- strsplit(a2[lastmidi], 'mid=\"')[[1]][2]
lastmid <- strsplit(lastmid, '\"')[[1]][1]

# 于是第三屏
the3url <- paste('http://weibo.com/aj/mblog/mbloglist?page=', pg, 
'&count=15&max_id=', lastmid, '&pre_page=', pg, '&end_id=&pagebar=1&uid=', oid, sep='')
the3get <- getURL(the3url, curl=cH, .encoding="gbk")
write(the3get, "temp.txt")
the3get <- readLines("temp.txt")
a3 <- fromJSON(the3get)$data
write(a3, 'a3.txt')
a3 <- readLines("a3.txt")

# 筛选微博正文内容，连接起来
a123 <- c(a1, a2, a3)
index <- grep('node-type=\"feed_list_content\"', a123)
a11 <- a123[index]
b <- gregexpr('>[^<>]*<', a11)
getcontent <- function(string, greg){
paste(substring(string, greg+1, greg+attr(greg,'match.length')-2), collapse='  ')
}
a111 <- mapply(getcontent, a11, b)
names(a111) <- NULL
weibo_data <- c(weibo_data, a111)
gc()
}

# 去掉英文和数字，去掉@对象
weibo_data <- gsub('__@.*__', '', weibo_data)
weibo_data <- gsub('[0-9a-zA-Z]+', '', weibo_data)

return(weibo_data[1:min(as.numeric(number), N)])
}



# 登录
ch0 <- f_weibo_login('我的账号', '我的密码')
ch1 <- f_weibo_login('马甲的账号', '马甲的密码')
# 获取微博数据（这里只做了我自己的版本，10000是个足够大的数字）
weibo_10000_0 <- f_weibo_get(cH=ch0, N=10000, hisnick='chenyibo')
weibo_10000_1 <- f_weibo_get(cH=ch1, N=10000, hisnick='chenyibo')
# 这两个结果有一点点点差异，目前看来，貌似是显示给自己的微博比较全。
all(weibo_10000_0 %in% weibo_10000_1)
# FALSE
all(weibo_10000_1 %in% weibo_10000_0)
# TRUE


# 亮哥@许亮_在路上指导我可以用个人词频与公共词频做比较，来筛选关键词。所以我又做了生成词云的函数。
f_weibo_wordcloud <- function(weibo_data=weibo_10000_0, hisnick='chenyibo'){

# 分词
library(rsmartcn)
f_cut <- function(x){
unlist(strsplit(smartcn(x), ' '))
}
words <- unlist(mapply(f_cut, weibo_data))
words <- words[words != 'na']
words <- words[words != '转发']

# 统计词频
words_freq <- sort(table(words), dec=T)
words_names <- names(words_freq)
words_length <- nchar(words_names)

# 加载搜狗实验室的词频文件   http://www.sogou.com/labs/dl/w.html
SogouLabDic <- read.table('SogouLabDic.dic', fill=T, head=F)

words_df <- data.frame(words_names=words_names, words_freq=words_freq, words_length=words_length)
# 只做两个字的词，简单一点。。。
words_df <- words_df[words_df$words_length == 2, ]
names(SogouLabDic)[1] <- 'words_names'
SogouLabDic <- SogouLabDic[SogouLabDic[,1] %in% words_df$words_names, ]

words_df2 <- merge (words_df, SogouLabDic, by='words_names', all.x=T)
# words_df2 <- words_df2[grep('^[NV],',words_df2$V3), ]
# 可以筛选名词和动词。不过似乎没有必要，因为形容词副词什么的也能够体现用词风格嘛
words_df2 <- words_df2[order(-words_df2[,2]), ]

words_df3 <- words_df2[is.na(words_df2$V2), ]
words_df4 <- words_df2[!is.na(words_df2$V2), ]

# 匹配不到的，用原来的词频。匹配到的，就看是否超过平均水平
words_df4$words_freq2 <- words_df4$words_freq/words_df4$V2
words_df4 <- words_df4[words_df4$words_freq2 > mean(words_df4$words_freq2), 1:2]

words_df5 <- rbind(words_df4, words_df3[, 1:2])

# 做词云（这个包貌似对中文支持不是很好）
library(wordcloud)
png(paste('weibo_wordcloud_', hisnick, '.png', sep=''),width=500,height=500)
par(mar=c(0,0,0,0))
wordcloud(words_df5$words_names, words_df5$words_freq, min.freq=2, 
scale=c(9,1), max.words=50, random.order=F, colors=terrain.colors(50,1))
dev.off()
}

f_weibo_wordcloud(weibo_10000_0, hisnick='chenyibo')