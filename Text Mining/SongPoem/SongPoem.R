txt=read.csv("SongPoem.csv",colClasses="character");
# 句子用标点符号分割。
sentences=strsplit(txt$Sentence,"，|。|！|？|、");
sentences=unlist(sentences);
sentences=sentences[sentences!=""];
s.len=nchar(sentences);

# 单句太长了说明有可能是错误的字符，去除掉。
sentences=sentences[s.len<=15];
s.len=nchar(sentences);

splitwords=function(x,x.len) substring(x,1:(x.len-1),2:x.len);

words=mapply(splitwords,sentences,s.len,SIMPLIFY=TRUE,USE.NAMES=FALSE);
words=unlist(words);
words.freq=table(words);
words.freq=sort(words.freq,decreasing=TRUE);
words.freq[1:100];
