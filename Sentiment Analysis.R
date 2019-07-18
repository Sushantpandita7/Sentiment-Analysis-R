library(twitteR)

consumerKey  <- "aEQi6kUDfmJnAPOGUVfGA2Lz8"#API Key
consumerSecret <- "HdMbdBtoa8Fh1Y6IVhsvv38nojI10L2gWFI1ZSaCPJNohAgb72"#API Secret
accessToken <- "885776603174666240-zjJh9vq6YIkCbCQXL09erzBeRWlm7gu"
accessTokenSecret <- "t2nLX46zG0ZZFrsgZGDaea3k5CMGxeMEMM5SGlNfchaRZ"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
tweets <- searchTwitter("#Sanju", n=1000, lang="en",
                        geocode = "28.459497,77.026638,500mi",
                        since = "2018-01-03") 

t1= twListToDF(tweets)
t2 = t1[,"text"]

clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  some_txt = gsub("[[:cntrl:]]","", some_txt) 
  
  some_txt = sapply(some_txt,tolower)
  names(some_txt) = NULL
  some_txt <- strsplit(some_txt," ")
  return(some_txt)
}

tweet_clean = clean.text(t2)
getwd()
setwd("C:/Users/Sushant/Desktop")
positive=scan("positive-words.txt",what='character')
negative=scan("negative-words.txt",what='character')

tweet_clean = unlist(tweet_clean)

pos.match=match(tweet_clean, positive)
pos.match=!is.na(pos.match)
pos.score=sum(pos.match)                              #number of positive matches

neg.match=match(tweet_clean, negative)
neg.match=!is.na(neg.match)
neg.score=sum(neg.match)                              #number of negative matches

pmatch = match(tweet_clean, positive)
pmatch = na.omit(pmatch)
posw = positive[pmatch]                               #positive words that matched

nmatch = match(tweet_clean, negative)
nmatch = na.omit(nmatch)
negw = positive[nmatch]                               #negative words that matched
                              

#plot prep
pdata = data.frame(NULL)
ndata = data.frame(NULL)

pdata = c(pdata, posw)
ndata = c(ndata, negw)

pdata = unlist(pdata)
ndata = unlist(ndata)

tpdata = table(pdata)
tndata = table(ndata)

p = data.frame(tpdata)
n = data.frame(tndata)

library(ggplot2)

x11(20,20)

ggplot(p, aes(x=pdata,y=Freq)) +
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90))

ggplot(n, aes(x=ndata,y=Freq)) +
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90))
