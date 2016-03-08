library(stringr)
library(tm)
library(wordcloud)
library(proxy)

setwd("~/Documents/MDA/DM_Project/")

download.file("http://arxiv.org/corr/home", destfile = "home.html")
home.html = readLines("home.html")
p1 = regexpr('http.*arxiv.*cs.*recent', home.html, TRUE)
p2 = p1 + attr(p1, "match.length") - 7
str_match = substr(home.html, p1, p2)

categories = list()
counter = 1

for(i in 1:length(str_match))
{
  if(str_match[i] != "")
  {
    categories[counter] = str_match[[i]]
    counter = counter + 1
  }
}

p1 = regexpr('cs...', categories, TRUE)
p2 = p1 + attr(p1, "match.length") - 1
categories.names = substr(categories, p1, p2)

counter = 1
pdf.files = list()
pdf.index = list()

for(i in 1:length(categories))
{
  categories.names[i] = paste("./", categories.names[i], sep="")
  dir.create(categories.names[i])
  setwd(categories.names[i])
  download.file(paste(categories[[i]], "1506", "?show=20", sep=""), destfile = "file.html")
  file.html = readLines("file.html")
  p1 = regexpr('.pdf/[0-9]*.[0-9]*', file.html, TRUE, )
  p2 = p1 + attr(p1, "match.length") - 1
  str_match2 = substr(file.html, p1, p2)
  
  for(j in 1:length(str_match2))
  {
    if(str_match2[j] != "")
    {
      pdf.files[counter] = paste("http://arxiv.org", str_match2[[j]], sep="")
      pdf.index[counter] = paste(getwd(), "/", as.character(counter), ".pdf", sep = "")
      pdf.index[100:150]
      download.file(pdf.files[[counter]], destfile = paste(as.character(counter), ".pdf", sep = ""))
      counter = counter + 1
    }
  }
  setwd("../")
}

tt <- readPDF()
rr = list()
rr2 = list()

for(i in 1:length(pdf.index))
{
  rr[i] <- tt(elem=list(uri=pdf.index[[i]]),language="en",id="id1")
}

for(i in 1:length(pdf.index))
{
  rr2[i] = rr[[i]]
  review_text = paste(rr[[i]], collapse = " ")
  review_text_vector = unlist(strsplit(review_text, split=" "))
  index.bad = grep("review_text_vector", iconv(review_text_vector, "latin1", "ASCII", sub="review_text_vector"))
  review_subset = review_text_vector[-index.bad]
  rr2[i] = paste(review_subset, collapse = " ")
}

review_src = VectorSource(rr2)
corpus = Corpus(review_src)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stemDocument)

#matriz_dt = DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
matriz_dt2 = DocumentTermMatrix(corpus)

dtm2 = as.matrix(matriz_dt)
dtm22 = as.matrix(matriz_dt2)
freq = colSums(dtm2)
freq2 = colSums(dtm22)
freq = sort(freq, decreasing = TRUE)
freq2 = sort(freq2, decreasing = TRUE)
head(freq)
head(freq2)
barplot(freq)
barplot(freq2)

words = names(freq)
wordcloud(words[1:100], freq[1:100])

result.cosine = proxy::dist(dtm2, method = "cosine")
result.cosine[1:length(result.cosine)]
