library(stringr)
library(tm)

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
      download.file(pdf.files[[counter]], destfile = paste(as.character(counter), ".pdf", sep = ""))
      counter = counter + 1
    }
  }
  setwd("../")
}

tt <- readPDF()
rr <- tt(elem=list(uri="cs.AI/10.pdf"),language="en",id="id1")
rr[1:15]
rr$content[2]
