#setwd("~/Documents/MDA/DM_Project/")
library(stringr)

download.file("http://arxiv.org/corr/home", destfile = "home.html")
home.html = readLines("home.html")
p1 = regexpr('http.*arxiv.*cs.*recent', home.html, TRUE)
p2 = p1 + attr(p1, "match.length") - 1
str_match = substr(home.html, p1, p2)

categories =list()
counter = 0

for(i in 1:length(str_match))
{
  if(str_match[i] != "")
  {
    categories[counter] = str_match[[i]]
    counter = counter + 1
  }
}

#for(i in 1:length(categories))
#{
#  print(categories[[i]])
#}

p1 = regexpr('cs...', categories, TRUE)
p2 = p1 + attr(p1, "match.length") - 1
categories.names = substr(categories, p1, p2)

for(i in 1:length(categories))
{
  categories.names[i] = str_replace_all(string=paste("./", categories.names[i]), pattern=" ", repl="")
  dir.create(categories.names[i])
  setwd(categories.names[i])
  #print(getwd())
  download.file(categories[[i]], destfile = "file.html")
  setwd("../")
}
