pdf.vector = function(path)
{
  sw_path = system.file("extdata", "stopwords", package = "TFIDF")
  noise   = c("[\t\n\r\f\b\v]+", "([0-9])", "\\.", "\\+", "\\-", "\\/",
              "\\*", "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\,",
              "\\%", "\\&", "\\=", "\\?", "\\|", "\\!", "\\\\", "\\:",
              "\\;", "<", ">")

  read_function = readPDF()
  data          = read_function(elem=list(uri=path),language="en",id="id1")
  stopwords.l   = readLines(sw_path)
  stopwords.l   = strsplit(stopwords.l, split=",")
  data          = data$content
  review_text   = paste(data, collapse = " ")
  str_replace_all(review_text, "[^[:alnum:]]", " ")

  for( i in noise ) { review_text = gsub(i, " ", review_text) }

  for( i in 1:20 )  { review_text = gsub("  ", " ", review_text) }

  for( i in stopwords.l[[1]] )
  {
    review_text = gsub(paste(c('\\<',i,'\\>'), collapse=""), "", review_text, ignore.case = TRUE)
  }

  for( i in 1:10 ) { review_text = gsub("  ", " ", review_text) }

  review_text_vector = unlist(strsplit(review_text, split=" "))
  index.bad          = grep("review_text_vector", iconv(review_text_vector, "latin1", "ASCII", sub="review_text_vector"))
  review_subset      = review_text_vector[-index.bad]
  result             = tolower(paste(review_subset, collapse = " "))
  result             = strsplit(result, split=" ")
  frequency_table    = table(result)

  frequency_table
}

max_table = function(data)
{
  names(which(data == max(data)))
}

min_table = function(data)
{
  names(which(data == min(data)))
}

terms.table = function(data)
{
  length(names(data))
}

join.terms = function(a, b)
{
  union(names(a), names(b))
}

join.aux = function(a, b)
{
  union(a, names(b))
}

generate_directories = function()
{
  setwd("/home/cesandovalp/Documents/MDA/DM_Project")
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
    setwd(categories.names[i])
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
        counter = counter + 1
      }
    }
    setwd("../")
  }

  pdf.index
}
