pdf.vector = function(path)
{
  sw_path = system.file("extdata", "stopwords", package = "TFIDF")
  noise   = c("[\t\n\r\f\b\v]+", "([0-9])", "\\.", "\\+", "\\-", "\\/",
              "\\*", "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\,",
              "\\%", "\\&", "\\=", "\\?", "\\|", "\\!", "\\\\", "\\:",
              "\\;")

  read_function = readPDF()
  data          = read_function(elem=list(uri=path),language="en",id="id1")
  stopwords.l   = readLines(sw_path)
  stopwords.l   = strsplit(stopwords.l, split=",")
  data          = data$content
  review_text   = paste(data, collapse = " ")

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

max.table = function(data)
{
  names(which(data == max(data)))
}

min.table = function(data)
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