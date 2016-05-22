# TF-IDF

test_pre = function()
{
  sw_path = system.file("extdata", "list_files.txt", package = "TFIDF")
  documents = readLines(sw_path)

  result = list()

  for( i in 1:length(documents) )
  {
    result[i] = txt.table(documents[i])
  }

  result
}


for( i in 1:length(documents) ) { txt.table(documents[i]) }