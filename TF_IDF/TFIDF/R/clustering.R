# TF-IDF

test_pre = function()
{
  sw_path = system.file("extdata", "list_files.txt", package = "TFIDF")
  documents = readLines(sw_path)

  result = list()

  for( i in documents )
  {
    result[i] = txt.table(i)
  }

  result
}