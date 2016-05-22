# TF-IDF
sw_path = system.file("extdata", "list_files.txt", package = "TFIDF")
documents = readLines(sw_path)

for( i in documents )
{
  txt.table(i)
}