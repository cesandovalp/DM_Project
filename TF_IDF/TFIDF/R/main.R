main = function()
{
  document = c("/home/cristiansp/Documents/MDA/DM_Project/TF_IDF/Test/1.pdf",
               "/home/cristiansp/Documents/MDA/DM_Project/TF_IDF/Test/2.pdf",
               "/home/cristiansp/Documents/MDA/DM_Project/TF_IDF/Test/3.pdf")

  result = list()

  term = c()

  for(d in 1:length(document))
  {
    result[[d]] = pdf.vector(document[d])
    term        = join.aux(term, result[[d]])
  }

  for(d in result)
  {
    for(t in term)
    {
      print(d)
    }
  }

  term
}
