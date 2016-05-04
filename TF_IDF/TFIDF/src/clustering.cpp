#include <Rcpp.h>
#include <iostream>
#include <omp.h>

using namespace Rcpp;

double distance(NumericVector centroid, NumericVector point);
// [[Rcpp::export]]
NumericMatrix k_means(NumericMatrix centroid, NumericMatrix tf_idf)
{
  NumericMatrix clusters(centroid.nrow(), tf_idf.nrow());
  int counter[centroid.nrow()];

  for(int i = 0; i < centroid.nrow(); i++)
  {
    counter[i] = 0;
  }

  omp_set_num_threads(32);
  #pragma omp parallel for
  for(int document = 0; document < tf_idf.nrow(); document++)
  {
    NumericVector point(tf_idf.nrow());
    NumericVector point_c(tf_idf.nrow());
    NumericVector point_to_centroid(centroid.nrow());

    for(int i = 0; i < tf_idf.ncol(); i++)
    {
      point[i] = tf_idf(document, i);
    }

    for(int c = 0; c < centroid.nrow(); c++)
    {
      for(int i = 0; i < tf_idf.ncol(); i++)
      {
        point_c[i] = centroid(c, i);
      }
      point_to_centroid[c] = distance(centroid, point);
    }

    double min = 0;
    std::cout << *(std::min_element(point_to_centroid.begin(), point_to_centroid.end())) << std::endl;
    int index;

    for(int i = 0; i < centroid.nrow(); i++)
    {
      if(point_to_centroid[i] == min)
      {
        index = i;
        break;
      }
    }
    centroid(index, counter[index]++) = document;
  }
}

double distance(NumericVector centroid, NumericVector point)
{
  double sum = 0;

  for(int i = 0; i < centroid.size(); i++)
  {
    sum += (centroid[i]-point[i])*(centroid[i]-point[i]);
  }

  return sqrt(sum);
}
