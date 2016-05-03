#include <iostream>
#include <omp.h>

// [[Rcpp::plugins(openmp)]]
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix k_means(NumericMatrix clusters, NumericMatrix tf_idf)
{
  omp_set_num_threads(32);
  #pragma omp parallel for
  for(int document = 0; document < tf_idf.nrows(); document++)
  {
    ;
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