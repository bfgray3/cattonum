#include <Rcpp.h>

// [[Rcpp::export]]
double mean_cattonum(const Rcpp::NumericVector &x) {

  int nonmissing_n = 0, tot_n = x.size();
  double s = NA_REAL;

  for (int i = 0; i < tot_n; ++i) {

    if (Rcpp::NumericVector::is_na(x[i]))
      continue;

    ++nonmissing_n;

    if (!Rcpp::NumericVector::is_na(s))
      s += x[i];
    else
      s = x[i];
  }

  return s / nonmissing_n;
}
