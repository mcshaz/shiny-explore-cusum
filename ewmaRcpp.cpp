#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector ewmaRcpp(Rcpp::NumericVector x, double lambda){
  int n = x.length();
  Rcpp::NumericVector s(n);
  if (n > 0) {
    s[0] = x[0];
    for (int j = 1; j < n; ++j) {
      s[j] = lambda * x[j] + (1.0 - lambda) * s[j-1];
    }
  }
  return s;
}

inline double loglikelihood(double p, bool outcome, double odds) {
  return (outcome ? std::log(odds) : 0.0) - std::log(1 + p * (odds - 1));
}

// [[Rcpp::export]]
Rcpp::DataFrame cusumRcpp(Rcpp::NumericVector risks, Rcpp::IntegerVector outcomes, double odds, double h){
  int n = risks.length();
  Rcpp::NumericVector us(n);
  Rcpp::NumericVector uf(n);
  Rcpp::NumericVector ls(n);
  Rcpp::NumericVector lf(n);
  if (n > 0) {
    double w = loglikelihood(risks[0], outcomes[0] > 0, odds);
    us[0] = 0.0;
    uf[0] = w > 0 ? w : 0.0 ;
    ls[0] = 0.0;
    lf[0] = w < 0 ? w : 0.0;
    for (int j = 1; j < n; ++j) {
      w = loglikelihood(risks[j], outcomes[j] > 0, odds);
      
      us[j] = uf[j-1] >= h ? 0.0 : uf[j-1];
      uf[j] = std::max(0.0, us[j] + w);
      ls[j] = lf[j-1] <= -h ? 0.0 : lf[j-1];
      lf[j] = std::min(0.0, ls[j] + w);
    }
  }
  return Rcpp::DataFrame::create(
    Rcpp::Named("sequence") = Rcpp::seq(1, n),
    Rcpp::Named("doubling.start") = us,
    Rcpp::Named("doubling.finish") = uf,
    Rcpp::Named("halving.start") = ls,
    Rcpp::Named("halving.finish") = lf
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector ewmaSERcpp(Rcpp::NumericVector vars, double lambda) {
  double lc2 = (1.0 - lambda) * (1.0 - lambda);
  int n = vars.length();
  Rcpp::NumericVector s(n);
  if (n > 0) {
    s[0] = vars[0];
    for (int j = 1; j < n; ++j) {
      s[j] = s[j - 1] * lc2 + vars[j];
    }
    for (int j = 0; j < n; ++j) {
      s[j] = lambda * std::sqrt(s[j]);
    }
  }
  return s;
}
