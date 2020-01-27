#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
double RcppDist(NumericVector x, NumericVector y){
  int x_len = x.length();
  int y_len = y.length();
  
  if(x_len != y_len)
    stop("Lengths of both vectors must match");
  
  double res = 0;
  
  for(int i = 0; i < x_len; i++){
    res += pow((x[i] - y[i]), 2);
  }
  
  return pow(res, 0.5);
}

//[[Rcpp::export]]
NumericMatrix RcppDistanceTwoMatrices(NumericMatrix x, NumericMatrix y){
  int x_ncol = x.ncol();
  int y_ncol = y.ncol();
  
  if(x_ncol != y_ncol)
    stop("Number of columns in both matrices must match");
  
  int x_nrow = x.nrow();
  int y_nrow = y.nrow();
  
  NumericMatrix res(x_nrow, y_nrow);
  
  for(int i = 0; i < x_nrow; i++){
    for(int j = 0; j <= i; j++){
      res(i,j) = RcppDist(x(i, _), y(j, _));
    }
  }
  
  return(res);
}

class MyClass{
public:
  MyClass(double x_, double y_):
    x(x_), y(y_) {}
  double get_x() { return x;}
  double get_y() { return y;}
  void set_x(double value) {x = value;}
  
private:
  double x;
  double y;
};

RCPP_MODULE(mod_myclass){
  class_<MyClass>("MyClass")
  .constructor<double, double>()
  .property("x", &MyClass::get_x, &MyClass::set_x)
  .property("y", &MyClass::get_y)
  ;
}

RCPP_EXPOSED_CLASS(MyClass);

//[[Rcpp::export]]
MyClass newMyClass(double x, double y){
  return(MyClass(x, y));
}






