#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]

// Klasa przechowująca dane dotyczące modyfikowanego szeregu i wynikowych podsekwencji
// z wbudowanymi metodami do ich uzupełniania

class SubsequenceFiller{
  
public:
  NumericMatrix *ResultMatrix;
  NumericVector *values;
  int tsLength;
  int subsequenceWidth;
  int subsequenceLength;
  
  SubsequenceFiller(NumericMatrix *RM, NumericVector *val, int tsL,
                    int subW, int subL){
    ResultMatrix = RM;
    values = val;
    tsLength = tsL;
    subsequenceWidth = subW;
    subsequenceLength = subL;
  }
  
  void ShortLeftShortRightFiller(int numIter){
    
    NumericVector proceedingRow(subsequenceLength);
    
    std::fill(proceedingRow.begin(), proceedingRow.end(), 0);
    int k = subsequenceWidth - numIter;
    NumericVector::iterator it_first = proceedingRow.begin();
    NumericVector::iterator it_last = (it_first + k);
    std::fill(it_first, it_last, (*values)(0));
    
    NumericVector::iterator it_copy_first = (*values).begin();
    NumericVector::iterator it_copy_last = (*values).end();
    std::copy(it_copy_first, it_copy_last, it_last++);
    
    NumericVector::iterator it_final_first = it_last + tsLength - 1;
    NumericVector::iterator it_final_last = proceedingRow.end();
    std::fill(it_final_first, it_final_last, (*values)(tsLength - 1));
    (*ResultMatrix)(numIter, _) = clone(proceedingRow);
  }
  
  void ShortLeftFiller(int numIter){
    
    NumericVector proceedingRow(subsequenceLength);
    
    std::fill(proceedingRow.begin(), proceedingRow.end(), 0);
    int k = subsequenceWidth - numIter;
    NumericVector::iterator it_first = proceedingRow.begin();
    NumericVector::iterator it_last = (it_first + k);
    std::fill(it_first, it_last, (*values)(0));
    int lastIndVec = subsequenceLength - k;
    NumericVector::iterator it_copy_first = (*values).begin();
    NumericVector::iterator it_copy_last = it_copy_first + lastIndVec;
    std::copy(it_copy_first, it_copy_last, it_last++);
    (*ResultMatrix)(numIter, _) = clone(proceedingRow);
  }
  
  void ShortRightFiller(int numIter){
    
    NumericVector proceedingRow(subsequenceLength);
    
    std::fill(proceedingRow.begin(), proceedingRow.end(), 0);
    int transgression = (numIter + subsequenceWidth) - tsLength + 1;
    NumericVector::iterator it_first = (proceedingRow.end() - transgression);
    NumericVector::iterator it_last = proceedingRow.end();
    std::fill(it_first, it_last, (*values)(tsLength - 1));
    int partLength = subsequenceLength - transgression;
    NumericVector::iterator it_copy_first = (*values).begin() + numIter - subsequenceWidth;
    NumericVector::iterator it_copy_last = it_copy_first + partLength;
    std::copy(it_copy_first, it_copy_last, it_first - partLength);
    (*ResultMatrix)(numIter, _) = clone(proceedingRow);
  }
  
  void OpenEndedFiller(int numIter){
    
    NumericVector proceedingRow(subsequenceLength);
    
    std::fill(proceedingRow.begin(), proceedingRow.end(), 0);
    NumericVector::iterator it_first = (*values).begin() + numIter - subsequenceWidth;
    NumericVector::iterator it_last = it_first + subsequenceLength;
    std::copy(it_first, it_last, proceedingRow.begin());
    (*ResultMatrix)(numIter, _) = clone(proceedingRow);
  }
};

//Funkcja pomocnicza do asSubsequence
//[[Rcpp::export]]
NumericMatrix subsequencesMatrix(NumericVector values, int subsequenceWidth){
  if(subsequenceWidth < 0)
    stop("Szerokosc okna musi byc liczba nieujemna");
  
  int subseqenceLength = (2*subsequenceWidth) + 1;
  int tsLength = values.length();
  NumericMatrix res(tsLength, subseqenceLength);
  
  SubsequenceFiller SubFill = SubsequenceFiller(&res, &values, tsLength,
                                                subsequenceWidth, subseqenceLength);
  
  for(int i = 0; i < tsLength; i++){
    
    if(((i - subsequenceWidth) < 0) && ((i + subsequenceWidth) >= tsLength)){
    
      SubFill.ShortLeftShortRightFiller(i);
      
    }else if((i - subsequenceWidth) < 0){
      
      SubFill.ShortLeftFiller(i);
    
    }else if((i + subsequenceWidth) >= tsLength){
    
      SubFill.ShortRightFiller(i);
     
    }else{
      SubFill.OpenEndedFiller(i);
    }  
  }
  
  return res;
  
}

//Klasa zawierająca metody przekształcające sekwencję w jej shapeDescriptor

class shapeDescriptors{
public:
  
  NumericVector RawSubsequence(NumericVector subsequence){
    return subsequence;
  } 
    
  NumericVector PAADescriptor(NumericVector subsequence, int PAAWindow){
    int subLength = subsequence.length();
    
    if(subLength < PAAWindow)
      stop("Szerokosc okna nie moze byc wieksza od dlugosci podsekwencji");
    
    int lastIndex = subLength - PAAWindow;
    NumericVector res(lastIndex + 1);
    
    for(int i = 0; i <= lastIndex; i++){
      Rcpp::Range currentRange = seq(i, i+PAAWindow-1);
      res[i] = mean(subsequence[currentRange]);
    }
    
    return res;
  }
  
  NumericVector derivativeDescriptor(NumericVector subsequence){
    int subLenght = subsequence.length();
    NumericVector res(subLenght);
    int lastInd = subLenght - 1;
    
    for(int i = 1; i < lastInd; i++){
      res[i] = discreteDerivative(subsequence, i);
    }
    
    res[0] = res[1];
    res[subLenght -1] = res[subLenght - 2];
    
    return res;
  }
  
  NumericVector slopeDescriptor(NumericVector subsequence, int slopeWindow){
    int subLength = subsequence.length();
    
    if(subLength < slopeWindow)
      stop("Szerokosc okna nie moze byc wieksza od dlugosci podsekwencji");
    
    int lastIndex = subLength - slopeWindow;
    NumericVector res(lastIndex + 1);
    NumericVector regresors(slopeWindow);
    for(int i = 1; i <= slopeWindow; i++)
      regresors[i] = i;
    
    for(int i = 0; i <= lastIndex; i++){
      Rcpp::Range currentRange = seq(i, i+slopeWindow-1);
      res[i] = rcppSlope(regresors, subsequence[currentRange]);
    }
    
    return res;
  }
  
private:
  double discreteDerivative(NumericVector subsequence, int i){
    double res = ((subsequence[i] - subsequence[i-1]) + ((subsequence[i+1] - subsequence[i-1])/2))/2;
    return res;
  }
  
  double rcppSlope(NumericVector x, NumericVector y){
    int x_len = x.length();
    int y_len = y.length();
    
    if(x_len != y_len)
      stop("Blad: wektory o roznej dlugosci");
    
    double x_mean = mean(x);
    double y_mean = mean(y);
    
    double covariance = 0;
    double x_variance = 0;
    
    for(int i = 0; i < x_len; i++){
      covariance += (x[i] - x_mean)*(y[i] - y_mean);
      x_variance += pow((x[i] - x_mean), 2);
    }
    
    double res = covariance / x_variance;
    return res;
  }
  
  shapeDescriptors() {}
};

