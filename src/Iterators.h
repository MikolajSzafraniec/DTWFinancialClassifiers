using namespace Rcpp;

/*
 * Function for generating iterators for time series subsetting.
 */

namespace Iterators{

  int CalcIteratorLength(int refSeriesLength,
                         int testSeriesLength,
                         int forecastHorizon,
                         int subsequenceBreaks){
    
    int farestStartIndex = testSeriesLength - forecastHorizon - refSeriesLength;
    int iteratorLenght = std::floor((double)farestStartIndex / subsequenceBreaks) + 1;
    
    return iteratorLenght;
  }
  
  IntegerVector CreateTSIterator(int refSeriesLength,
                                 int testSeriesLength,
                                 int forecastHorizon,
                                 int subsequenceBreaks){
    
    if(forecastHorizon < 1)
      stop("Forecast horizon must be integer greater than 0");
    
    if(subsequenceBreaks < 1)
      stop("Subsequence breaks must be integer greater than 0");
    
    int iteratorLenght = CalcIteratorLength(refSeriesLength, testSeriesLength, forecastHorizon,
                                            subsequenceBreaks);
    
    if(iteratorLenght < 1)
      stop("Wrong params, unable to generate time series iterator.");
    
    IntegerVector it(iteratorLenght);
    
    if(iteratorLenght > 1){
      for(int i = 1; i < iteratorLenght; i++){
        it[i] = it[i-1] + subsequenceBreaks;
      }
    }
    
    return it;
  }
}
