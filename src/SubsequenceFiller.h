using namespace Rcpp;

/*
 * Class which contains pointers to data and parameteres of modyfing series and
 * expected results. This class includes also methods which handle with filling
 * subsequences with proper values.
 */

namespace SFiller
{
  class SubsequenceFiller{
    
  public:
    NumericMatrix *ResultMatrix;
    NumericVector *values;
    int tsLength;
    int subsequenceWidth;
    int subsequenceLength;
    
    SubsequenceFiller(NumericMatrix *RM, NumericVector *val, int tsL,
                      int subW, int subL)
      : ResultMatrix(RM), values(val), tsLength(tsL), subsequenceWidth(subW), subsequenceLength(subL)  
    {
    }
    
    ~SubsequenceFiller() = default;
    
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

  NumericMatrix subsequencesMatrix(NumericVector values, int subsequenceWidth){
    if(subsequenceWidth < 0)
      stop("Window width have to be positive, integer number.");
    
    int subseqenceLength = (2*subsequenceWidth) + 1;
    int tsLength = values.length();
    NumericMatrix res(tsLength, subseqenceLength);
    
    auto SubFill = new SubsequenceFiller(&res, &values, tsLength, 
                                         subsequenceWidth, subseqenceLength);
    
    for(int i = 0; i < tsLength; i++){
      
      if(((i - subsequenceWidth) < 0) && ((i + subsequenceWidth) >= tsLength)){
        
        (*SubFill).ShortLeftShortRightFiller(i);
        
      }else if((i - subsequenceWidth) < 0){
        
        (*SubFill).ShortLeftFiller(i);
        
      }else if((i + subsequenceWidth) >= tsLength){
        
        (*SubFill).ShortRightFiller(i);
        
      }else{
        (*SubFill).OpenEndedFiller(i);
      }  
    }
    
    delete SubFill;
    return res;
  }
}