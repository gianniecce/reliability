#include <Rcpp.h>
using namespace Rcpp;

// Mismatch(Am = as.matrix(sek), hid = hid, Total = dd$total) 

// [[Rcpp::export]]
NumericMatrix RetrieveInfoCondMatrix(NumericMatrix mat1, 
                                       NumericMatrix mat3, 
                                     CharacterVector hid) {
  
  Rcout << "Must Be a Character Matrix Not Numeric" << std::endl; 
  
  int nrows = mat1.nrow();
  int ncolumns = mat1.ncol();

  for (int i = 1; i < nrows; i ++){
    for (int j = 0; j < ncolumns; j++){
      
      if( (hid(i) == hid(i-1)) & ( mat1(i,j) != mat1(i-1,j) ) ){
        mat3(i,j) = 1;  
      }
      
      if( (hid(i) == hid(i-1)) & ( mat1(i,j) != mat1(i-1,j) ) ){
        mat3(i-1,j) = 1;  
      }
    }
  } 
  
  return mat3; 
} 


