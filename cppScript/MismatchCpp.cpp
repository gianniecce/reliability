#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/


// Mismatch(Am = as.matrix(sek), hid = hid, Total = dd$total) 

// [[Rcpp::export]]
NumericVector Mismatch(CharacterMatrix Am, CharacterVector hid, NumericVector Total) {
  
  Rcout << "Must Be a Character Matrix No Numeric" << std::endl; 
  
  int nrows = Am.nrow();
  int ncolumns = Am.ncol();

    for (int i = 1; i < nrows; i ++){
      for (int j = 0; j < ncolumns; j++){
      
      if( (hid(i) == hid(i-1)) & ( Am(i,j) != Am(i-1,j) ) ){
        Total(i) = Total(i) + 1; 
        }
      
      if( (hid(i) == hid(i-1)) & ( Am(i,j) != Am(i-1,j) ) ){
        Total(i-1) = Total(i-1) + 1; 
      }
      
    }
  }
  return(Total); 
}


