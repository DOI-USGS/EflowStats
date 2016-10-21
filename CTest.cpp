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
//
// [[Rcpp::export]]
NumericVector pdistC(double thresh, NumericVector x) {
        int n = x.size();
        NumericVector out(n);
        int flag = 0;
        double temp; 
        int nevents = 0;
        
                
        for (int i = 0; i < n; ++i) {
                if (x[i]>thresh) {
                        flag = flag+1;
                        temp = x[i];
                        if(flag==1){
                                nevents =nevents+1;
                        } else (nevents = nevents);
                if(temp >peak[nevents]) {
                        peak[nevents] = temp;
                        else(peak[nevents] = temp);
                }
                
                } else {flag = 0}
        }
}



