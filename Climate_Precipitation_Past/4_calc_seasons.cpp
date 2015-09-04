#include<RcppArmadillo.h>
#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
// [[Rcpp::depends(RcppArmadillo)]]
arma::cube identify_seasons(arma::cube wms, arma::cube mms) {
    arma::cube out = wms;
    // Loop over the pixels within wm and mm
    for (int x=0; x < wms.n_rows; x++) {
        for (int y=0; y < wms.n_cols; y++) {
            arma::vec wm = wms.tube(x, y);
            arma::vec mm = mms.tube(x, y);

            // Extend series so seasons can wrap around beg/end of year
            arma::vec wm_wrap = arma::repmat(wm, 1, 3);
            arma::vec mm_wrap = repmat(mm, 1, 3);
            // In forward direction, only need to process max indices in the 
            // first two thirds of the wrapped series
            arma::uvec fwd_indices = arma::find(wm_wrap);
            fwd_indices = fwd_indices(fwd_indices <= (2*wm.n_elem));
            for (int i=0; i < fwd_indices.n_elem; i++) {
                int max_i = fwd_indices(i);
                int lag_n = 0;
                while (lag_n <= 6) {
                    // if ((mm_wrap[max_i + lag_n] >= .3*mm_wrap[max_i]) |
                    //     (mm_wrap[max_i + lag_n] >= 2*mm_wrap[max_i + lag_n + 1])) {
                    if (mm_wrap(max_i + lag_n) >= .3*mm_wrap(max_i)) {
                        wm_wrap(max_i + lag_n) = 1;
                        lag_n++;
                    } else {
                        break;
                    }
                }
            }
            // In backwards direction, only need to process max indices in the 
            // second two thirds of the wrapped series
            arma::uvec bwd_indices = arma::find(wm_wrap == 1);
            bwd_indices = bwd_indices(bwd_indices > wm.n_elem);
            for (int i=0; i < bwd_indices.n_elem; i++) {
                int max_i = bwd_indices(i);
                int lag_n = 0;
                while (lag_n < 6) {
                    // if ((mm_wrap[max_i - lag_n] >= .3*mm_wrap[max_i]) |
                    //     (mm_wrap[max_i - lag_n] >= 2*mm_wrap[max_i - lag_n - 1])) {
                    if (mm_wrap(max_i - lag_n) >= .3*mm_wrap(max_i)) {
                        wm_wrap(max_i - lag_n) = 1;
                        lag_n++;
                    } else {
                        break;
                    }
                }
            }
            // Pull out indicators from middle of wrapped series
            arma::vec this_out = wm_wrap(arma::span(wm.n_elem + 1, 2 * wm.n_elem));
            // Now number seasons sequentially. Assign positive numbers to wet 
            // seasons and negative numbers to dry seasons
            for (int i=1; i < this_out.n_elem; i++) {
                if (this_out(i) == this_out(i-1))
                out.tube(x, y) = wm_wrap(arma::span(wm.n_elem + 1, 2 * wm.n_elem));
            }
        }
    }
    return(out);
}
