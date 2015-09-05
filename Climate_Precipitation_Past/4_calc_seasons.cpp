// [[Rcpp::depends(RcppArmadillo)]]

#include<RcppArmadillo.h>
#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
arma::cube identify_seasons(NumericVector wms_vec, NumericVector mms_vec, IntegerVector dims) {
    arma::cube wms(wms_vec.begin(), dims[0], dims[1], dims[2], false);
    arma::cube mms(mms_vec.begin(), dims[0], dims[1], dims[2], false);
    arma::cube out(dims[0], dims[1], dims[2]);

    // wms.print();
    // mms.print();
    // out.print();

    // Loop over the pixels within wm and mm
    for (int x=0; x < wms.n_rows; x++) {
        for (int y=0; y < wms.n_cols; y++) {
            arma::vec wm = arma::vectorise(wms.tube(x, y));
            arma::vec mm = arma::vectorise(mms.tube(x, y));

            // Extend series so seasons can wrap around beg/end of year
            Rcpp::Rcout << "test";
            arma::vec wm_wrap = arma::repmat(wm, 3, 1);
            arma::vec mm_wrap = arma::repmat(mm, 3, 1);
            Rcpp::Rcout << "test1";
            // In forward direction, only need to process max indices in the 
            // first two thirds of the wrapped series
            arma::uvec fwd_indices = arma::find(wm_wrap);
            fwd_indices = fwd_indices(fwd_indices <= (2*wm.n_elem));
            for (int i=0; i < fwd_indices.n_elem; i++) {
                int max_i = fwd_indices(i);
                int lag_n = 1;
                while (lag_n <= 6) {
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
                int lag_n = 1;
                while (lag_n <= 6) {
                    if (mm_wrap(max_i - lag_n) >= .3*mm_wrap(max_i)) {
                        wm_wrap(max_i - lag_n) = 1;
                        lag_n++;
                    } else {
                        break;
                    }
                }
            }
            // TODO: Renumber seasons so numbers are positive for rainy seasons 
            // with the wettest season being 1, second being 2. Number dry 
            // seasons with negative numbers, with driest being negative 1, 
            // second driest -2
            // arma::vec this_out = wm_wrap(arma::span(wm.n_elem + 1, 2 * wm.n_elem));
            // for (int i=1; i < this_out.n_elem; i++) {
            //     if (this_out(i) == this_out(i - 1))
            //     out.tube(x, y) = wm_wrap(arma::span(wm.n_elem + 1, 2 * wm.n_elem));
            // }

            // Pull out indicators from middle of wrapped series
            out.tube(x, y) = wm_wrap(arma::span(wm.n_elem + 1, 2 * wm.n_elem));
        }
    }
    return(out);
}
