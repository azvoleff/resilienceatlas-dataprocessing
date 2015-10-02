// [[Rcpp::depends(RcppArmadillo)]]

#include<RcppArmadillo.h>
#include<Rcpp.h>

using namespace arma;

// [[Rcpp::export]]
cube identify_seasons(Rcpp::NumericVector mms_vec, Rcpp::IntegerVector dims) {
    cube mms(mms_vec.begin(), dims[0], dims[1], dims[2], false);
    cube out(dims[0], dims[1], dims[2]);

    // Loop over the pixels within mm
    for (int x=0; x < mms.n_rows; x++) {
        for (int y=0; y < mms.n_cols; y++) {
            vec mm = vectorise(mms.tube(x, y));

            // float total_annual_precip = sum(mm);

            // Extend series so seasons can wrap around beg/end of year
            vec mm_wrap = repmat(mm, 3, 1);

            uvec maxima = find(diff(sign(diff(mm_wrap))) == -2) + 1;
            uvec minima = find(diff(sign(diff(mm_wrap))) == 2) - 1;

            uvec extrema = sort(join_cols(maxima, minima));

            maxima.print();
            minima.print();

            vec seasons(mm.n_elem);
            // Loop over the middle section of the wrapped mm vector (mm_wrap).  
            // seasons is of length equal to original (unwrapped) mm vector, so 
            // note that when indexing seasons using i, need to subtract 
            // mm.n_elem from i to index correctly.
            for (int i=mm.n_elem; i < (mm.n_elem*2); i++) {
                // don't alter seasonal assignment for extrema
                if (any(extrema == i)) {
                    seasons(i - mm.n_elem) = i;
                } else {
                    // identify extrema on left and right
                    int ext_l = max(extrema(find(extrema < i)));
                    int ext_r = min(extrema(find(extrema > i)));
                    // temporary holder for this season assignment
                    int this_season;
                    if (mm_wrap(ext_l) > mm_wrap(i)) {
                        // left extrema is greater than this point
                        if ((mm_wrap(ext_l) / mm_wrap(i)) < (mm_wrap(i) / mm_wrap(ext_r))) {
                            this_season = ext_l;
                        } else {
                            this_season = ext_r;
                        }
                    } else {
                        // left extrema is less than this point
                        if ((mm_wrap(ext_r) / mm_wrap(i)) < (mm_wrap(i) / mm_wrap(ext_l))) {
                            this_season = ext_r;
                        } else {
                            this_season = ext_l;
                        }
                    }
                    // handle wrapping around for season at the end of the year
                    if (this_season >= mm.n_elem*2) {
                        seasons(i - mm.n_elem) = seasons(0);
                    } else {
                        seasons(i - mm.n_elem) = this_season;
                    }
                }
            }

            // Don't allow seasons that are only a month long. If a season is 
            // only a month long, join it with the closest adjoining season 
            // (using same ratio test as above).
            vec seas_ids = seasons(find_unique(seasons));
            // for (int i=0; i < seas_ids.n_elem; i++) {
            //     uvec ind = find(seasons == seas_ids);
            //     if (ind.n_elem == 1) {
            //         if ((mm_wrap(mm.n_elem + ind) / mm_wrap(mm.n_elem + 1) >
            //             (mm_wrap(mm.n_elem + ind) / mm_wrap(mm.n_elem - 1)}
            //     }
            //
            //     }
            // }
            

            // Setup list of season ids again because seasons may have changed 
            // if any seasons were 1 month long.
            seas_ids = seasons(find_unique(seasons));
            vec seas_tot(seas_ids.n_elem);
            for (int i=0; i < seas_tot.n_elem; i++) {
                seas_tot(i) = sum(mm(find(seasons == seas_ids(i))));
            }
            // Order seas_ids by total precipitation received in each season.
            seas_ids = seas_ids(sort_index(seas_tot));

            // Number seasons in order of the total amount of rain received.
            vec this_out = seasons;
            for (int i=0; i < seas_ids.n_elem; i++) {
                this_out(find(seasons == seas_ids(i))).fill(i);
            }
            out.tube(x, y) = this_out;
        }
    }
    return(out);
}
