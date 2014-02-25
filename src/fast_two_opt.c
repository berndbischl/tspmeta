/*
 * fast_two_opt.c - Fast implementation of the 2-opt TSP heuristic.
 *
 * To compile run:
 *   
 *   R CMD SHLIB fast_two_opt.c
 *
 * Author: Olaf Mersmann <olafm@datensplitter.net>
 *
 */
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

#define DISTANCE(I, J) distance_matrix[(I - 1) * number_of_cities + (J - 1)]

#ifdef DEBUG
static void print_vector(int *x, int n) {
    int i;
    for (i = 0; i < n; ++i) {
        Rprintf("%i ", x[i]);
    }
}
#endif

SEXP do_fast_two_opt(SEXP s_distance_matrix, SEXP s_start_tour) {
    const R_len_t number_of_cities = nrows(s_distance_matrix);
    int *start_tour = INTEGER(s_start_tour);
    double *distance_matrix = REAL(s_distance_matrix);
    int number_of_swaps = 0;
    R_len_t current, other;
    SEXP s_tour, s_res;

    PROTECT(s_tour = allocVector(INTSXP, number_of_cities));
    int *tour = INTEGER(s_tour);

    if (length(s_start_tour) != number_of_cities)
        error("Invalid inital tour "
              "(length != number of destinations in distance matrix)");
    
    for (current = 0; current < number_of_cities; current++) {
        tour[current] = start_tour[current];
        if (start_tour[current] < 1 
            || start_tour[current] > number_of_cities)
            error("Invalid inital tour "
                  "(contains invalid destinations)");
    }
    
    while(1) {
        double best_improvement = 0;
        int best_current = -1, best_other = -1;
        
        for (current = 0; current < number_of_cities - 1; ++current) {
            const int current_left_city = tour[current];
            const int current_right_city = tour[current + 1];
            const double distance_cl_cr = DISTANCE(current_left_city, 
                                                   current_right_city);
            
            for (other = current + 1; other < number_of_cities; ++other) {
                const int other_left_city = tour[other];
                /* Make sure we wrap gracefully over to the other end
                 * of the tour vector. */
                const int other_right_city = tour[(other + 1) % number_of_cities];
                const double distance_ol_or = DISTANCE(other_left_city, 
                                                       other_right_city);

                const double distance_cl_ol = DISTANCE(current_left_city, 
                                                       other_left_city);

                const double distance_cr_or = DISTANCE(current_right_city, 
                                                       other_right_city);

                const double improvement = (distance_cl_cr + distance_ol_or)
                    - (distance_cl_ol + distance_cr_or);
                
                if (improvement > best_improvement) {
                    best_improvement = improvement;
                    best_current = current;
                    best_other = other;
                }
            }
        }

        if (best_improvement > 0) {
            /* Since we swap edges, we need to reverse the cities
             * between best_current and best_other+1 in our
             * 'tour' vector to reflect this.
             */
#ifdef DEBUG
            Rprintf("%i <=> %i: ", best_current, best_other);
            print_vector(tour, number_of_cities);
#endif
            for (current = best_current + 1, other=best_other; 
                current <= other;
                ++current, --other) {
                int tmp = tour[current];
                tour[current] = tour[other];
                tour[other] = tmp;
            }
#ifdef DEBUG
            Rprintf("=> ");
            print_vector(tour, number_of_cities);
            Rprintf("\n");
#endif
            ++number_of_swaps;
        } else {
            /* No improvement ... time to exit. */
            break;
        }
    }
    
    PROTECT(s_res = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(s_res, 0, s_tour);
    SET_VECTOR_ELT(s_res, 1, ScalarInteger(number_of_swaps));
    UNPROTECT(2); /* s_tour, s_res */
    return s_res;
}
