
#include <time.h>
#if !defined(__FreeBSD__)
#include <malloc.h>
#endif
#include <assert.h>
#if !defined(__FreeBSD__)
#endif
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <omp.h>

#include "cgeneric.h"

#define Calloc(n_, type_)  (type_ *)calloc((n_), sizeof(type_))
#define SQR(x) ((x)*(x))

void write_to_log_file(const char *filename, const char *message) {
	FILE *log_file;
	log_file = fopen(filename, "a");
	fprintf(log_file, "%s", message);
	fclose(log_file);
}

double *inla_cgeneric_alpha_model(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp * data)
{
	static double time_used = 0.0;
	static int ntimes = 0;
	
	double *ret = NULL, lambda, kappa;

	if (theta) {
		lambda = exp(theta[0]);
		kappa = exp(theta[1]);
	} else {
		lambda = kappa = NAN;
	}

	double high_prec = exp(15);
	double gaussian_const = -0.91893853320467;

	// The total number of observations
	assert(!strcasecmp(data->ints[0]->name, "n"));
	int n = data->ints[0]->ints[0];
	assert(n > 0);

	// GMRFLib_cpu();

	// Should we create a log-file for debugging purposes?
	assert(!strcasecmp(data->ints[2]->name, "logging"));
	//int logging = data->ints[2]->ints[0];

	// Which s0 do the different y(s0)'s come from?
	assert(!strcasecmp(data->ints[3]->name, "s0_index"));
	int n_y_s0 = data->ints[3]->len;
	int* s0_index = data->ints[3]->ints;

	// All values of y(s0) for all the different s0's
	assert(!strcasecmp(data->doubles[0]->name, "y_s0"));
	assert(n_y_s0 == data->doubles[0]->len);
	double *y_s0 = data->doubles[0]->doubles;

	// Initial values for the hyperparameters
	assert(!strcasecmp(data->doubles[1]->name, "init"));
	double *init = data->doubles[1]->doubles;
	assert(data->doubles[1]->len == 2);

	// PC priors for lambda and kappa
	assert(!strcasecmp(data->doubles[2]->name, "log_lambda_prior"));
	double *log_lambda_prior = data->doubles[2]->doubles;
	assert(!strcasecmp(data->doubles[3]->name, "log_kappa_prior"));
	double *log_kappa_prior = data->doubles[3]->doubles;

	// First index in data->doubles that correspond to a dist_to_s0 object
	int dist_to_s0_start = 4;

	switch (cmd) {
	case INLA_CGENERIC_VOID:
	{
		assert(!(cmd == INLA_CGENERIC_VOID));
		break;
	}

	case INLA_CGENERIC_GRAPH:
	{
		// return a vector of indices with format
		// c(n, M, ii, jj)
		// where ii<=jj and both ii and jj are non-decreasing
		// and M is the length of ii
		ret = Calloc(2 + 2 * n, double);
		ret[0] = n;
		ret[1] = n;
		for (int i = 0; i < n; i++) {
			ret[2 + i] = i;			       /* i */
			ret[2 + n + i] = i;		       /* j */
		}

		break;
	}

	case INLA_CGENERIC_Q:
	{
		if (1) {
			// optimized format
			// return c(n, M, Qij) in the same order as defined in INLA_CGENERIC_GRAPH
			// where M is the length of Qij
			ret = Calloc(2 + n, double);
			ret[0] = -1;			       /* code for optimized output */
			ret[1] = n;			       /* number of (i <= j) */
			for (int i = 0; i < n; i++) {
				ret[2 + i] = high_prec;
			}
		} else {
			// plain format, but the optimized format above is better to use
			// return c(n, M, ii, jj, Qij)
			// where ii<=jj and both ii and jj are non-decreasing
			// and M is the length of ii
			ret = Calloc(2 + 3 * n, double);
			ret[0] = n;
			ret[1] = n;
			for (int i = 0; i < n; i++) {
				ret[2 + i] = i;                      /* i */
				ret[2 + n + i] = i;	               /* j */
				ret[2 + 2 * n + i] = high_prec;      /* Q_ij */
			}
		}
		break;
	}

	case INLA_CGENERIC_MU:
	{
		double tref = omp_get_wtime();
		// return (n, mu)
		// if n==0 then mu is not needed as its taken to be mu[]==0
		ret = Calloc(1 + n, double);
		ret[0] = n;
//#pragma omp parallel for
		for (int i = 0; i < n_y_s0; i++) {
			int dist_to_s0_index = dist_to_s0_start + s0_index[i] - 1; // R uses 1-indexing
			int len = data->doubles[dist_to_s0_index]->len;
			int m = i * len;
			for (int j = 0; j < len; j++) {
				ret[1 + m + j] = y_s0[i] *
					exp(-pow(data->doubles[dist_to_s0_index]->doubles[j] / lambda, kappa));
			}
		}

		time_used += omp_get_wtime() - tref;
		ntimes++;
		
		break;
	}

	case INLA_CGENERIC_INITIAL:
	{
		// return c(M, initials)
		// where M is the number of hyperparameters
		ret = Calloc(3, double);
		ret[0] = 2;
		ret[1] = init[0];
		ret[2] = init[1];
		break;
	}

	case INLA_CGENERIC_LOG_NORM_CONST:
	{
		// return c(NORM_CONST) or a NULL-pointer if INLA should compute it by itself
		ret = Calloc(1, double);
		ret[0] = n * (gaussian_const + 0.5 * log(high_prec));
		break;
	}

	case INLA_CGENERIC_LOG_PRIOR:
	{
		printf("\nTIME spent %f\n", time_used);

		// return c(LOG_PRIOR)
		ret = Calloc(1, double);
		ret[0] = 2 * gaussian_const - log(log_kappa_prior[1]) - log(log_lambda_prior[1]) -
			pow(lambda - log_lambda_prior[0], 2) / (2 * pow(log_lambda_prior[1], 2)) -
			pow(kappa - log_kappa_prior[0], 2) / (2 * pow(log_kappa_prior[1], 2));
		break;
	}

	case INLA_CGENERIC_QUIT:
	default:
		break;
	}

	return ret;
}

