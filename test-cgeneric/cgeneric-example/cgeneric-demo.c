#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <strings.h>

#include "cgeneric.h"

#if !defined(_OPENMP)
#error File must be compiled with OpenMP
#endif	

#define Calloc(n_, type_)  (type_ *)calloc((n_), sizeof(type_))
#define SQR(x) ((x)*(x))

void eigen_Q(double *Q, int n);

typedef struct 
{
	double *Q_unscaled;
}
	my_cache_tp;

double *inla_cgeneric_iid_model(inla_cgeneric_cmd_tp cmd, double *theta, inla_cgeneric_data_tp * data)
{
	double *ret = NULL;
	double prec = (theta ? exp(theta[0]) : NAN);
	double lprec = (theta ? theta[0] : NAN);

	assert(!strcasecmp(data->ints[0]->name, "n"));	       // this will always be the case
	int N = data->ints[0]->ints[0];			       // this will always be the case
	assert(N > 0);

	if (!(data->cache)) {
#pragma omp critical  (Name_7c3b4712ebb2dda8def3a5273e2a7e6cf1794b5d)
		if (!(data->cache)) {
			data->cache = (void **) Calloc(CGENERIC_CACHE_LEN(data), my_cache_tp *);
		}
	}

	int cache_idx;
	CGENERIC_CACHE_ASSIGN_IDX(cache_idx, data);
	my_cache_tp *cache = ((my_cache_tp **) data->cache)[cache_idx];
	
	switch (cmd) {
	case INLA_CGENERIC_VOID:
	{
		assert(!(cmd == INLA_CGENERIC_VOID));
	}
	break;

	case INLA_CGENERIC_GRAPH:
	{
		// return a vector of indices with format
		// c(N, M, ii, jj)
		// where ii<=jj, ii is non-decreasing and jj is non-decreasing for the same ii
		// so like the loop
		// for i=0, ...
		// for j=i, ...
		// G_ij = 
		// and M is the total length while N is the dimension

		int M = N * (N + 1) / 2;
		ret = Calloc(2 + 2 * M, double);
		assert(ret);
		ret[0] = N;				       /* dimension */
		ret[1] = M;				       /* number of (i <= j) */
		for (int i = 0, k = 0; i < N; i++) {
			for (int j = i; j < N; j++) {
				ret[2 + k] = i;	
				ret[2 + M + k] = j;
				k++;
			}
		}
	}
	break;

	case INLA_CGENERIC_Q:
	{
		// return c(-1, M, Qij) in the same order as defined in INLA_CGENERIC_GRAPH
		int M = N * (N + 1) / 2;
		ret = Calloc(2 + M, double);
		assert(ret);
		ret[0] = -1;	
		ret[1] = M;

		if (cache) {
			// get the unscaled Q from the cache
			memcpy(ret + 2, cache->Q_unscaled, M * sizeof(double));
#pragma omp critical
			printf("use cache_idx = %1d\n", cache_idx);
		} else {
			// compute the unscaled Q
			eigen_Q(ret + 2, N);
			// then cache it
			((my_cache_tp **) data->cache)[cache_idx] = cache = Calloc(1, my_cache_tp);
			cache->Q_unscaled = Calloc(M, double);
			memcpy(cache->Q_unscaled, ret + 2,  M * sizeof(double));
#pragma omp critical
			printf("fill cache_idx = %1d\n", cache_idx);
		}

#pragma omp simd
		for(int i = 0; i < M; i++) {
			ret[2 + i] *= prec;
		}
	}
	break;

	case INLA_CGENERIC_MU:
	{
		// return (N, mu)
		// if N==0 then mu is not needed as its taken to be mu[]==0
		ret = Calloc(1, double);
		assert(ret);
		ret[0] = 0;
	}
	break;

	case INLA_CGENERIC_INITIAL:
	{
		// return c(M, initials)
		// where M is the number of hyperparameters
		ret = Calloc(2, double);
		assert(ret);
		ret[0] = 1;
		ret[1] = 4.0;
	}
	break;

	case INLA_CGENERIC_LOG_NORM_CONST:
	{
		ret = NULL;
	}
	break;

	case INLA_CGENERIC_LOG_PRIOR:
	{
		// return c(LOG_PRIOR)
		ret = Calloc(1, double);
		assert(ret);
		ret[0] = -prec + lprec;			       // prec ~ gamma(1,1)
	}
	break;

	case INLA_CGENERIC_QUIT:
	default:
		break;
	}

	return (ret);
}
