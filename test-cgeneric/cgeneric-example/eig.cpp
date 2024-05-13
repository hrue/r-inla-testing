#include <math.h> 
#include <Eigen/Dense>

extern "C" void eigen_Q(double *Q, int n);
void eigen_Q(double *Q, int n)
{
	Eigen::MatrixXd q(n, n);

	for(int i = 0; i < n; i++) {
		for(int j = 0; j < n; j++) {
			q(i, j) = (i == j ? 1.0 : 0.0) + sin(i + 2.0 * j);
		}
	}
	q = q.transpose() * q;

	double sum = 0.0;
	for(int i = 0; i < n; i++) {
		sum += q(i, i);
	}

	for(int i = 0, k = 0; i < n; i++) {
		for(int j = i; j < n; j++) {
			Q[k++] = q(i, j) / sum;
		}
	}
}
