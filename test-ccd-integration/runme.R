## theta = theta.mode + EigenVectors %*%  sqrt(Eigen.Values) %*% z
## where we have factorized the Covariance-matrix

n = 3
dtheta = 0.1
theta.limit = 6
lim.scale = 0.3

Sigma = matrix(runif(n^2), n, n)
Sigma = Sigma %*% t(Sigma)
eig = eigen(Sigma)
EigenValues = diag(eig$values)
EigenVectors = eig$vectors

sd.scale.pos = numeric(n)
sd.scale.neg = numeric(n)
for(i in 1:n) {
    sd.scale.pos[i] = runif(1, min=1, max=1+lim.scale)
    sd.scale.neg[i] = 1/runif(1, min=1, max=1+lim.scale)
    if (runif(1) < 0.5) {
        tmp = sd.scale.pos[i]
        sd.scale.pos[i] = sd.scale.neg[i]
        sd.scale.neg[i] = tmp
    }
}

args = list(
        EigenValues = EigenValues,
        EigenVectors = EigenVectors,
        sd.scale.pos = sd.scale.pos,
        sd.scale.neg = sd.scale.neg
        )

z2theta = function(z, args) {
    return(args$EigenVectors %*% sqrt(args$EigenValues) %*% z)
}

theta2z = function(theta, args) {
    return(1/sqrt(diag(args$EigenValues)) *
           as.vector(t(args$EigenVectors) %*% theta))
}

dens = function(theta, args) {
    z = theta2z(theta, args)
    ldens = 0.0
    for(i in 1:length(z)) {
        if (z[i] > 0) {
            ldens = ldens - 0.5 * (z[i] / args$sd.scale.pos[i])^2
        } else {
            ldens = ldens - 0.5 * (z[i] / args$sd.scale.neg[i])^2
        }
    }

    return (exp(ldens))
}


theta.seq = seq(-theta.limit, theta.limit, by = dtheta)
marginal = c()

if (n == 2) {
    for(th in theta.seq) {
        dens.sum = 0
        for(th2 in theta.seq) {
            theta = c(th, th2) * sqrt(diag(Sigma))
            dens.sum = dens.sum + dens(theta, args)
        }
        marginal = c(marginal, dens.sum)
    }
}
if (n == 3) {
    for(th in theta.seq) {
        dens.sum = 0
        for(th2 in theta.seq) {
            for(th3 in theta.seq) {
                theta = c(th, th2, th3) * sqrt(diag(Sigma))
                dens.sum = dens.sum + dens(theta, args)
            }
        }
        marginal = c(marginal, dens.sum)
    }
}
if (n == 4) {
    for(th in theta.seq) {
        dens.sum = 0
        for(th2 in theta.seq) {
            for(th3 in theta.seq) {
                for(th4 in theta.seq) {
                    theta = c(th, th2, th3, th4) * sqrt(diag(Sigma))
                    dens.sum = dens.sum + dens(theta, args)
                }
            }
        }
        marginal = c(marginal, dens.sum)
    }
}

##par(mfrow=c(2, 1))

marginal = marginal/sum(marginal)
marginal = exp(log(marginal) - max(log(marginal)))
plot(theta.seq * sqrt(Sigma[1, 1]), log(marginal))

marginal.true = exp(-0.5*theta.seq^2)
lines(theta.seq * sqrt(Sigma[1, 1]), log(marginal.true), lty=2)

SigmaBB = Sigma[1, 1]
SigmaAB = Sigma[1, -1]
CondE = SigmaAB/SigmaBB

## positive theta1
ldens = numeric(3)
for(k in 1:3) {
    ldens[k] = log(dens(c(k, CondE*k), args))
}
sd.pos = sqrt(-1/diff(ldens, diff=2))
## negative theta1
ldens = numeric(3)
for(k in 1:3) {
    kk = -k
    ldens[k] = log(dens(c(kk, CondE*kk), args))
}
sd.neg = sqrt(-1/diff(ldens, diff=2))

theta.seq = seq(-2*theta.limit, 2*theta.limit, len=200)

theta.seq.pos = theta.seq[ theta.seq > 0 ]
marginal.approx = exp(-0.5*theta.seq.pos^2)
lines(theta.seq.pos * sd.pos, log(marginal.approx), col="red", lwd=2)

theta.seq.neg = theta.seq[ theta.seq <= 0 ]
marginal.approx = exp(-0.5*theta.seq.neg^2)
lines(theta.seq.neg * sd.neg, log(marginal.approx), col="red", lwd=2)


## plot(theta.seq * sqrt(Sigma[1, 1]), c(diff(log(marginal),  diff=2), NA, NA))
