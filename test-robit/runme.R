n = 500
Nt = 2
x = rnorm(n, sd = 0.3)
eta = 1 + x
df = 7
y = rbinom(n, size=Nt, prob = inla.link.invrobit(eta, df = df))

r = inla(y ~ 1 + x,
         family = "binomial",
         Ntrials = Nt,
         data = data.frame(y, x, Nt),
         control.family = list(
             control.link = list(
                 model = "probit")))
rr = inla(y ~ 1 + x,
          family = "binomial",
          Ntrials = Nt,
          data = data.frame(y, x, Nt),
          control.family = list(
              control.link = list(
                  model = "robit",
                  initial = log(df - 2), 
                  hyper = list(dof = list(fixed = FALSE)))), 
          verbose=TRUE)
summary(r)
summary(rr)

         
