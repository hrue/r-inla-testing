n = 10
i = paste("a",  1:10, sep="")
ifac = factor(i, i)
ii = rep(i,  each = 10)
iifac = factor(ii, levels(ifac))
z = rnorm(10)
y = z[iifac] + rnorm(length(iifac))

formula = y ~ -1 + f(iifac, values = i)
r = inla(formula,  data = data.frame(iifac, y), verbose=TRUE,  keep=TRUE,
        control.data = list(hyper = list(prec = list(initial = 0,  fixed=FALSE))))

stop("XXX")

i = as.factor(paste("a",  1:n, sep=""))
j = as.factor(paste("a",  2:n, sep=""))

stopifnot(length(setdiff(levels(j), levels(i))) == 0L)

## convert j into the levels of 'i'.

sum(is.na(j)) ## FALSE
jj = factor(j,  levels(i))
sum(is.na(jj)) == sum(is.na(j))  ## otherwise,  j has a level not in levels(i)

match("a2", levels(i))
match("a2", levels(j))

y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
i = rev(c("a", "b",  "c", "d",  "e",  "f",  "g",  "h", "i", "j"))
formula = y ~ -1 + f(i, values=i, model="crw2")
r = inla(formula, data = data.frame(y, i), keep=TRUE, verbose=TRUE,
        control.data = list(hyper = list(prec = list(initial = 12,  fixed=TRUE))))

 
