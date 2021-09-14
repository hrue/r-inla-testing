x <- factor(c("a","b","c"))
xx <- as.factor(1:3)  

r <- inla(y ~ 1 + x + xx, 
          data = data.frame(y = 0, x, xx),
          control.fixed = list(remove.names = c("a", "b")))
r$model.matrix


