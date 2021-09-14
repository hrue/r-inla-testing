N <- 1000
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon
 
write.table(data.frame(X = x, Y = y, Epsilon = epsilon),
            file = 'example2.data',
            row.names = FALSE,
            col.names = TRUE)
