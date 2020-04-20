n <- 5

xy <- data.frame(x=rnorm(n, 100, 1),
                 y=rnorm(n, 20, 1),
                 id=LETTERS[1:n])

D <- get_distance_matrix(x=xy[,1],
                         y=xy[,2],
                         id=xy[,3])
