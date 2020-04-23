# Some XY coords in decimal degrees
xy1 <- data.frame(x=rnorm(3, -90, 1),
                  y=rnorm(3, 30, 1),
                  id=LETTERS[1:3])

xy2 <- data.frame(x=rnorm(5, -90, 1),
                  y=rnorm(5, 35, 1),
                  id=LETTERS[4:8])

D <- get_crossdist(xy1=xy1[,1:2],
                   xy2=xy2[,1:2],
                   id1=xy1[,3],
                   id2=xy2[,3])
D
