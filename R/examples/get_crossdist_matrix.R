# Some XY coords in decimal degrees
xy1 <- data.frame(x=rnorm(3, 100, 1),
                  y=rnorm(3, 20, 1),
                  id=LETTERS[1:3])

xy2 <- data.frame(x=rnorm(5, 100, 1),
                  y=rnorm(5, 20, 1),
                  id=LETTERS[4:8])

D <- get.crossdist(xy1=xy1[,1:2],
                   xy2=xy2[,1:2],
                   id1=xy1[,3],
                   id2=xy2[,3])

D
D*111.35 # in km


# Overlapping sets of coordinates
xy1 <- data.frame(x=rnorm(3, 100, 1),
                  y=rnorm(3, 20, 1),
                  id=LETTERS[1:3])

xy2 <- rbind(xy1,
             data.frame(x=rnorm(5, 100, 1),
                        y=rnorm(5, 20, 1),
                        id=LETTERS[4:8]))

D <- get.crossdist(xy1=xy1[,1:2],
                   xy2=xy2[,1:2],
                   id1=xy1[,3],
                   id2=xy2[,3])

D
D*111.35 # in km
