list <- rownames(index_obr_fy)
list

columns <- length(list)

columns
testdf <- data.frame(matrix(0, nrow = 45, ncol = columns, byrow = TRUE))
colnames(testdf) <- list
testdf

matrix(0, nrow = columns, ncol = columns)

L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
(d <- data.frame(x = 1, y = 1:10, fac = fac))

class(list)
