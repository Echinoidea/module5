library(matlib)

# Create matrices
A = matrix(1:100, nrow=10)
B = matrix(1:1000, nrow=10)

# Transpose matrices
a_t <- t(A)
b_t <- t(B)

a_rand <- replicate(10, as.integer(abs(rnorm(10)*10)))
b_rand <- replicate(10, as.integer(abs(rnorm(100)*100)))

# Multiply transposed matrices by noise matrices
A2 <- a_t * a_rand
B2 <- b_t * b_rand

# Convert to matrix
a_rand_m <- matrix(a_rand, nrow = 10)
b_rand_m <- matrix(b_rand, nrow = 10)

# Multiply transposed matrices by the product of the original matrices and the transposed matrices
a <- A2 %*% a_rand_m
b <- B2 %*% b_rand_m

# Find the determinants
det(a)
det(b) # Determinant of b = 0, not solvable

# Solve to find the inverse
a_inv <- solve(a)
b_inv <- solve(b, tol = 2.02965e-22)

#' IsCorrect
#'
#' @param m original matrix m
#' @param m_inv inverse of matrix m
#'
#' @return TRUE if m * m^-1 = identity matrix with the same dimensions as m
IsCorrect <- function(m, m_inv) { 
  # if (identical(zapsmall(m %*% m_inv), diag(nrow(m)))) { 
  #   return(TRUE) 
  # }
  # else {
  #   return(FALSE)
  # }
  return(identical(zapsmall(m %*% m_inv), diag(nrow(m))))
}

# Use IsCorrect and pass our matrices and inverted matrices
IsCorrect(a, a_inv)
IsCorrect(b, b_inv)

# Example with hard-coded, non-singular matrix
# m <- matrix(c(3, 5, 8, 20), nrow=2)
# i <- solve(m)
# check_m <- zapsmall(i %*% m)

# Check if IsCorrect works
# IsCorrect(m, i)


