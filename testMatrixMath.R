## Coursera
## R Programming - Johns Hopkins University
## Week 3
## Programming Assignment 2: Lexical Scoping
#-------------------------------------------------------------------------------
## Companion working notes for cachematrix.R
################################################################################
### generic matrix math 
################################################################################
# https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html

A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

# test whether inverse exists (det != 0)
det(A)

# find the inverse
AI <- solve(A)

# verify inverse
# AI %*% A should equal the identity matrix, also generated via diag(nrow(A))

AI %*% A

I <- diag(nrow(A))

# https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
if (all(AI %*% A == diag(nrow(A)))) {
  message("seems OK")
} else {
  message("oopsie")
}

# check matrix squareness

dim(A)

dim(A)[1]

dim(A)[2]

B <- matrix( c(5, 1, 0, 2,
               3,-1, 2, 1,
               4, 0,-1, 0), nrow=3, byrow=TRUE)

dim(B)

dim(B)[1]

dim(B)[2]

class(B)

det(B)

C <- matrix( c(5, 1, 0,
               10,2, 0,
               4, 0,-1), nrow=3, byrow=TRUE)

det(C)

################################################################################
### testing cachematrix.R
################################################################################

ACM <- makeCacheMatrix(A)

ACM$get()

cacheSolve(ACM)

BCM <- makeCacheMatrix(B)

cacheSolve(BCM)

CCM <- makeCacheMatrix(C)

cacheSolve(CCM)

################################################################################
### test cached vs uncached performance
################################################################################
ACM <- makeCacheMatrix(A)

ptm <- proc.time()

cacheSolve(ACM)   # uncached

proc.time() - ptm

ptm <- proc.time()

cacheSolve(ACM)   # cached

proc.time() - ptm

# too short; need a longer runtime

# generate random matrix
set.seed(123)
D <- matrix( sample(1:50,10000,replace=TRUE), nrow=100, byrow=TRUE)

# verify invertibility
det(D)

ptm <- proc.time()
solve(D)
proc.time() - ptm

# test 

DCM <- makeCacheMatrix(D)

ptm <- proc.time()

cacheSolve(DCM)   # uncached

proc.time() - ptm

# > proc.time() - ptm
# user  system elapsed 
# 0.002   0.000   0.021 


ptm <- proc.time()

cacheSolve(DCM)   # cached

proc.time() - ptm

# > proc.time() - ptm
# user  system elapsed 
# 0.012   0.003   0.015 

print(proc.time())


#-------------------------------------------------------------------------------
# benchmark testing using microbenchmark
# https://www.alexejgossmann.com/benchmarking_r/
#-------------------------------------------------------------------------------
newMatrixTest <- function() {
  E <- D
  ECM <- makeCacheMatrix(E)
  cacheSolve(ECM)
}
existMatrixTest <- function() {
  E <- D
  ECM <- makeCacheMatrix(E)
  cacheSolve(DCM)
}


library(microbenchmark)

mbm <- microbenchmark("freshSolve" = {newMatrixTest()},
                      "cacheSolve" = {existMatrixTest()},
                      times = 10)
mbm
library(ggplot2)
autoplot(mbm)
