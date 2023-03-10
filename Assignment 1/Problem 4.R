tempA <- matrix(c(3,-1,-2,4),nrow = 2,ncol = 2)
A <- cbind(tempA,c(1,-2))
tempB <- matrix(c(-7,4,9,5),nrow = 2,ncol = 2, byrow = T)
B <- rbind(tempB,c(2,-1))
multiplicationAB <- A%*%B
print(multiplicationAB)
transposeAB <- t(multiplicationAB)
print(transposeAB)
inverseAB <- solve(multiplicationAB)
print(inverseAB)
rowMeans(A); colMeans(A)
rowMeans(B); colMeans(B)
rowMeans(multiplicationAB); colMeans(multiplicationAB)
rowMeans(transposeAB); colMeans(transposeAB)
rowMeans(inverseAB); colMeans(inverseAB)

sd(A[,1]); sd(A[,2]); sd(A[,3])
sd(A[1,]); sd(A[2,])

sd(B[,1]); sd(B[,2])
sd(B[1,]); sd(B[2,]); sd(B[3,])

sd(multiplicationAB[,1]); sd(multiplicationAB[,2])
sd(multiplicationAB[1,]); sd(multiplicationAB[2,])

sd(transposeAB[,1]); sd(transposeAB[,2])
sd(transposeAB[1,]); sd(transposeAB[2,])

sd(inverseAB[,1]); sd(inverseAB[,2])
sd(inverseAB[1,]); sd(inverseAB[2,])