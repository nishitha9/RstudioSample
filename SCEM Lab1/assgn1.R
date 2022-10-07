#Creating a vector of animals
animals <- c("monkey","dog","cat")
print(animals)
#Crating a vector which has the count of the legs
num_legs <- c(4,4,4)
print(num_legs)
#Combining the two vectors using data frame
animals_df <- data.frame(
  animals,num_legs
)
print(animals_df)
#Creating a vector using seq function 
x_vect <- seq(from = 12, to = 2, by = -2)
print(x_vect)

#Converting x_vector into matrix
X=matrix(x_vect, nrow = 2, ncol = 3, byrow = FALSE)
print(X)

#Creating a 2*2 matrix Y
Y=matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = FALSE)
print(Y)

#Creating a 2*2 matrix Z
Z=matrix(c(4,6,8,10), nrow = 2, ncol = 2, byrow = FALSE)
print(Z)

#Creating transpose of Y and Z

YTranspose= t(Y)
print(YTranspose)

ZTranspose <- t(Z)
print(ZTranspose)

#Adding original and Transpose matrix to verify the theorem

sumOfYAndZ = Y+Z
print(sumOfYAndZ)

sumOfYAndZTranspose = YTranspose+ZTranspose
print(sumOfYAndZTranspose)

originalSum=sum(sumOfYAndZ)
print(originalSum)

transponseSum=sum(sumOfYAndZTranspose)
print(transponseSum)

if(originalSum == transponseSum){
  print("Theorem Verified")
}

#Matrix Multiplication- Non Element Wise
YZ = Y%*%Z
print(YZ)

ZY=Z%*%Y
print(ZY)

YX=Y%*%X
print(YX)

##  Element Wise Multiplication is Non Commutative


#XY=X%*%Y
#print(XY)

#Matrix Multiplication-  Element Wise
YZElement = Y*Z
print(YZElement)

ZYElement=Z*Y
print(ZYElement)

##  Element Wise Multiplication is Commutative

#Matrix Inverse
YInverse= solve(Y)
print(YInverse)

print(YInverse%*%Y)
print(YInverse%*%X)

