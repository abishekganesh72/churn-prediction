# Equation: z = a+b*y

v = 750000
# Simulate y
y = runif(v, -1, 1); y
#y = runif(1000, 0, 1); y

# Choose a & b such that a & b are small
a = 0.2
b = 0.5
z = a+b*y;z

# Probablity function
p = 1/(1+exp(-z));p

# Generate a random number to evaluate 
r = runif(v);r

# Function to have 0s & 1s only
x = ifelse(r<=p, 1, 0);x

# Check params for glm to be same as a & b
glm(x~y, data = data.frame(x,y), family = "binomial", maxit=1000)

# Use nlminb to optimize the parameters
modelUR <- function(par,data){
  a = par[1]
  b = par[2]
  y = data[,1]
  x = data[,2]
  #z = data[1]+data[2]*y
  z = a + b*y
  pz = 1/(1+exp(-(z)))
  #return(sum(-log(1-x-pz)^2))
  return(-sum((log(pz)*x)+(log(1-pz)*(1-x))))
}

modelR <- function(par,data){
  a = par[1]
  b = par[2]
  y = data[,1]
  x = data[,2]
  #z = data[1]+data[2]*y
  z = a 
  pz = 1/(1+exp(-(z)))
  #return(sum(-log(1-x-pz)^2))
  return(-sum((log(pz)*x)+(log(1-pz)*(1-x))))
}


urMod =nlminb(start = c(0.1,0.1), objective = modelUR, d = data.frame(y, x), control = list(eval.max=5000, iter.max=5000))
rMod =nlminb(start = c(0.1,0.1), objective = modelR, d = data.frame(y, x), control = list(eval.max=5000, iter.max=5000))

lrt = 2*(-(urMod$objective)-(-rMod$objective)) ; lrt

pval = 1- pchisq(lrt,df = 1); pval