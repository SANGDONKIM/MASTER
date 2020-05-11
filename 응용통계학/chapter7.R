# 7장 


# point estimate

# ex 7.1 

m <- 1000
g <- numeric(m)
for (i in 1:m) {
        x <- rnorm(2)
        g[i] <- abs(x[1]-x[2])
}
est <- mean(g)
sqrt(sum((g-mean(g))^2))/m



# ex 7.2 
# mean
n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
        x <- sort(rnorm(n))
        tmean[i] <- sum(x[2:(n-1)])/(n-2) # theta hat 
}
mse <- mean(tmean^2)
mse
sqrt(sum((tmean-mean(tmean))^2))/m


# median 
n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
        x <- sort(rnorm(n))
        tmean[i] <- median(x)
}
mse <- mean(tmean^2)
mse

# ex 7.3
# contaminated normal distribution 

n <- 20
K <- n/2 - 1
m <- 1000
mse <- matrix(0, n/2, 6)


trimmed.mse <- function(n, m, k, p) {
        tmean <- numeric(m)
        for (i in 1:m) {
                sigma <- sample(c(1, 10), size = n, replace = T, prob = c(p, 1-p))
                x <- sort(rnorm(n, 0, sigma))
                tmean[i] <- sum(x[(k+1):(n-k)])/(n-2*k)
        }
        mse.est <- mean(tmean^2)
        se.mse <- sqrt(mean((tmean-mean(tmean))^2))/sqrt(m)
        return(c(mse.est, se.mse))
}

for (k in 0:K) {
        mse[k+1, 1:2] <- trimmed.mse(n=n, m=m, k=k, p=1.0)
        mse[k+1, 3:4] <- trimmed.mse(n=n, m=m, k=k, p=0.95)
        mse[k+1, 5:6] <- trimmed.mse(n=n, m=m, k=k, p=0.9)
        
}
mse


# pr 7.1

n <- 20
m <- 1000

f <- function(k) {
        kmean <- numeric(m)
        for (i in 1:m) {
                x <- sort(rcauchy(n))
                kmean[i] <- sum(x[(k+1):(n-k)])/(n-2*k)
                }
        mse.est <- mean(kmean^2)
        se.est <- sqrt(mean((kmean-mean(kmean))^2))/sqrt(m)
        return(c(mse.est, se.est))
}


result <- matrix(0, 9, 3)
for (i in 1:9) {
        result[i,1] <- i
        result[i,c(2,3)] <- f(i)
}

colnames(result) <- c('k', 'mean', 'se')
result



# confidence interval 

# ex 7.5 

n <- 20
alpha <- 0.05
m <- 1000
UCL <- numeric(m)
for (i in 1:m) {
        x <- rnorm(n, mean = 0, sd = 2)
        UCL[i] <- (n-1)*var(x)/qchisq(alpha, df = n-1)
}

sum(UCL>4)
mean(UCL>4)
