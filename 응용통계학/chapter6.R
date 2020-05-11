# blog : https://www.countbayesie.com/blog/2015/3/3/6-amazing-trick-with-monte-carlo-simulations

library(tidyverse)
theme_set(theme_bw())

distplot <- function(n, m, sd, x, y){
        runs <- n
        sims <- rnorm(runs, mean = m, sd = sd)
        sim_data <- data.frame(x=sims, y=rep(0, runs))
        sim_data %>% 
                ggplot(aes(x=x))+
                geom_histogram(aes(y=..density..), alpha=0.3, color = 'black', fill = 'steelblue')+
                geom_density()+
                geom_jitter(aes(y=y), height = 0.001)
        mc.integral1 <- sum(sims<=x)/runs
        mc.integral2 <- sum(sims<=y)/runs
        print(mc.integral1)
        print(mc.integral2)
        print(mc.integral2-mc.integral1)
}

distplot(10000, 1, 10, 3, 6)
pnorm(6, mean = 1, sd = 10)-pnorm(3, mean = 1, sd = 10)





# Simple Monte Carlo Estimator


# ex 6.1 

m <- 10000
x <- runif(m) 
plot(x, exp(-x))

theta.hat <- mean(exp(-x)) # exp(-x) : 높이, 1/m : 밑변
# mean(exp(-x)) : (1/m*e^-x1)+91/m*e^-x2)+...+(1/m*e^-xm) == 1/m*sum(e^x1+..+e^xm) == mean(e^-x) 


print(theta.hat) # theta hat
print(1-exp(-1)) # theta



# ex 6.2 

m <- 10000
x <- runif(m, min = 2, max = 4)
theta.hat <- mean(exp(-x))*2
theta.hat1 <- (1/m)*sum(exp(-x))*2
print(theta.hat)
print(exp(-2)-exp(-4))



# ex 6.3

x <- seq(0.1, 2.5, length = 10)
m <- 10000
u <- runif(m)

cdf <- numeric(length(x))
for (i in 1:length(x)) {
        g <- x[i]*exp(-(u*x[i])^2/2)
        # print(g)
        cdf[i] <- mean(g)/sqrt(2*pi)+0.5
}
cdf


Phi <- pnorm(x) # P(X<=x)
plot(Phi)
print(round(rbind(x, cdf, Phi) ,3))


# pr 6.2

f <- function(x){
        m <- 10000
        u <- runif(m, min = 0, max = x)
        g <- exp(-u^2/2)
        cdf <- ((1/m*sum(g)*x)*1/(sqrt(2*pi)))+0.5
        print(cdf)
}

round(rbind(f(2), pnorm(2, mean = 0, sd = 1)), 3)



# ex 6.4 

x <- seq(0.1, 2.5, length = 10)
m <- 10000
z <- rnorm(m)

dim(x) <- length(x) # apply는 2차원 데이터(array, matrix, data.frame)에 적용 가능 
p <- apply(x, MARGIN = 1, FUN = function(x, z) {mean(z<x)}, z=z) # MARGIN = 1 : 가로방향
p

Phi <-pnorm(x)
print(round(rbind(x, p, Phi), 3))



# ex 6.5 

x <- 2
m <- 10000
z <- rnorm(m)

g <- (z<x) # indicator function 
v <- mean((g-mean(g))^2)/m
cdf <- mean(g)

c(cdf, v)
c(cdf-1.96*sqrt(v), cdf + 1.96*sqrt(v))



# pr 6.3 

# by sampling from U(0, 0.5)

m <- 10000
ex <- rep(0, 10000)

for (i in 1:m) {
        u <- runif(m, min = 0, max = 0.5)
        ex[i] <- 1/m*sum(exp(-u))*1/2
}

c(mean(ex), var(ex))
var1 <- var(ex)

# by sampling from Exponential(1)

x <- 0.5

cdf <- rep(0, m)

for (i in 1:m) {
        z <- rexp(m, rate = 1)
        g <- (z<=x)
        cdf[i]=mean(g)
}

c(mean(cdf), var(cdf))

var2 <- var(cdf)

var1/var2 # var2가 분산이 더 크다




# antithetic variables 

# ex 6.6 

MC.Phi <- function(x, R=10000, antithetic = TRUE) {
        u <- runif(R/2)
        if (!antithetic) v <- runif(R/2) else
                v <- 1-u
        u <- c(u,v)
        cdf <- numeric(length(x))
        for (i in 1:length(x)) {
                g <- x[i]*exp(-(u*x[i])^2/2)
                cdf[i] <- mean(g) / sqrt(2*pi)+0.5
        }
        cdf
}

x <- seq(0.1, 2.5, length=5)
Phi <- pnorm(x)
set.seed(123)
MC1 <- MC.Phi(x, antithetic = FALSE)
set.seed(123)
MC2 <- MC.Phi(x)

print(round(rbind(x, MC1, MC2, Phi), 5))


m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 1.95

for (i in 1:m) {
        MC1[i] <- MC.Phi(x, R=1000, antithetic = FALSE)
        MC2[i] <- MC.Phi(x, R=1000)
}

print(sd(MC1))
print(sd(MC2))

print((var(MC1)-var(MC2))/var(MC1))



# pr 6.10

# without variance reduction

m <- 10000
cdf <- rep(0, m)
for (i in 1:m) {
        u <- runif(m)
        cdf[i] <- (1/m)*sum(exp(-u)/(1+u^2))
}

mean(cdf)
var(cdf)


# variance reduction 

m <- 10000
cdf_vr <- rep(0, m)
for (i in 1:m) {
        u <- runif(m/2)
        v <- 1-u
        u <- c(u, v)
        cdf_vr[i] <- (1/m)*sum(exp(-u)/(1+u^2))
}
mean(cdf_vr)
var(cdf_vr)

print((var(cdf)-var(cdf_vr))/var(cdf))



# control variates 


m <- 10000

theta <- numeric(m)
for (i in 1:m){
        u <- runif(m)
        theta[i] <- 1/m*sum(exp(u))
        
}

mean(theta)
var(theta)



# ex 6.7

m <- 10000
a <- -12+6*(exp(1)-1)
U <- runif(m)
T1 <- exp(U)
T2 <- exp(U)+a*(U-1/2)
mean(T1)
mean(T2)



# ex 6.8




# ex 6.11 

m <- 10000
theta.hat <- se <- numeric(5)
g <- function(x) {
        exp(-x-log(1+x^2))*(x>0)*(x<1)
}

x <- runif(m) # f0
fg <- g(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

theta.hat
se


# pr 6.15
M <- 10000
k <- 5
m <- M/k
si <- numeric(k)
v <- numeric(k)

g <- function(x) {
        exp(-x)/(1+x^2)
}

f <- function(x) {
        (k/(1-exp(-1)))*exp(-x)
}

for (j in 1:k) {
        u <- runif(m, (j-1)/k, j/k)
        x <- -log(1-(1-exp(-1))*u)
        fg <- g(x)/f(x)
        si[j] <- mean(fg)
        v[j] <- var(fg)
        
}
sum(si)
mean(v)
