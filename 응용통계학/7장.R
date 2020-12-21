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

# ex 7.4 
n <- 20
alpha <- 0.05
x <- rnorm(n, mean = 0, sd = 2)
UCL <- (n-1)*var(x)/qchisq(alpha, df = n-1)



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


# ex 7.6

n <- 20
alpha <- 0.05
m <- 1000
UCL <- numeric(m)
for (i in 1:m){
        x <- rchisq(n, df = 2)
        UCL[i] <- (n-1)*var(x)/qchisq(alpha, df = n-1)
}
sum(UCL>4)
mean(UCL>4)


# ex 7.7

n <- 20
alpha <- 0.05
mu0 <- 500
sigma <- 100
m <- 10000

p <- numeric(m)
for (j in 1:m) {
        x <- rnorm(n, mu0, sigma)
        ttest <- t.test(x, alternative = 'greater', mu = mu0)
        p[j] <- ttest$p.value
}
p
p.hat <- mean(p<alpha)
se.hat <- sqrt(p.hat*(1-p.hat)/m)
print(c(p.hat, se.hat))



# ex 7.8 

n <- c(10, 20, 30, 50, 100, 500)
cv <- qnorm(0.975, 0, sqrt(6/n)) # 지정된 확률에서의 검정통계량 값 
cv

sk <- function(x){
        xbar <- mean(x)
        m3 <- mean((x-xbar)^3)
        m2 <- mean((x-xbar)^2)
        return(m3/m2^1.5)
}

p.reject <- numeric(length(n))
m <- 10000

for (i in 1:length(n)) {
        sktests <- numeric(m)
        for (j in 1:m) {
                x <- rnorm(n[i])
                sktests[j] <- as.integer(abs(sk(x))>=cv[i])
        }
        p.reject[i] <- mean(sktests)
}
p.reject


cv <- qnorm(0.975, 0, sqrt(6*(n-2)/((n+1)*(n+3))))
round(cv, 4)


# ex 7.9
n <- 20
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10))
M <- length(mu)
power <- numeric(M)
pvalues <- numeric(m)
for (i in 1:M) {
        mu1 <- mu[i]
        for (j in 1:m) {
                x <- rnorm(n, mean = mu1, sd = sigma)
                ttest <- t.test(x, alternative = 'greater', mu = mu0)
                pvalues[j] <- ttest$p.value
        }
        power[i] <- mean(pvalues<=0.05)
}
se <- sqrt(power*(1-power)/m)
se

library(tidyverse)
theme_set(theme_bw())
df <- data.frame(mean = mu, power = power, upper = power + 2*se, lower = power-2*se)
df %>% 
        ggplot(aes(x=mean, y=power))+
        geom_line()+
        geom_vline(xintercept = 500, lty = 2)+
        geom_hline(yintercept = c(0, 0.05), lty = 1:2)+
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, lwd = 1.5)




# pr 7.3 

f <- function(m, n, mu0, sigma){
        mu <- c(seq(450, 650, 10))
        M <- length(mu)
        power <- numeric(M)
        pvalues <- numeric(m)
        for (i in 1:M) {
                mu1 <- mu[i]
                for (j in 1:m) {
                        x <- rnorm(n, mean = mu1, sd = sigma)
                        ttest <- t.test(x, alternative = 'greater', mu = mu0)
                        pvalues[j] <- ttest$p.value
                }
                power[i] <- mean(pvalues<=0.05)
                se <- sqrt(power*(1-power)/m)
        }
        data.frame(mean = mu, power = power, upper = power + 2*se, lower = power-2*se)
}

dat1 <- cbind(f(m=1000, n=10, mu0=500, sigma=100), n=rep(10, 21))
dat2 <- cbind(f(m=1000, n=20, mu0=500, sigma=100), n=rep(20, 21))
dat3 <- cbind(f(m=1000, n=30, mu0=500, sigma=100), n=rep(30, 21))
dat4 <- cbind(f(m=1000, n=40, mu0=500, sigma=100), n=rep(40, 21))
dat5 <- cbind(f(m=1000, n=50, mu0=500, sigma=100), n=rep(50, 21))

dat <- rbind(dat1, dat2, dat3, dat4, dat5)
dat

dat %>% 
        ggplot(aes(x=mean, y=power, group = n, color = n))+
        geom_line()+
        geom_vline(xintercept = 500, lty = 2)+
        geom_hline(yintercept = c(0, 0.05), lty = 1:2)+
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, lwd = 1.5)



# pr 7.6

n <- 20
m <- 1000

CL1 <- numeric(m)
CL2 <- numeric(m)

for (i in 1:m) {
        x <- rchisq(n, df = 2)
        CL1[i] <- mean(x)-qt(0.975, df = n-1)*sd(x)/sqrt(n)
        CL2[i] <- mean(x)-qt(0.025, df = n-1)*sd(x)/sqrt(n)
}
mean(CL1<2&CL2>2)
