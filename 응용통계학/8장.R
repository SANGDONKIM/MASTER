library(bootstrap)

# ex 8.2 

head(law)
cor(law$LSAT, law$GPA)
cor(law82$LSAT, law82$GPA)

B <- 200
n <- nrow(law)
R <- numeric(B)

for (b in 1:B) {
        i <- sample(1:n, size = n, replace = T)
        LSAT <- law$LSAT[i]
        GPA <- law$GPA[i]
        R[b] <- cor(LSAT, GPA) # bootstrap 샘플 각각에 대해 cor을 구함. 총 200개 
}
se.R <- sd(R)
hist(R, prob = T)


# ex 8.3 
library(boot)

r <- function(x, i){
        cor(x[i,1], x[i,2])
}
obj <- boot(data = law, statistic = r, R = 2000)
obj
y <- obj$t
sd(y)


# ex 8.4 

theta.hat <- cor(law$LSAT, law$GPA)

B <- 2000
n <- nrow(law)
theta.b <- numeric(B)

for (b in 1:B) {
        i <- sample(1:n, size = n, replace = TRUE)
        LSAT <- law$LSAT[i]
        GPA <- law$GPA[i]
        theta.b[b] <- cor(LSAT, GPA)
}
bias <- mean(theta.b-theta.hat) # 2000개의 bootstrap 추정치에서 데이터의 추정치를 뺀 bias를 평균 
bias



# ex 8.5

head(patch)
n <- nrow(patch)
B <- 2000
theta.b <- numeric(B)
theta.hat <- mean(patch$y)/mean(patch$z)

for (b in 1:B) {
        i <- sample(1:n, size = n, replace = T)
        y <- patch$y[i]
        z <- patch$z[i]
        theta.b[b] <- mean(y)/mean(z)
}
bias <- mean(theta.b)
se <- sd(theta.b)
abs(bias)/se



# ex 8.6 
head(patch)
n <- nrow(patch)
y <- patch$y
z <- patch$z
theta.hat <- mean(y)/mean(z)
theta.hat

theta.jack <- numeric(n)
for (i in 1:n) {
        theta.jack[i] <- mean(y[-i]/mean(z[-i]))
}
bias <- (n-1)*(mean(theta.jack)-theta.hat)
bias


# ex 8.7

se <- sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
se


# ex 8.8 
n <- 10
x <- sample(1:100, size = n)
M <- numeric(n)

for (i in 1:n) {
        y <- x[-i]
        M[i] <- median(y)
}

Mbar <- mean(M)
sqrt((n-1)/n*sum((M-Mbar)^2))


# bootstrap estimate of se 
Mb <- numeric(1000)
for (i in 1:1000) {
        y <- sample(x, size = n, replace = T)
        Mb[i] <- median(y)
}
sd(Mb)












# pr 8.1

library(bootstrap)

n <- nrow(law)
B <- 2000
theta.b <- numeric(B)
theta.hat <- cor(law$LSAT, law$GPA)

for (b in 1:B) {
        i <- sample(1:n, size = n, replace = T)
        LSAT <- law$LSAT[-i]
        GPA <- law$GPA[-i]
        theta.b[b] <- cor(LSAT, GPA) 
}
bias <- mean(theta.b-theta.hat)
se <- sd(theta.b)

bias
se

# pr 8.4
library(boot)
head(aircondit)
str(aircondit)


n <- nrow(aircondit)
B <- 2000
theta.b <- numeric(B)
theta.hat <- mean(aircondit$hours)

for (b in 1:B) {
        i <- sample(1:n, size = n, replace = T)
        x <- aircondit$hours[i]
        theta.b[b] <- mean(x) 
}
bias <- mean(theta.b-theta.hat)
se <- sd(theta.b)

bias
se


# ex 8.9 
library(boot)
library(bootstrap)

theta.boot <- function(dat, ind)
        { 
        y <- dat[ind, 1]
        z <- dat[ind, 2]
        mean(y)/mean(z)
}

y <- patch$y
z <- patch$z
dat <- cbind(y, z)

boot.obj <- boot(dat, statistic = theta.boot, R = 2000)
boot.obj

boot.ci(boot.obj, type = c('basic', 'norm', 'perc'))

# bootstrap confidence intervals 
alpha <- c(0.025, 0.975)

# normal
boot.obj$t0+qnorm(alpha)*sd(boot.obj$t)
# basic
2*boot.obj$t0-quantile(boot.obj$t, rev(alpha), type = 1)

# percentile 
quantile(boot.obj$t, alpha, type = 6)


# ex 8.10 

library(boot)
boot.obj <- boot(law, R = 2000, statistic = function(x, i){cor(x[i, 1], x[i, 2])})
boot.ci(boot.obj, type = c('basic', 'norm', 'perc'))
# percentile, normal CI는 표본분포가 근사적으로 정규분포일 때 비슷해진다. 


# ex 8.11

boot.t.ci <- function(x, B = 500, R = 100, level = 0.95, statistic){
        x <- as.matrix(x)
        n <- nrow(x)
        stat <- numeric(B)
        se <- numeric(B)
        
        boot.se <- function(x, R, f){
                x <- as.matrix(x)
                m <- nrow(x)
                th <- numeric(R)
                for (i in 1:R) {
                        i <- sample(1:m, size = m, replace = T)
                        th[i] <- f(x[i, ])
                }
                return(sd(th))
        }
        
        for (b in 1:B) {
                j <- sample(1:n, size = n, replace = TRUE)
                y <- x[j, ]
                stat[b] <- statistic(y)
                se[b] <- boot.se(y, R=R, f = statistic)
        }
        stat0 <- statistic(x)
        t.stats <- (stat-stat0)/se
        se0 <- sd(stat)
        alpha <- 1-level
        Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
        names(Qt) <- rev(names(Qt))
        CI <- rev(stat0 - Qt*se0) # rev : 벡터를 거꾸로 뒤집음 
}


# ex 8.12 
dat <- cbind(patch$y, patch$z)
stat <- function(dat){
        mean(dat[, 1]) / mean(dat[, 2])
}
ci <- boot.t.ci(dat, statistic = stat, B = 2000, R = 200)
ci



# 8.13 



