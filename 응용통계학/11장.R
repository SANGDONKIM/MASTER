# Metropolis-Hastings sampler 


# ex 11.1

f <- function(x, sigma){
        if (any(x<0)) return(0)
        stopifnot(sigma>0)
        return((x/sigma^2)*exp(-x^2/(2*sigma^2)))
} # 랄리 분포


m <- 10000
sigma <- 4
x <- numeric(m) # theta의 값 10000개 p(theta|x)를 구해야 하므로 
x[1] <- rchisq(1, df = 1) # 제안 분포에서 초기값 생성 
k <- 0
u <- runif(m)

for (i in 2:m) {
        xt <- x[i-1] # theta_t-1
        y <- rchisq(1, df = xt) # 제안 분포 : support가 비슷한 분포가 좋음
        num <- f(y, sigma)*dchisq(xt, df = y) # t 시점 posterior
        den <- f(xt, sigma)*dchisq(y, df = xt) # t-1시점 posterior
        if (u[i] <= num/den) {
                x[i] <- y
        } else {
                x[i] <- xt
                k <- k+1
                }
}
print(k) # reject 갯수 
k/m # 기각 비율이 너무 높음

index <- 3000:10000
y1 <- x[index]
plot(index, y1, type = 'l', main = '', ylab = 'x') # random walk 형태면 mcmc를 실패했다고 볼 수 있음   


# ex 11.2

b <- 2001 # 앞에 2000개를 임의로 버림.  
y <- x[b:m]

a <- ppoints(100); a # 백분위수 계산 
QR <- sigma*sqrt(-2*log(1-a)); QR # 이론적 랄리분포 quantile

Q <- quantile(y, a);Q # 경험적 랄리 분포 quantile

qqplot(QR, Q, main = '', cex = 0.5, xlab = 'Rayleigh Quantiles', ylab = 'Sample Quantiles')
abline(0,1) 

hist(y, breaks = 'scott', main = '', xlab = '', freq = F)
lines(QR, f(QR, 4)) # f(x, sigma) : 이론적 랄리분포 



# pr 11.3

m <- 10000
x <- numeric(m)
set.seed(1)
x[1] <- rnorm(1) # common support를 가지는 제안분포
k <- 0
u <- runif(m)
set.seed(1)
for (i in 2:m) {
        xt <- x[i-1]
        y <- rnorm(1)
        num <- dcauchy(y, location = 0, scale = 1)*dnorm(xt)
        den <- dcauchy(xt, location = 0, scale = 1)*dnorm(y)
        if (u[i] <= num/den) {
                x[i] <- y
        } else {
                x[i] <- xt
                k <- k+1
        }
}
print(k)  


b <- 1001 # 앞에 1000개를 임의로 버림 
y <- x[b:m]
a <- ppoints(10); a # 십분위수 계산 

QR <- qt(a, df = 1); QR # theoretical
Q <- quantile(y, a) # empirical

qqplot(QR, Q, main = '', cex = 0.5, xlab = 'Cauchy Quantiles', ylab = 'Sample Quantiles')
abline(0,1) 



set.seed(1)
m <- 10000
x <- numeric(m)
set.seed(1)
x[1] <- rnorm(1) # common support를 가지는 제안분포
k <- 0
u <- runif(m)


# metropolis sampler (if proposal distribution is symm)
set.seed(1)
for (i in 2:m) {
        xt <- x[i-1]
        y <- rnorm(1)
        num <- dcauchy(y, location = 0, scale = 1)
        den <- dcauchy(xt, location = 0, scale = 1)
        if (u[i] <= num/den) {
                x[i] <- y
        } else {
                x[i] <- xt
                k <- k+1
        }
}
print(k)  





# ex 11.3 

f.mu <- function(x){
        exp(-lbeta(432, 5)+431*log(x)-431*log(1+x)-6*log(1+x))
}

curve(f.mu(x), from = 0, to = 400, xlab = 'hours', ylab = '') # known posterior density

fr <- function(x, y){
        a <- 431*(log(y)-log(x))
        b <- 437*(log(1+x)-log(1+y))
        return(exp(a+b))
}


m <- 10000
x <- numeric(m)
x[1] <- rchisq(1, df = 1)
k <- 0
u <- runif(m)
for (i in 2:m) {
        xt <- x[i-1]
        y <- rchisq(1, df = xt)
        r <- fr(xt, y)*dchisq(xt, df = y)/dchisq(y, df = xt)
        if (u[i]<=r) {
                x[i] <- y
        } else {
                x[i] <- xt
                k <- k+1
        }
}

k

plot(acf(x))

# ex 11.4

m <- 10000
x <- numeric(m)
a <- 4
x[1] <- rlnorm(1) # lognormal 난수 
k <- 0
u <- runif(m)
for (i in 2:m) {
        xt <- x[i-1]
        y <- rgamma(1, shape = a, rate = a/xt)
        r <- fr(xt, y)*dgamma(xt, shape = a, rate = a/y)/dgamma(y, shape = a, rate = a/xt)
        if (u[i]<=r) {
                x[i] <- y
        } else {
                x[i] <- xt
                k <- k+1
        }
}
k/m # reject 비율 

plot(acf(x))





# ex 11.10 (Gibbs sampler: Bivariate distribution)

N <- 5000               
burn <- 1000            
X <- matrix(0, N, 2)    

rho <- -.75             
mu1 <- 0
mu2 <- 2
sigma1 <- 1
sigma2 <- .5
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2


X[1, ] <- c(mu1, mu2) # 초기값

for (i in 2:N) {
        x2 <- X[i-1, 2] # x2가 주어짐
        m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2 # x2가 주어졌을 때 x1 조건부 분포의 평균 
        X[i, 1] <- rnorm(1, m1, s1) # 조건부 분포로 생성된 x1 업데이트 
        x1 <- X[i, 1]
        m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1 # x1이 주어졌을 때 x2 조건부 분포의 평균
        X[i, 2] <- rnorm(1, m2, s2) # 조건부 분포로 생성된 x1 업데이트
}

b <- burn + 1
x <- X[b:N, ] # 1000개 버림

colMeans(x) # 0, 2에 거의 근사 
cov(x)
cor(x) # rho = -0.75에 거의 근사 

plot(x, main = '', cex = 0.5, xlab = bquote(X[1]), ylab = bquote(X[2]), ylim = range(x[, 2]))


# pr 11.9


N <- 10000               
burn <- 2000            
X <- matrix(0, N, 2)    

rho <- 0.9             
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2


X[1, ] <- c(mu1, mu2) # 초기값

for (i in 2:N) {
        x2 <- X[i-1, 2] # x2가 주어짐
        m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2 # x2가 주어졌을 때 x1 조건부 분포의 평균 
        X[i, 1] <- rnorm(1, m1, s1) # 조건부 분포로 생성된 x1 업데이트 
        x1 <- X[i, 1]
        m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1 # x1이 주어졌을 때 x2 조건부 분포의 평균
        X[i, 2] <- rnorm(1, m2, s2) # 조건부 분포로 생성된 x1 업데이트
}

b <- burn + 1
x <- X[b:N, ] # 1000개 버림

plot(x, main = '', cex = 0.5, xlab = bquote(X[1]), ylab = bquote(X[2]), ylim = range(x[, 2]))

X <- data.frame(x=X[,1], y = X[,2])
head(X)

fit <- lm(y~x, X)
par(mfrow = c(2, 2))
plot(fit)


# ex 11.11

Gelman.Rubin <- function(psi){
        psi <- as.matrix(psi)
        n <- ncol(psi)
        k <- nrow(psi)
        
        psi.means <- rowMeans(psi) # 각 체인의 평균
        B <- n*var(psi.means) # 각 체인의 평균의 분산 (between)
        psi.w <- apply(psi, 1, 'var') # 각 체인의 분산 (within)
        W <- mean(psi.w) # 각 체인의 분산의 평균 
        v.hat <- W*(n-1)/n+(B/n) # 
        r.hat <- v.hat/W # G-R stat
        return(r.hat)
}

normal.chain <- function(sigma, N, X1){ # N : length, X1 : 초기값, sigma : normal 표준편차 
        x <- rep(0, N)
        x[1] <- X1
        u <- runif(N)
        
        for (i in 2:N) {
                xt <- x[i-1]
                y <- rnorm(1, xt, sigma)  
                r1 <- dnorm(y, 0, 1)*dnorm(xt, y, sigma)
                r2 <- dnorm(xt, 0, 1)*dnorm(y, xt, sigma)
                r <- r1/r2
                if (u[i]<=r) {
                        x[i] <- y
                } else {
                        x[i] <- xt
                }
        }
        return(x)
}

sigma <- 4
k <- 4
n <- 15000
b <- 1000

x0 <- c(-10, -5, 5, 10) # 극단값으로 초기값 부여  
X <- matrix(0, nrow = k, ncol = n) # 4x15000
for (i in 1:k) {
        X[i, ] <- normal.chain(sigma, n, x0[i]) # 체인 4개 생성 
}

psi <- t(apply(X, 1,cumsum)) # 각 체인의 누적합

for (i in 1:nrow(psi)) {
        psi[i, ] <- psi[i, ]/(1:ncol(psi))
}
print(Gelman.Rubin(psi))


par(mfrow = c(2, 2))
for (i in 1:k) {
        plot(psi[i, (b+1):n], type = 'l', xlab = i, ylab = bquote(psi))
}
par(mfrow = c(1,1))


rhat <- rep(0, n)
for (j in (b+1):n) {
        rhat[j] <- Gelman.Rubin(psi[, 1:j])
}
plot(rhat[(b+1):n], type = 'l', xlab = '', ylab = 'R', )
abline(h = 1.1, lty = 2)



d <- matrix(c(1,3,5,7,9,
              2,4,6,8,10,
              3,5,7,10,11,
              4,5,2,5,7), byrow = T, ncol = 5, nrow = 4)


psi <- t(apply(d, 1, cumsum))
psi
for (i in 1:nrow(psi)) {
        psi[i, ] <- psi[i, ]/(1:ncol(psi))
}
psi

# pr 11.11


normal.chain <- function(sigma, N, X1){ # N : length, X1 : 초기값, sigma : normal 표준편차 
        x <- rep(0, N)
        x[1] <- X1
        u <- runif(N)
        
        for (i in 2:N) {
                xt <- x[i-1]
                y <- rnorm(1, xt, sigma)  
                r1 <- dnorm(y, 0, 1)*dnorm(xt, y, sigma)
                r2 <- dnorm(xt, 0, 1)*dnorm(y, xt, sigma)
                r <- r1/r2
                if (u[i]<=r) {
                        x[i] <- y
                } else {
                        x[i] <- xt
                }
        }
        return(x)
}

sigma <- 4
k <- 4
n <- 15000
b <- 1000

x0 <- c(-10, -5, 5, 10) # 극단값으로 초기값 부여  
X <- matrix(0, nrow = k, ncol = n) # 4x15000
for (i in 1:k) {
        X[i, ] <- normal.chain(sigma, n, x0[i]) # 체인 4개 생성 
}

psi <- t(apply(X, 1,cumsum)) # 각 체인의 누적합

for (i in 1:nrow(psi)) {
        psi[i, ] <- psi[i, ]/(1:ncol(psi))
}

Gelman.Rubin <- function(psi){
        psi <- as.matrix(psi) # 4x15000
        n <- ncol(psi)
        k <- nrow(psi)
        i <- 3
        # psi : 4 x 15000
        while (TRUE) {
                psi_iter <- psi[,(1:i)]
                psi.means <- rowMeans(psi_iter)
                B <- n*var(psi.means)
                psi.w <- apply(psi_iter, 1, 'var')
                W <- mean(psi.w)
                v.hat <- W*(n-1)/n+(B/n)  
                r.hat <- v.hat/W
                if (r.hat<=1.2) {
                        break
                }
                i <- i+1
        }
        return(c(r.hat, i))
}


Gelman.Rubin(psi)


# pr 11.12 

f <- function(x, sigma){
        if (any(x<0)) return(0)
        stopifnot(sigma>0)
        return((x/sigma^2)*exp(-x^2/(2*sigma^2)))
}

chain <- function(sigma, N, X1){ # N : length, X1 : 초기값, sigma : normal 표준편차 
        x <- rep(0, N)
        x[1] <- X1
        u <- runif(N)
        
        for (i in 2:N) {
                xt <- x[i-1]
                y <- rchisq(1, df = xt)  
                r1 <- f(y, sigma)*dchisq(xt, df = y)
                r2 <- f(xt, sigma)*dchisq(y, df = xt)
                r <- r1/r2
                if (u[i]<=r) {
                        x[i] <- y
                } else {
                        x[i] <- xt
                }
        }
        return(x)
}

sigma <- 4
k <- 4
n <- 15000
b <- 1000

x0 <- c(2, 4, 6, 8) # 극단값으로 초기값 부여  
X <- matrix(0, nrow = k, ncol = n) # 4x15000
for (i in 1:k) {
        X[i, ] <- chain(sigma, n, x0[i]) # 체인 4개 생성 
}

psi <- t(apply(X, 1, cumsum)) # 각 체인의 누적합

for (i in 1:nrow(psi)) {
        psi[i, ] <- psi[i, ]/(1:ncol(psi))
}

Gelman.Rubin <- function(psi){
        psi <- as.matrix(psi) # 4x15000
        n <- ncol(psi)
        k <- nrow(psi)
        i <- 3
        # psi : 4 x 15000
        while (TRUE) {
                psi_iter <- psi[,(1:i)]
                psi.means <- rowMeans(psi_iter)
                B <- n*var(psi.means)
                psi.w <- apply(psi_iter, 1, 'var')
                W <- mean(psi.w)
                v.hat <- W*(n-1)/n+(B/n)  
                r.hat <- v.hat/W
                if (r.hat<=1.2) {
                        break
                }
                i <- i+1
        }
        return(c(r.hat, i))
}


Gelman.Rubin(psi)



