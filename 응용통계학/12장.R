

# ex 12.1 (sturge's rule)

set.seed(1)
n <- 1000
x <- rnorm(n)
nclass <- ceiling(1+log2(n)); nclass # bin 갯수, ceiling : x 보다 크거나 같은 정수 
cwidth <- diff(range(x)/nclass) # bin 간격, diff : 관측값에서 직전 관측값을 뺀 함수 

breaks <- min(x)+cwidth*0:nclass; breaks # 간격 별 x 값 
h.default <- hist(x, freq = F, xlab = 'default', main = 'hist:default')
curve(dnorm(x), from = -3, to = 3, add = T)

h.sturges <- hist(x, breaks = breaks, freq = F, main = 'hust : sturges')
curve(dnorm(x), from = -3, to = 3, add = T)

print(h.default$breaks)
print(round(h.sturges$breaks, 1))

cwidth # bin 길이 
body(nclass.Sturges)


print(h.default$density[5]) # X = x일 때 확률 밀도  
print(h.sturges$density[4]) # vk * h, vk = 4, h = cwidth
h.sturges$counts[4]/n*1/cwidth


# ex 12.2 

x0 <- 0.1
b <- which.min(h.default$breaks <= x0) - 1 
print(c(b, h.default$density[b])) # x = 0 - 0.5(8번째 bin) 일 때 확률 밀도 

b <- which.min(h.sturges$breaks <= x0) - 1 # x = 0.09 - 0.71 구간 (6번째 bin)
print(c(b, h.sturges$density[b])) # # x = 0.09 - 0.71(6번째 bin) 일 때 확률 밀도


h.default$counts[7] / (n*0.5) # vk/n*h
h.sturges$counts[6] / (n*cwidth)



# ex 12.3

library(MASS)

head(geyser)
waiting <- geyser$waiting
n <- length(waiting)

h <- 3.5*sd(waiting)*n^(-1/3); h # 3.5*s*n^-1/3 : bin 간격 

m <- min(waiting); m # 43
M <- max(waiting); M # 108

nclass <- ceiling((M-m)/h); nclass # range(x)/h
breaks <- m + h*0:nclass; breaks # 각 구간의 x좌표 

h.scott <- hist(waiting, breaks = breaks, freq = F, main = '')
truehist(waiting, nbins = 'scott', x0 = 0, prob = T, col = 0)
hist(waiting, breaks = 'scott', prob = T, density = 5, add = T)



# ex 12.4
library(MASS)
waiting <- geyser$waiting
h <- 2.15*sd(waiting)*n^(-1/5)

br <- pretty(waiting, diff(range(waiting))/h); br # pretty(x, n = 등간격으로 쪼개는 bin 갯수)
# 7개의 구간으로 나눔 

brplus <- c(min(br)-h, max(br+h)); brplus

histg <- hist(waiting, breaks = br, freq = F, main = '', xlim = brplus)

vx <- histg$mids; vx # bin의 중간값
vy <- histg$density; vy # 롹률 밀도 

delta <- diff(vx)[1] # bin의 중간값끼리 간격 
k <- length(vx)
vx <- vx + delta
vx <- c(vx[1] - 2*delta, vx[1] - delta, vx)
vy <- c(0, vy, 0)

polygon(vx, vy)

fpoly <- approxfun(vx, vy)
integrate(fpoly, lower = min(vx), upper = max(vx))


library(ggplot2)
ggplot(geyser, aes(waiting)) + geom_freqpoly(binsize = h)



# ex 12.7
library(MASS)
waiting <- geyser$waiting
n <- length(waiting)
h1 <- 1.06*sd(waiting)*n^(-1/5)
h2 <- 0.9*min(c(IQR(waiting)/1.34, sd(waiting)))*n^(-1/5)

plot(density(waiting))
density(waiting)

sdK <- density(kernel = 'gaussian', give.Rkern = T)
print(c(sdK, sdK*sd(waiting)))

print(c(sd(waiting), IQR(waiting)))
print(c(h1, h2))



# ex 12.8
n <- length(precip)
h1 <- 1.06*sd(precip)*n^(-1/5)
h2 <- 0.9*min(c(IQR(precip)/1.34, sd(precip)))*n^(-1/5)
h0 <- bw.nrd0(precip) # h2와 동일 

par(mfrow = c(2,2))
plot(density(precip)) # h0
plot(density(precip, bw = h1)) # h1
plot(density(precip, bw = h2)) # h2
plot(density(precip, kernel = 'cosine')) 
par(mfrow = c(1,1))

# ex 12.9
d <- density(precip)
xnew <- seq(0, 70, 10)
approx(d$x, d$y, xout = xnew) # 지정된 수의 근사 함수 값을 반환

fhat <- approxfun(d$x, d$y)
fhat(xnew)


# ex 12.10

set.seed(1)
x <- rexp(1000, 1)
plot(density(x), xlim = c(-1, 6), ylim = c(0, 1), main = '')
abline(v = 0)

y <- seq(0.001, 6, 0.01)
lines(y, dexp(y, 1), lty = 2)


# ex 12.11

xx <- c(x, -x)
g <- density(xx, bw = bw.nrd0(x))
a <- seq(0, 6, 0.01)
ghat <- approx(g$x, g$y, xout = a)
fhat <- 2*ghat$y

bw <- paste('Bandwidth = ', round(g$bw, 5))
plot(a, fhat, type = 'l', xlim = c(-1, 6), ylim = c(0, 1), main = '', xlab = bw, ylab = 'density')
abline(v=0)

y <- seq(0.001, 6, 0.01)
lines(y, dexp(y, 1), lty = 2)



# pr 12.2 

n <- 500
set.seed(1)
x <- rnorm(n)

a <- hist(x, breaks = 'sturge', prob = T, add = T)
b <- hist(x, breaks = 'scott', prob = T, add = T)
c <- hist(x, breaks = 'fd', prob = T, add = T)

f <- dnorm(x)

m <- min(x)
M <- max(x)

h1 <- (M-m)/(1+log2(n))
h2 <- 3.49*sd(x)*n^(-1/3)
h3 <- 2*IQR(x)*n^(-1/3)

sum((f - a$density)^2*h1/a$counts)

num1 <- a$counts

for (i in num1) {
        for (j in 1:) {
                
        }
}


n <- 500
x <- sort(x)
k <- c(nclass.Sturges(x), nclass.scott(x), nclass.FD(x))
R <- diff(range(x))
h <- R/k

br1 <- min(x) + h[1]*0:k[1]
br2 <- min(x) + h[2]*0:k[2]
br3 <- min(x) + h[3]*0:k[3]

hg1 <- hist(x, breaks = br1, plot = F)
hg2 <- hist(x, breaks = br2, plot = F)
hg3 <- hist(x, breaks = br3, plot = F)

bin1 <- rep(1:k[1], hg1$counts)
hg1$counts[bin1]

(f-hg1$density[bin1])^2*h[1]/hg1$counts[bin1]

# pr 12.8

library(gss)
data("buffalo")

par(mar=c(1,1,1,1))
par(mfrow = c(4,2))

for (i in 1:4) {
        plot(density(buffalo, bw = i, kernel = 'gaussian'), main = 'Gaussian')
        plot(density(buffalo, bw = i, kernel = 'biweight'), main = 'biweight')
}



# pr 12.10

x <- precip
xx <- c(x, -x)
g <- density(xx, bw = bw.nrd0(x))

summary(x)
a <- seq(0, 68, 0.01)
ghat <- approx(g$x, g$y, xout = a)
fhat <- 2*ghat$y

bw <- paste('Bandwidth = ', round(g$bw, 5))
plot(a, fhat, type = 'l', xlim = c(-5, 70), main = '', xlab = bw, ylab = 'density')
abline(v=0)
lines(density(x), lty = 3)

plot(density(x, bw = bw.nrd0(x), from = 0), main = '')
plot(density(x, bw = bw.nrd0(x), cut = 0), main = '')




# kernel density estimation example 

# https://medium.com/analytics-vidhya/kernel-density-estimation-kernel-construction-and-bandwidth-optimization-using-maximum-b1dfce127073

# kernel = gaussian

# xi = 65일 때 

x <- c(65, 75, 67, 79, 81, 91)
y <- 50:99  
h <- 5.5
n <- length(y)

B <- numeric(n)
K <- numeric(n)


for (j in 1:length(y)) {
        A <- 1/(h*sqrt(2*pi))
        B[j] <- (-0.5)*((y[j] - 65)/h)^2
        K[j] <- A*exp(B[j])
}
print(B)
K

plot(y, K, type = 'l', main = 'kernel at xi = 65')


# 각 xi 별 kernel plot 
x <- c(65, 75, 67, 79, 81, 91)
y <- 50:99  
h <- 5.5
n <- length(y)
m <- length(x)
B <- matrix(0, nrow = n, ncol = m)
K <- matrix(0, nrow = n, ncol = m)

for (i in 1:length(x)) {
        for (j in 1:length(y)) {
                A <- 1/(h*sqrt(2*pi)*m)
                B[j, i] <- (-0.5)*((y[j] - x[i])/h)^2
                K[j, i] <- A*exp(B[j, i])
        }
}

plot(y, K[,1], type = 'l', main = '', xlim = c(45, 110), ylim = c(0, 0.04))
for (i in 2:6) {
        lines(y, K[,i], type = 'l', main = '')
}


K <- round(K, digit = 7)
d <- rowSums(K)

lines(y, d, type = 'l', main = '')


# p.353

x <- c(-0.77, -0.6, -0.25, 0.14, 0.45, 0.64, 0.65, 1.19, 1.71, 1.74)
y <- seq(-4, 4, 0.1)
n <- NROW(x)
m <- NROW(y)

B <- matrix(0, nrow = m, ncol = n)
K <- matrix(0, nrow = m, ncol = n)

h <- 0.25

for (i in 1:n) {
        for (j in 1:m) {
                A <- 1/(h*sqrt(2*pi)*n)
                B[j, i] <- (-1/2)*((y[j] - x[i])/h)^2
                K[j, i] <- A*exp(B[j, i])
        }
}

plot(y, K[,1], type = 'l', main = 'h = 0.25', ylim = c(0, 0.55), ylab = '', xlab = '')
for (i in 2:10) {
        lines(y, K[,i], type = 'l', main = '')
}


K <- round(K, digit = 7)
d <- rowSums(K)

lines(y, d, type = 'l', main = '')

