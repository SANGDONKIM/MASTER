# permutation test 
# https://www.jwilber.me/permutationtest/
# ex 10.1 

attach(chickwts)
head(chickwts)

x <- sort(weight[feed == 'soybean']) # feed = 'soybean' 인 weight 값
y <- sort(weight[feed == 'linseed']) # feed = 'linseed'인 weight 값

detach(chickwts)


R <- 999
z <- c(x, y)
K <- 1:26

reps <- numeric(R)
t0 <- t.test(x, y)$statistic # original 

for (i in 1:R) {
        k <- sample(K, size = 14, replace = F)
        x1 <- z[k]
        y1 <- z[-k]
        reps[i] <- t.test(x1, y1)$statistic
}
# head(reps)
# tail(c(reps, t0)) # vector 합치기 
p <- mean(c(t0, reps)>=t0) # alpha = 0.05 에서 기각 x 
p

hist(reps, main = '', freq = F, xlab = 'T (p = 0.202)', breaks = 'scott')
points(t0, 0, cex = 1, pch = 16)



# ex 10.2

R <- 999
z <- c(x, y)
K <- 1:26
D <- numeric(R)

options(warn = -1)
D0 <- ks.test(x, y, exact = F)$statistic

for (i in 1:R) {
        k <- sample(K, size = 14, replace = F)
        x1 <- z[k]
        y1 <- z[-k]
        D[i] <- ks.test(x1, y1, exact = F)$statistic
}

p <- mean(c(D0, D)>=D0)
options(warn = 0)

p

hist(D, main = '', freq = F, xlab = 'D (p = 0.46)', breaks = 'scott')
points(D0, 0, cex = 1, pch = 16)


# ex 10.3 

attach(chickwts)
head(chickwts)

x <- sort(weight[feed == 'sunflower']) # feed = 'soybean' 인 weight 값
y <- sort(weight[feed == 'linseed']) # feed = 'linseed'인 weight 값

detach(chickwts)

summary(cbind(x, y))


R <- 999
z <- c(x, y)
K <- 1:26
D <- numeric(R)

options(warn = -1)
D0 <- ks.test(x, y, exact = F)$statistic

for (i in 1:R) {
        k <- sample(K, size = 14, replace = F)
        x1 <- z[k]
        y1 <- z[-k]
        D[i] <- ks.test(x1, y1, exact = F)$statistic
}

p <- mean(c(D0, D)>=D0)
options(warn = 0)

p


# pr 10.3 

attach(chickwts)
head(chickwts)

x1 <- sort(weight[feed == 'soybean']) # feed = 'soybean' 인 weight 값
x2 <- sort(weight[feed == 'linseed']) # feed = 'linseed'인 weight 값
x3 <- sort(weight[feed == 'sunflower'])

detach(chickwts)


# cramer von Mises test 

stat <- function(x, y){
        x <- sort(x)
        y <- sort(y)
        z <- c(x, y)
        
        zrank <- rank(z, ties.method = 'random')
        n <- length(x)
        m <- length(y)
        r <- zrank[1:n]
        s <- zrank[(n+1):(n+m)]
        i <- 1:n
        j <- 1:m
        U <- n*sum((r-i)^2) + m*sum((s-j)^2)
        return (U/(n*m*(n+m))-(4*m*n-1)/(6*(n+m)))
        
}


R <- 999
K <- 1:26
z <- c(x1, x2)

t0 <- stat(x1, x2)
t0
n <- length(x1)
m <- length(x2)

reps <- numeric(R)
for (i in 1:R) {
        k <- sample(K, size = 14, replace = F) # 26개 중에 14개 비복원 
        dat <- z[k]
        x1 <- dat[1:n]
        y1 <- dat[(n+1):m]
        reps[i] <- stat(x1, y1)
}


p <- (sum(c(t0, reps)>=t0)+1)/(R+1)  
p


