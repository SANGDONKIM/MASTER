# rejection sampling 

# ex 3.7
set.seed(1)
a <- 2
b <- 2
plot(density(rbeta(100, a,b)))
max(rbeta(100, a,b))
min(rbeta(100, a,b))

y_grid <- seq(0,1, length.out = 5)
y_beta <- dbeta(y_grid, a, b)
plot(y_grid, y_beta, ylab = 'density', xlab = 'y', type = 'l')
plot(density(rbeta(10, 2, 2)))



# beta(2,2)에서 sampling이 어렵다고 가정한다. 즉 rbeta 함수를 사용할 수 없다. 
# 하지만 분포의 형태는 알고 있다. 

# Assume y grid [0,1]
set.seed(1)
a <- 2
b <- 2

y_grid <- seq(0,1, length.out = 1200)
y_beta <- dbeta(y_grid, a, b)

plot(y_grid, y_beta, type = "l", ylab = "density", xlab = "y", main = "Red line is nqy's density")

nqy <- function(y) {
        constant_n <- 1.5 
        return(constant_n * dunif(y, min = 0, max = 1)) 
}
# nqy(y_grid) : 1.5로 1200개
points(y_grid, nqy(y_grid), type = "l", col = "red")


number_sampling <- 1000
set.seed(12345)
sample_nqy <- sample(y_grid, number_sampling, replace = T)  
sampling <- nqy(sample_nqy) * runif(number_sampling, min = 0, max = 1)

max(sampling)

plot(sample_nqy, sampling, main = "Red line is nqy, blue line is target (beta)", 
     xlab = "y")
points(y_grid, nqy(y_grid), type = "l", col = "red", lwd = 3)
points(y_grid, y_beta, type = "l", col = "blue", lwd = 3)

max(dbeta(sample_nqy, a, b))
final_sample <- sample_nqy[sampling <= dbeta(sample_nqy, a, b)] 

hist(final_sample, freq = F, breaks = 50, xlab = "y", main = "Red line is density (dbeta)")
points(y_grid, y_beta, type = "l", xlab = "y", col = "red")



mu <- 3
sigma <- 1.5
y_grid <- seq(-3, 9, length.out = 1201)
y_norm <- dnorm(y_grid, mean = mu, sd = sigma) # dnorm(x0) : x0일 때 확률밀도 계산 
plot(y_grid, y_norm, type = 'l', ylab = 'density', xlab = 'y')

set.seed(12345)
y_sample <- rnorm(1e+05, mu, sigma) # 1e+05 : 10^5
hist(y_sample, freq = F, breaks = 50, xlab = 'y', main = 'red line is dnorm')
points(y_grid, y_norm, type = 'l', xlab = 'y', col = 'red')


# rejection sampling의 기본적인 아이디어는 어떤 샘플링 하기 쉽고 알고 싶은 분포를 
# 모두 포함하는 샘플링 공간을 정의하고 (nqy), 
# 우리가 샘플하고 싶은 pdf를 기준으로 밖에 위치하는 샘플을 버리는 것이다. 



# 정규분포에서 sampling이 어렵다고 가정한다. 

mu <- 3
sigma <- 1.5

# Assume y grid [-3,9]
y_grid <- seq(-3, 9, length.out = 1201)
y_grid

y_norm <- dnorm(y_grid, mean = mu, sd = sigma) # dnorm(x0) : x0일 때 확률밀도 계산 

# to fill out we will define constant*q(y) function . say nqy() n*qy should
# include all density curve of the normal. I will use uniform distribution
nqy <- function(y) {
        # since the normal uniform distribution doesn't include the top density
        # curve of normal graph.  we need to multiply (constant_n) to make the
        # uniform density include all the normal density.
        # Y~U(-3,9), f(y)=1/12이므로 높이를 0.28로 맞추기 위해서 12x0.28을 곱해서 맞춰줌
        constant_n <- 0.28/(1/(9 - (-3))) 
        return(constant_n * dunif(y, min = -3, max = 9)) 
        # 정규분포를 다 덮는 uniform 분포를 생성하기 위한 함수 
}
# dunif(y_grid, min = -3, max = 9)
plot(y_grid, y_norm, type = 'l', ylab = "density", xlab = "y", ylim = c(0, 0.3), 
     main = "Red line is nqy's density")
points(y_grid, nqy(y_grid), type = "l", col = "red")


number_sampling <- 10000
set.seed(12345)

# sample y based on nqy (actually uniform)
sample_nqy <- sample(y_grid, number_sampling, replace = T)  
sample_nqy
# U(0,1)에서 난수를 발생시키고 c=0.28을 곱해서 높이를 맞춰준다. 
sampling <- nqy(sample_nqy) * runif(number_sampling, min = 0, max = 1)



plot(sample_nqy, sampling, main = "Red line is nqy, blue line is target (normal)", 
     xlab = "y")
points(y_grid, nqy(y_grid), type = "l", col = "red", lwd = 3)
points(y_grid, y_norm, type = "l", col = "blue", lwd = 3)



final_sample <- sample_nqy[sampling <= dnorm(sample_nqy, mu, sigma)] 
# y축 0에서  0.28까지 10000의 점을 찍었을 때 
hist(final_sample, freq = F, breaks = 50, xlab = "y", main = "Red line is density (dnorm)")
points(y_grid, y_norm, type = "l", xlab = "y", col = "red")





# # ex 4.1 

lambda <- 2
t0 <- 3

set.seed(1234)
Tn <- rexp(100, lambda) # lambda = 2인 지수분포에서 표본을 100번 뽑음. 즉 iid X1-X100   

# Tn
# rexp(1,lambda)
# plot(density(Tn))



# plot(density(Sn)) 
Sn <- cumsum(Tn) # Sn = X1, X1+X2, X1+X2+X3,... X1+...+X100 : 이렇게 형성된 100개 중에서 X? 몇번째부터 t0를 넘는지 구해야함  
# Sn

n <- min(which(Sn>t0)) # n번째 이메일이 도착한 시간이 처음 to 시점을 넘을 사건 

n-1 # 7번째 메일이 도착한 시간까지는 to 시점 안에 있음 
round(Sn[1:n], 4) # T1=X1, T2=X1+X2, ... , T7=X1+..+X7 

# 이러한 과정을 반복해서 N(3)=n-1을 계속 구하면 E(N(3))=6에 근사한다.  
 


lambda <- 2
t0 <- 3

set.seed(1234)
Tn <- rexp(1000, lambda)   
Tn
Sn <- cumsum(Tn)
n <- min(which(Sn>t0))
n-1
round(Sn[1:n], 4)




# ex 4.2 (조건부 분포 이용)

lambda <- 2
t0 <- 3
upper <- 100 # t0
pp <- numeric(10000)

for (i in 1:10000) {
        # N=240
        N <- rpois(1, lambda*upper) # N~Pois(200)에서 표본 1번 추출 : 240 
        # print(N)
        Un <- runif(N, 0, upper) # Un = iid U1,..U240 ~ U(0, 100) : unordered arrival time 
        Sn <- sort(Un) # arrival times 
        n <- min(which(Sn>t0)) # t0 시점을 처음 넘었을 때 n의 값 
        pp[i] <- n-1
}

c(mean(pp), var(pp))





# 연습문제 4.2 
# X(10)의 평균과 분산을 추정하라. 
# hint : E[X(t)]=lambdaE[Y1]
# lambda = 2로 가정 


alpha <- 1
beta <- 1
lambda <- 2
t0 <- 10
n <- numeric(1000)
Y <- numeric(1000)
for (i in 1:1000) {
        Yn <- rexp(1000, lambda)
        Xn <- cumsum(Yn)
        n[i] <- max(which(Xn<=t0))
        Y[i] <-sum(rgamma(n[i], shape = alpha, rate = beta)) 
}
Y
c(mean(Y), alpha*lambda*t0)         
c(var(Y), lambda*t0*(alpha/beta^2+(alpha/beta)^2))        



# ex 4.3 (Nonhomogeneous poisson process)

set.seed(1234)
lambda <- 3
upper <- 100

N <- rpois(1, lambda*upper) # 특정시간에 발생 횟수 N 

Tn <- rexp(N, lambda) # 각 발생횟수 구간 별 시간의 분포 : Exp(3)
Sn <- cumsum(Tn) # N번의 사건이 발생할 때까지 걸린시간의 분포 : Gamma(N,3)
Un <- runif(N)

keep <- (Un <= cos(Sn)^2)
Sn[keep] # ordered arrival time

round(Sn[keep], 4)

sum(Sn[keep] <= 2*pi)
table(keep)/N

# 연습문제 4.3 

lambda <- 3
upper <- 100


Y <- numeric(1000)
for (i in 1:1000) {
        N <- rpois(1, lambda*upper) # 특정시간에 발생 횟수 N 
        
        Tn <- rexp(N, lambda) 
        Sn <- cumsum(Tn) 
        Un <- runif(N)
        
        keep <- (Un <= (2*Sn+2)/lambda)
        Y[i] <-  sum(Sn[keep]<=5 & Sn[keep]>4)
        
}

mean(Y)
var(Y)


# ex 4.5
n <- 400
incr <- sample(c(-1, 1), size = n, replace = T)
S <- as.integer(c(0, cumsum(incr)))
plot(0:n, S, type = 'l', main = '', xlab = 'i')
which(S==0)



# 연습문제 4.1 (gambler's ruin)

# 참고 문제 
a <- c(1, 2, 3, 4, 5, 6)
a<2|a>5
a<2||a>5 # 벡터의 첫번째 결과만 출력 


N <- 20
p <- 1/2
sample(50:100, 1)

t <- sample(50:100, 1)



N <- 20
p <- 1/2

x <- rep(0,1000);
x[1] <- 10

for (i in 2:1000) {
        if (x[i-1]==0||x[i-1]==N) {
                x[i] <- x[i-1]
        }
        else{
                x[i] <- x[i-1]+sample(c(1,-1), 1, prob=c(p, 1-p))
                
        }
}

plot(x, type = 'l', ylim = c(0, N), ylab = 'dollers')



A <- 10
p <- 1/2
x <- numeric(1000)
x[1] <- A
for (i in 2:1000) {
        incr <- sample(c(-1,1), size = 1, prob=c(p, 1-p))
        x[i] <- incr
        A <- sum(x)
        if (isTRUE(all.equal(A, 20))) break # isTRUE(x): x가 TRUE와 같은지 
        if (isTRUE(all.equal(A,0))) break
}
x <- cumsum(x[1:i])
plot(x, type = 'l', ylim = c(0, 20))
