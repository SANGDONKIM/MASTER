library(boot)
library(bootstrap)
patch
dim(patch)
set.seed(1111)

theta.boot <- function(patch, i){
        y <- patch[i, 'y']
        z <- patch[i, 'z']
        mean(y)/mean(z)
}


boot.out <- boot(patch, statistic = theta.boot, R = 2000)

A <- boot.array(boot.out);A # boostrap index 
theta.b <- boot.out$t; theta.b # theta 값  
n <- NROW(patch); n # 벡터의 길이 계산. nrow() : matrix, dataframe 만 적용 가능 
jack.se <- numeric(n)

for (i in 1:n) {
        keep <- which(A[, i]==0) # i번 째 데이터가 포함되지 않는 열 위치 
        jack.se[i] <- sd(theta.b[keep]) # 붓스트랩 샘플의 i번째 케이스를 제외한 se 계산 
}

# jack.se : n개 theta(j)에 대해 평균, 표준편차 계산 

se.bar <- mean(jack.se)
se.se <- sqrt((n-1)*mean((jack.se-se.bar)^2))


# ex 9.2 

# initialize 

y <- patch$y
z <- patch$z
dat <- cbind(y, z)
n <- NROW(dat)
B <- 2000

# jackknife after bootstrap step 1 : run to bootstrap
theta_boot <- function(dat, ind){
        y <- dat[ind, 1]
        z <- dat[ind, 2]
        mean(y)/mean(z)
}

boot.obj <- boot(dat, statistic = theta_boot, R = 2000)
theta.hat <- boot.obj$t0; theta.hat # original se
theta.b <- boot.obj$t; theta.b # 각 bootstrap sample 의 theta hat 
se.boot <- sd(theta.b); se.boot 

sample.freq <- boot.array(boot.obj);sample.freq # boot sample index 
se.se.reps <- numeric(n)

for (i in 1:n) {
        keep <- which(sample.freq[, i]==0)
        se.se.reps[i] <- sd(theta.b[keep]) # boot sample 2000 개 각각에서 i번째 관측치를 0으로 만들고 계산 
}

se.bar <- mean(se.se.reps); se.bar
se.se <- sqrt((n-1)*mean((se.se.reps-se.bar)^2))
se.se

round(se.se.reps, 5)


# ex 9.3 

library(tidyverse)
library(DAAG)
theme_set(theme_bw())

L1 <- lm(magnetic~chemical, data = ironslag)
cf3 <- round(L1$coefficients, 3)
cap <- paste('Fit: magnetic =', cf3[1], '+', cf3[2], 'chemical')

ironslag %>% 
        ggplot(aes(x=chemical, magnetic))+
        geom_point()+
        geom_smooth(method = 'lm')+
        ggtitle(cap)
        
plot(L1, which = 1:2)


# ex 9.4
library(MASS)
cor(log(mammals$body), log(mammals$brain))
summary(mammals)

y <- log(mammals$brain)
x <- log(mammals$body)

L <- lm(y~x)

cap <- paste('Fit: log(brain) =', round(L$coeff[1], 3), '+', round(L$coefficients[2], 3), 
             'log(body)')
mammals %>% 
        ggplot(aes(x, y))+
        geom_point()+
        geom_smooth(method = 'lm')+
        labs(x = 'log(body', y='log(brain)', title = cap)

plot(L)


# ex 9.5 

set.seed(1)
x <- ironslag$chemical
y <- ironslag$magnetic

m <- 2000
n <- NROW(x) # 벡터의 길이

L1 <- lm(y~x)
b0 <- L1$coefficients[1] # original
b1 <- L1$coefficients[2] # original

# run bootstrap of cases 
set.seed(1)
out <- replicate(m, expr= {
        i <- sample(1:n, replace = TRUE, size = n)
        xstar <- x[i]
        ystar <- y[i]
        Lb <- lm(ystar ~ xstar)
        s <- summary(Lb)$sigma
        c(Lb$coeff[1], slope = Lb$coeff[2], s=s)
        
})

bootcase <- t(out); head(bootcase)
meancase <- colMeans(bootcase); meancase
sdcase <- apply(bootcase, 2, 'sd'); sdcase

# for loop 이용 
set.seed(1)
out <- matrix(0, nrow = m, ncol = 3)

for (i in 1:m) {
        j <- sample(1:n, replace = TRUE, size = n)
        xstar <- x[j]
        ystar <- y[j]
        Lb <- lm(ystar ~ xstar)
        out[i,1] <- Lb$coeff[1]
        out[i,2] <- Lb$coeff[2]
        out[i,3] <- summary(Lb)$sigma
}

bootcase <- out
colnames(bootcase) <- c('intercept', 'slope.xstar', 's')
dim(bootcase)
head(bootcase)

meancase <- colMeans(bootcase); meancase
sdcase <- apply(bootcase, 2, 'sd'); sdcase


head(bootcase) # dim (2000, 3)

biasint <- mean(bootcase[,1]-b0); biasint # 2000개의 bootstrap b0 추정량 - original b0 추정량
biasslope <- mean(bootcase[, 2]-b1); biasslope # 2000개의 bootstrap b1 추정량 - original b1 추정량

rbind(estimate = c(b0, b1), bias = c(biasint, biasslope), se = sdcase[1:2], cv = c(biasint, cv=biasslope/sdcase[1:2]))



# ex 9.6 (using boot function)

library(boot)
set.seed(1)
stats <- function(dat, i){
        x <- dat$chemical[i]
        y <- dat$magnetic[i]
        Lb <- lm(y~x)
        s <- summary(Lb)$sigma
        c(Lb$coefficients[1], slope = Lb$coefficients[2], s=s)
}

boot.out <- boot(ironslag, statistic = stats, R = 2000)
boot.out
boot.out$t0

head(boot.out$t)

sd(boot.out$t[, 2])

boottbl <- broom::tidy(boot.out)
boottbl
boottbl$std.error[2]


MASS::truehist(boot.out$t[, 2], main = '', xlab = 'slopes') 
# truehist : hist 함수와 비슷함. axes = FALSE를 지정하는 것으로 축 좌표를 그리지 않도록 할 수 있다. 
# 서로 다른 축의 좌표를 여러개 붙일 수 있는 장점이 있음. 
# https://shlee1990.tistory.com/677 참고 

abline(v=boot.out$t0[2], lwd = 2)


boot.ci(boot.out, index = 2, type = c('norm', 'perc', 'basic', 'bca'))
# 각 신뢰구간이 0을 포함하지 않으므로 alpha = 0.05에서 
# chemical과 magnetic 사이에는 선형관계가 있다고 할 수 있다. 


# pr 9.4 

mam <- MASS::mammals
head(mam)

dat <- data.frame(body = log(mam$body), brain = log(mam$brain))
head(dat)

library(boot)

stats <- function(dat, i){
        x <- dat[i, 1]
        y <- dat[i, 2]
        Lb <- lm(y~x)
        s <- summary(Lb)$sigma
        c(Lb$coefficients[1], slope = Lb$coefficients[2], s=s)
}
set.seed(1)
boot.out <- boot(dat, statistic = stats, R = 2000)
boot.out
boot.out$t0

boottbl <- broom::tidy(boot.out)

regstats <- function(dat, i){
        ystar <- dat$yhat[i]+dat$r[i]
        xstar <- dat$x[i]
        Lnew <- lm(ystar~xstar)
        Lnew$coefficients
}

y <- log(mammals$brain)
x <- log(mammals$body)
L <- lm(y~x)
r <- rstandard(L, sd = 1)
r <- r-mean(r)
df <- data.frame(r = r, x = x, yhat = L$fitted.values)
head(df)

set.seed(1)
boot.obj <- boot(data = df, statistic = regstats, R = 2000)


broom::tidy(boot.out)
broom::tidy(boot.obj)




# ex 9.7 
library(MASS)
cor(log(mammals$body), log(mammals$brain))
summary(mammals)

y <- log(mammals$brain)
x <- log(mammals$body)

L <- lm(y~x)

m.resid <- rstandard(L, sd = 1) # modified residual : ei'
r <- m.resid - mean(m.resid) # center the modified residual : ei'-ei bar' 

m <- 1000
n <- NROW(x)

set.seed(1)
estsErr <- matrix(0, nrow = m, ncol = 3)
for (i in 1:m) {
        estar <- sample(r, replace = T, size = n)
        ystar <- L$fitted.values+estar # yhat + ei'-ei bar'. yhat : 고정 
        Lb <- lm(ystar~x)
        
        estsErr[i, 1] <- Lb$coeff[1]
        estsErr[i, 2] <- Lb$coeff[2]
        estsErr[i, 3] <-  summary(Lb)$sigma
}
head(estsErr)

colnames(estsErr) <- c('b0.(Intercept', 'b1.x', 's')


# ex 9.8  
sd(ests[, 2]) # boot se(B1)

s <- summary(L)$sigma
SSx <- (n-1)*var(x)
se.beta1 <- sqrt(s^2/SSx) # reg se(b1)

s*sqrt(1/n+mean(x)^2/SSx)
sd(ests[,1])
 

# summary.lm method 

betas <- summary(L)$coeff; betas # reg se(b0), se(b1)
broom::tidy(summary(L))


# ex 9.9 (model-based resampling method)

regstats <- function(dat, i){
        ystar <- dat$yhat[i]+dat$r[i]
        xstar <- dat$x[i]
        Lnew <- lm(ystar~xstar)
        Lnew$coefficients
}

y <- log(mammals$brain)
x <- log(mammals$body)
L <- lm(y~x)
r <- rstandard(L, sd = 1)
r <- r-mean(r)
df <- data.frame(r = r, x = x, yhat = L$fitted.values)
head(df)

boot.obj <- boot(data = df, statistic = regstats, R = 2000)
broom::tidy(boot.obj)


# ex 9.10 

library(boot)
library(bootstrap)
theta_boot <- function(dat, ind){
        mean(dat[ind, ]$y / mean(dat[ind, ]$z))
}

boot.out <- boot(patch, theta_boot, R = 2000)
infl <- empinf(boot.out, type = 'jack')
theta.hat <- boot.out$t0
jack <- theta.hat-infl/(nrow(patch)-1)

rbind(infl, jack)


# ex 9.11

jack.after.boot(boot.out, useJ = T, stinf = F)

n <- NROW(dat)
J <- numeric(n)

b.freq <- boot.array(boot.obj)
theta.b <- boot.obj$t

for (i in 1:n) {
        keep <- which(b.freq[, i] == 0)
        J[i] <- mean(theta.b[keep])
}

# the jackknife influence values

rbind((1:n), (n-1)*(mean(J)-J))


jack.after.boot(boot.out, useJ = T, stinf = T)


# pr 9.5 

m <- 200


f.boot <- function(dat, i){
        x <- dat[i, 'x']
        y <- dat[i, 'y']
        L <- lm(y~x)
        r <- mean((y-L$fitted.values)^2)
        return(r)
} # bootstap sample 하나당 return이 나오기 때문에 r[i]를 지정해주면 안됨. 

dat <- matrix(rnorm(m*2), m, 2)
colnames(dat) <- c('x', 'y')

boot.out <- boot(dat, f.boot, R = 2000)

boot.out









