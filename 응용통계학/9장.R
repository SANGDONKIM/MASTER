library(boot)
library(bootstrap)
patch

set.seed(1111)

theta.boot <- function(patch, i){
        y <- patch[i, 'y']
        z <- patch[i, 'z']
        mean(y)/mean(z)
}

boot.out <- boot(patch, statistic = theta.boot, R = 2000)

A <- boot.array(boot.out);A # boostrap index 
theta.b <- boot.out$t; theta.b # se 2000개 
n <- NROW(patch); n # 벡터의 길이 계산. nrow() : matrix, dataframe 만 적용 가능 
jack.se <- numeric(n)

for (i in 1:n) {
        keep <- which(A[, i]==0)
        jack.se[i] <- sd(theta.b[keep]) # 붓스트랩 샘플의 i번째 케이스를 제외한 se 계산 
}

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
theta.b <- boot.obj$t; theta.b # se 2000개 
se.boot <- sd(theta.b); se.boot # se.se
sample.freq <- boot.array(boot.obj);sample.freq # boot sample index 
se.se.reps <- numeric(n)

for (i in 1:n) {
        keep <- which(sample.freq[, i]==0)
        se.se.reps[i] <- sd(theta.b[keep]) # boot sample 2000 개 각각에서 i번째 관측치를 0으로 만들고 계산 
}

boot.obj
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

x <- ironslag$chemical
y <- ironslag$magnetic

m <- 2000
n <- NROW(x) # 벡터의 길이

L1 <- lm(y~x)
b0 <- L1$coefficients[1]
b1 <- L1$coefficients[2]

# run bootstrap of cases 

out <- replicate(m, expr= {
        i <- sample(1:n, replace = TRUE, size = n)
        xstar <- x[i]
        ystar <- y[i]
        Lb <- lm(ystar ~ xstar)
        s <- summary(Lb)$sigma
        c(Lb$coeff[1], slope = Lb$coeff[2], s=s)
        
})

dim(out)

bootcase <- t(out)
meancase <- colMeans(bootcase)
sdcase <- apply(bootcase, 2, 'sd')

meancase
sdcase

biasint <- mean(bootcase[,1]-b0)
biasslope <- mean(bootcase[, 2]-b1)

rbind(estimate = c(b0, b1), bias = c(biasint, biasslope), se = sdcase[1:2], cv = c(biasint, cv=biasslope/sdcase[1:2]))



# ex 9.6
library(boot)
m <- 2000
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

