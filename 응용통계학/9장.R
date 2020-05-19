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

A <- boot.array(boot.out)
A
theta.b <- boot.out$t
n <- NROW(patch)
jack.se <- numeric(n)

for (i in 1:n) {
        keep <- which(A[, i]==0)
        jack.se[i] <- sd(theta.b[keep])
}

se.bar <- mean(jack.se)
se.se <- sqrt((n-1)*mean((jack.se-se.bar)^2))
