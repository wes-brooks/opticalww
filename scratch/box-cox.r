box.cox <- function(x, parms=c(1,0)) {
    lambda <- parms[1]
    offset <- parms[2]
    if (lambda==0) log(x+offset) else ((x+offset)^lambda - 1)/lambda
}

bcreg = function(lambda, y, x) {
    resp = box.cox(y, c(lambda,0))
    return(logLik(lm(resp~x)))
}

threepoint <- function(x, y, ladder=c(1, 1/2, 1/3, 0, -1/2, -1)) {
    # x and y are length-three samples from a dataset.
    dx <- diff(x)
    f <- function(parms) (diff(diff(box.cox(y, parms)) / dx))^2
    fit <- nlm(f, c(1,0))
    parms <- fit$estimate #$
    lambda <- ladder[which.min(abs(parms[1] - ladder))]
    if (lambda==0) offset = 0 else {
        do <- diff(range(y))
        offset <- optimize(function(x) f(c(lambda, x)), 
                           c(max(-min(x), parms[2]-do), parms[2]+do))$minimum    
    }
    c(lambda, offset)
}


optimize(bcreg, interval=c(-2,2), y=dat[,2], x=dat[,1], maximum=TRUE)

dat = absorb2[[g]][['spectrum']]
n <- nrow(dat)
i3 <- c(2, floor((n+1)/2), n-1)
parms <- threepoint(dat[,1], dat[,2])
y <- box.cox(pressure, parms)