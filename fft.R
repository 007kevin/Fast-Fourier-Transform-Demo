library(polynom)

# Test values
A <- polynomial(c(2,0,0,17))
B <- polynomial(c(1,1,2,3,5,8))

fft <- function(A,B) {
    a = coef(A)
    b = coef(B)
    n = 2^ceiling(log(max(length(a),length(b))*2,base=2))
    a = append(a,rep(0,n-length(a)))
    b = append(b,rep(0,n-length(b)))
    c = idft(dft(a)*dft(b))/n
    return(polynomial(round(Re(c))))
}

dft <- function(c) {
    n = length(c)
    if (n == 1)
        return(c)
    wn = exp(2*pi*(1i)/n)
    w = 1
    y0 = dft(c[c(TRUE,FALSE)]) # extract even indexed coefficients
    y1 = dft(c[c(FALSE,TRUE)]) # extract odd indexed coefficients
    y = c(rep(0,n))
    for (k in 1:(n/2)){
        y[k] = y0[k]+w*y1[k]
        y[k+n/2] = y0[k]-w*y1[k]
        w = w*wn
    }
    return(y)
}

idft <- function(y) {
    n = length(y)
    if (n == 1)
        return(y)
    wn = 1/exp(2*pi*(1i)/n)
    w = 1
    c0 = idft(y[c(TRUE,FALSE)]) # extract even indexed coefficients
    c1 = idft(y[c(FALSE,TRUE)]) # extract odd indexed coefficients
    c = c(rep(0,n))
    for (k in 1:(n/2)){
        c[k] = c0[k]+w*c1[k]
        c[k+n/2] = c0[k]-w*c1[k]
        w = w*wn
    }
    return(c)
}
