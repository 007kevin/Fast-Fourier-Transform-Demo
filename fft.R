library(polynom)

fft <- function(A,B) {
    a = coef(A)
    b = coef(B)
    n = 2^ceiling(log(max(length(a),length(b))*2,base=2))
    a = append(a,rep(0,n-length(a)))
    b = append(b,rep(0,n-length(b)))
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
    
}
