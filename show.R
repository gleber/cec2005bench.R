library('cec2005benchmark')
library('DEoptim')
library('sfsmisc')

##
## Parameters
##

sys.source("params.R", envir = topenv(.GlobalEnv))

##
## Variables
##

load("results.rdata")
if (!exists("results")) {
  stop("No results!")
}

##
## Main loop
##

## 1. function number
## 2. dimentions, only c(1,30) are used
## 3. run
## 4. function evals barriers
## 5. values
##    1: value
##    2: iterations
##    3: number of f.eval

## results
## table 10 dimensions
##   bigrow for each barrier
##      rows quantiles, mean, std
##   columns
##      function numbers

print(dim(results))

res.rows = 0 # 7*length(dims)*length(fes)
res.cols = length(funcs) + 3
res = matrix(nrow=res.rows, ncol=res.cols)

print(res)
## stop()

biases = t(as.matrix(fbias))[,rep(1,25)]
biases = biases[funcs,]

## for (d in dims) {
for (d in c(10)) {
  indim <- results[funcs,d,,,1]
  for (fe.i in 1:length(fes)) {
    fe <- fes[fe.i]
    vals <- indim[,,fe.i]
    infe <- vals - biases
    infe.s <- apply(infe, 1,
                    function(x) format(c(quantile(x, na.rm=TRUE),mean(x),sd(x)), scientific=TRUE))
                    ## function(x) c(quantile(x, na.rm=TRUE),mean(x),sd(x)))
    rownames(infe.s)[6] <- "avg"
    rownames(infe.s)[7] <- "std"
    ## rownames(infe.s) <- lapply(rownames(infe.s),
    ##                            ## function(x) paste(d, "dim", format(fe, scientific=TRUE), "fes", x))
    ##                            function(x) paste(format(fe, scientific=TRUE), "fes", x))
    r <- cbind(d, fe, rownames(infe.s), infe.s)
    res <- rbind(res, r)
  } 
}
rownames(res) <- NULL
print(res)

## library(taRifx)
## library(xtable)

## res.by <- bytable(ChickWeight$weight, list(ChickWeight$Chick, ChickWeight$Diet) )
## colnames(test.by) <- c('Diet','Chick','Mean Weight')
## print(latex.table.by(test.by), include.rownames = FALSE, include.colnames = TRUE, sanitize.text.function = force)

stop()

for (i in funcs) { # for each of desired functions
  limit = limits[i,]
  bias = fbias[i]
  for (d in dims) { # for each of desired dimentions
    lower = as.double(rep(limit[1], d))
    upper = as.double(rep(limit[2], d))
    Np = params[i,sprintf('Np%dD', d)]
    Cr = params[i,sprintf('Cr%dD', d)]
    for (run in 1:runs) { # run DE algorithm 'runs' times
      for (fe.i in 1:length(fes)) { # fetch error values from optimization history
        fe = fes[fe.i]
        val = results[i, d, run, fe.i, 1]
        err = val - bias
        iter = results[i, d, run, fe.i, 2]
        nfeval = results[i, d, run, fe.i, 3]
        print(sprintf("Error of function %d in %d dims %dth time in %d fes is %g", i, d, run, fe, err))
      }
    }
  }
}
