library('taRifx')
library('xtable')

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

biases = t(as.matrix(fbias))[,rep(1,25)]

funcs.m <- matrix(funcs, ncol=6, byrow=TRUE)

## for (d in c(10)) {
for (d in dims) {
  for (fr in 1:nrow(funcs.m)) {
    fns = funcs.m[fr,]
    bs = biases[fns,]
    cat(sprintf("Results in %d dimensions for functions %s\n", d, paste(fns, collapse=", ")))
    ## print(fns)
    res.rows = 0 # 7*length(dims)*length(fes)
    res.cols = length(fns) + 2
    res = matrix(nrow=res.rows, ncol=res.cols)
    
    indim <- results[fns,d,,,1]
    for (fe.i in 1:length(fes)) {
      fe <- fes[fe.i]
      vals <- indim[,,fe.i]
      infe <- vals - bs
      infe.s <- apply(infe, 1,
                      function(x) format(c(quantile(x, na.rm=TRUE),mean(x),sd(x)), digits=2, scientific=TRUE))
      ## function(x) c(quantile(x, na.rm=TRUE),mean(x),sd(x)))
      rownames(infe.s)[6] <- "avg"
      rownames(infe.s)[7] <- "std"
      ## rownames(infe.s) <- lapply(rownames(infe.s),
      ##                            ## function(x) paste(d, "dim", format(fe, scientific=TRUE), "fes", x))
      ##                            function(x) paste(format(fe, scientific=TRUE), "fes", x))
      r <- cbind(format(fe, scientific=TRUE), rownames(infe.s), infe.s)
      colnames(r)[1] <- "fe"
      colnames(r)[2] <- "stats"
      res <- rbind(res, r)
    }

    rownames(res) <- NULL
    print(xtable(res), tabular.environment='longtable', include.rownames=FALSE)
    cat("\\newpage\n")
  }
}
