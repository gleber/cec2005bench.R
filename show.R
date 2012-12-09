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
