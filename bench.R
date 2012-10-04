library('cec2005benchmark')
library('DEoptim')
library('sfsmisc')

##
## Parameters
##

funcs = c(6:12, 19:24)
#funcs = c(6:7)
#fes = c(10**3, 10**4, 10**5, 5*10**5) # full workload
fes = c(10**3, 10**4, 10**5, 5*10**5) # test workload
#fes = c(10**1, 10**2) # debug workload
fes.max = last(fes)
dims = c(10, 30)
runs = 25
eps = 10**-8
strategy = 2 # strategy used by DEoptim
fe.per.strategy = c(NA, 2)
F = 0.9
params = read.table('data/nps.tbl',header=T) # contains Np and Cr
fbias = as.matrix(read.table('data/fbias_data.txt')) # contains minimum values
limits = as.matrix(read.table('data/limits.txt')) # contains upper and lower limits for optimization domain

##
## Variables
##

load("results.rdata")
if (exists("results")) {
  results.old = results
} else {
  results.old = array(NA, dim=c(max(funcs), max(dims), runs, length(fes), 3))
}
results = array(NA, dim=c(max(funcs), max(dims), runs, length(fes), 3)) # stores results of benchmark
iters = fes.max/fe.per.strategy[strategy] # defines number of iterations depending on the DE strategy used

##
## Main loop
##


for (i in funcs) { # for each of desired functions
  limit = limits[i,]
  bias = fbias[i]
  f = function(x) {
    cec2005benchmark(i, x)
  }
  for (d in dims) { # for each of desired dimentions
    lower = as.double(rep(limit[1], d))
    upper = as.double(rep(limit[2], d))
    Np = params[i,sprintf('Np%dD', d)]
    Cr = params[i,sprintf('Cr%dD', d)]
    for (run in 1:runs) { # run DE algorithm 'runs' times
      old.val = try(results.old[i, d, run, length(fes), 1])
      if (is.numeric(old.val)) {
        results[i, d, run, , ] = results.old[i, d, run, , ]
        print(sprintf("Skipping function %d in %d dims for %dth time for %d iters", i, d, run, iters))
        print(results[i, d, run, ,])
        next;
      }
      print(sprintf("Running function %d in %d dims for %dth time for %d iters", i, d, run, iters))
      r = DEoptim(f, lower, upper, control= # run optimization
        DEoptim.control(itermax=iters,
                        NP=Np, CR=Cr, F=F,
                        VTR=bias + eps,
                        strategy=strategy,
                        trace=iters / 10,
			parallelType=1, packages=c('cec2005benchmark'), parVar=c('i')
                        ))
      for (fe.i in 1:length(fes)) { # fetch error values from optimization history
        fe = fes[fe.i]
        fe.pop = min(r$optim$iter, (fe / fe.per.strategy[strategy]))
        pop = r$member$bestmemit[fe.pop,]
        val = f(pop)
        err = val - bias
        results[i, d, run, fe.i, 1] = val
        results[i, d, run, fe.i, 2] = r$optim$iter
        results[i, d, run, fe.i, 3] = r$optim$nfeval
        print(sprintf("Error of function %d in %d dims %dth time in %d fes is %g", i, d, run, fe, err))
      }
      save(results, file="results.rdata")
    }
  }
}
