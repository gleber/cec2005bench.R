library('sfsmisc')

funcs = c(6:11, 19:24)
fes = c(10**3, 10**4, 10**5, 5*10**5) # full workload
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

iters = fes.max/fe.per.strategy[strategy] # defines number of iterations depending on the DE strategy used
