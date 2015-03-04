suppressPackageStartupMessages(library(doMPI))
cl = startMPIcluster()
registerDoMPI(cl)

source("utils.r")

overhead_foreach <- function(n, wait)
{
  wait <- totaltime/n
  
  meas <- system.time({
    ret <- foreach(i=1:n) %dopar% {Sys.sleep(wait)}
  })[3]
  
  meas <- as.numeric(meas)
  minimum <- totaltime / clusterSize(cl)
  overhead <- meas-minimum
  ret <- list(minimum=minimum, measured=meas, overhead=overhead, PercentOfRuntime=overhead/meas*100)
  class(ret) <- "overhead"
  attr(ret, "printfun") <- cat

  return(ret)
}



overhead_foreach(n, totaltime)


closeCluster(cl)
mpi.quit()

