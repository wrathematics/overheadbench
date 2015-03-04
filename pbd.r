suppressPackageStartupMessages(library(pbdMPI))
init()

source("utils.r")

overhead_pbd <- function(n, wait)
{
  wait <- totaltime/n
  
  meas <- system.time({
    ret <- pbdLapply(1:n, function(i) Sys.sleep(wait))
  })[3]
  
  meas <- as.numeric(meas)
  minimum <- totaltime / comm.size()
  overhead <- meas-minimum
  ret <- list(minimum=minimum, measured=meas, overhead=overhead, PercentOfRuntime=overhead/meas*100)
  class(ret) <- "overhead"
  attr(ret, "printfun") <- function(x) comm.cat(x, quiet=TRUE)

  return(ret)
}


overhead_pbd(n, totaltime)


finalize()

