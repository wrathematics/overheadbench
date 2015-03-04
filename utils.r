args <- as.numeric(commandArgs(trailingOnly=TRUE))
totaltime <- args[1]
n <- args[2]

title_case <- function(x) gsub(x, pattern="(^|[[:space:]])([[:alpha:]])", replacement="\\1\\U\\2", perl=TRUE)

print.overhead <- function(x, digits=3)
{ 
  maxlen <- max(sapply(names(x), nchar))
  names <- gsub(names(x), pattern="_", replacement=" ")
  names <- title_case(x=names)
  spacenames <- simplify2array(lapply(names, function(str) paste0(str, ":", paste0(rep(" ", maxlen-nchar(str)), collapse=""))))
  
  printfun <- attr(x, "printfun")
  
  x <- as.character(sapply(x, round, digits=digits)) 
  x[4] <- paste0(x[4], "%") 
  
  printfun(paste0(paste(spacenames, x, sep=" ", collapse="\n"), "\n")) 
  invisible() 
}

