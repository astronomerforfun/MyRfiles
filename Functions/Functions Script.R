
#Inference Function. 
load(url('http://s3.amazonaws.com/assets.datacamp.com/course/dasi/inference.Rdata'))


#' Calculate hit streaks.

#' 

#' @param x A data frame or character vector of hits (\code{"H"}) and misses (\code{"M"}).

#' @return A data frame with one column, \code{length}, containing the length of each hit streak.

#' @examples

#' data(kobe_basket)

#' calc_streak(kobe_basket$shot)

#' 

#' @export



calc_streak = function(x)
  
{
  
  if (!is.atomic(x))
    
    x = x[,1]
  
  
  
  if (any(!x %in% c("H","M")))
    
    stop('Input should only contain hits ("H") and misses ("M")')
  
  
  
  y = rep(0,length(x))
  
  y[x == "H"] = 1
  
  y = c(0, y, 0)
  
  wz = which(y == 0)
  
  streak = diff(wz) - 1
  
  
  
  return(data.frame(length = streak))
  
}


#oilabs package (sample and Repeat 'n' times)

rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1)
  
{
  
  n <- nrow(tbl)
  
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))
  
  
  
  rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])
  
  
  
  dplyr::group_by(rep_tbl, replicate)
  
}