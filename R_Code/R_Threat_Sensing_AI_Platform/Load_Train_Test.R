library(httr)
loadTrain <- function(url){

  out <- GET(url=url)
  http_status(out)
  out <- content(out)
  
}
#Ask Bhavenya for Url
loadTrain("")