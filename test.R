#-----needed packages
source("funcs.R")


p=sql_get(query="select * from bid_ask")



HTML("<iframe> src='https://docs.google.com/spreadsheets/d/1kRDKgIM45l_gIHQKxtz4b8eALP6QFBVlu5yO2eR5Gg0/pubhtml?gid=08&range=a1:i25&single=true&widget=true&headers=false' </iframe>")

FloydHub

LogLossBinary = function(actual, predicted, eps = 1e-15) {
     predicted = pmin(pmax(predicted, eps), 1-eps)
     - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

LogLossBinary(1, c(0))  
