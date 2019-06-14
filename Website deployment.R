#网站部署
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='', token='',secret='')
setwd("F:\\") #路径最好都是英文的
#设置
options(RCurlOptions = list(proxy = "PROXY:PORT", proxyuserpwd="USER:PWD", verbose = TRUE))
options(rsconnect.http.trace = TRUE, rsconnect.error.trace = TRUE, rsconnect.http.verbose = TRUE)
options(RCurlOptions = list(proxy = "http://user:pwd@proxy:port"))
options(shinyapps.http = "rcurl")
options(shiny.usecairo = FALSE)
options(encoding = "UTF-8")
rsconnect::deployApp("") #在shinapps.io上部署
terminateApp("abcdef") #关闭连接