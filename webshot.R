library('devtools')
devtools::install_github('wch/webshot')
library(webshot)
webshot::install_phantomjs()

#webshot(url, filename.extension)
webshot("https://www.listendata.com/", "listendata.png")
