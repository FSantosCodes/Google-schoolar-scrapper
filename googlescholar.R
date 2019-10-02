library(XML)
library(xml2)
library(stringr)
library(RCurl)
library(rvest)

############### INPUTS #######################

#google schoolar URL link
url <- "https://scholar.google.de/scholar?hl=es&as_sdt=0%2C5&q=intitle%3Aremote+sensing+AND+landscape&btnG="
#include google books?
#include_books <- FALSE
#save
save_search <- FALSE
#output prefix
output_prefix <- "test"
#output folder
output_folder <- "C:/DATA/scholar"

############### PROCESSING #######################

#read
a.source <- readLines(url,encoding="UTF-8",warn=F)
a.parsed <- htmlParse(a.source,encoding="UTF-8")
a.links <- getHTMLLinks(a.parsed)
a.links <- a.links[grep("scholar\\?start\\=",a.links)]
a.links <- paste0("https://scholar.google.de",a.links)
a.links <- c(gsub("start\\=10","start\\=0",a.links[1]),a.links)
a.links <- a.links[!duplicated(a.links)]
#function
scraplinks <- function(url){
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data.frame(link = link_, url = url_,stringsAsFactors=F))
}

#get
schoolar.data <- list()
for(i in 1:length(a.links)){

  #open link
  i.df <- scraplinks(a.links[i])
  #remove voids
  i.df <- i.df[-c(grep("javascript\\:void",i.df$url)),]
  #identify relevant links
  pos.data <- grep("\\/scholar_alerts",i.df$url)
  i.df <- i.df[(pos.data[2]+1):(pos.data[3]-1),]

  #related links
  pos.data <- grep("\\/scholar\\?q\\=related",i.df$url)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #cluster
  pos.data <- grep("\\/scholar\\?cluster",i.df$url)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #cache
  pos.data <- grep("scholar\\.googleusercontent\\.com",i.df$url)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #others
  pos.data <- grep("\\/scholar\\?hl",i.df$url)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #pdf and html
  pos.data <- grep("\\[PDF\\]|\\[HTML\\]|\\[DOC\\]",i.df$link)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #authors
  pos.data <- grep("\\/citations\\?user",i.df$url)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #full views
  pos.data <- grep("\\?output\\=instlink\\&",i.df$url)
  if(length(pos.data)!=0){i.df <- i.df[-pos.data,]}
  #remove incomplete cases
  pos.data <- grep("\\/scholar\\?cites\\=",i.df$url)
  incom.case <- match(1,diff(pos.data))
  if(!is.na(incom.case)[1]){
    pos.data <- pos.data[incom.case] + 1
    i.df <- i.df[-pos.data,]
  }
  #get cites
  pos.data <- grep("\\/scholar\\?cites\\=",i.df$url)
  i.cita <- i.df$link[pos.data]
  i.cita <- as.numeric(gsub("[[:alpha:]]", "",i.cita))
  #get publications
  i.publi <- i.df[pos.data-1,]
  i.publi$cites <- i.cita
  i.publi <- i.publi[,c(1,3,2)]
  names(i.publi)[1] <- "name"
  i.publi$page <- i
  #include books?
  #if(!include_books){
  #  i.publi <- i.publi[-grep("books[.]",i.publi$url,value=F),]
  #}
  #save
  schoolar.data[[i]] <- i.publi
  print(paste0("done page: ",i))
  Sys.sleep(2)
}
#merge
schoolar.data <- do.call("rbind.data.frame",schoolar.data)

############### SAVE #######################

#name
if(save_search){
  output.name <- paste0(output_folder,"/",output_prefix,"_scrapped.rds")
  saveRDS(schoolar.data,output.name)
}else{
  print(schoolar.data)
}
rm(list=setdiff(ls(), "schoolar.data"))

