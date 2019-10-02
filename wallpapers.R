library(XML)
library(stringr)
library(RCurl)

################eyes wallpapers########################
url <- "http://www.surenmanvelyan.com/eyes/animal-eyes-2/?occur=1&cover=0&album=37"
links <- getHTMLLinks(url)
links <- links[str_detect(links,"animal-eyes-2/wppaspec")]
for(i in 1:length(links)){
  content.page <- htmlParse(links[i])
  content.meta <- as.vector(unlist(xpathSApply(content.page, "//meta", function(x) xmlAttrs(x))))
  content.img <- grep(".jpg",content.meta,value=T)
  content.title <- gsub(" ","_",content.meta[grep("title",content.meta,value=F)[2]+1])
  download.file(content.img,paste0("D:/temp/",content.title,".jpg"),mode="wb")
}

############google photos (mini)########################

a <- c("https://www.google.com/search?site=&tbm=isch&source=hp&biw=1536&bih=895&q=imbabura+volcan&oq=imbabura+volcan&gs_l=img.3..0j0i30k1j0i8i30k1l8.1892.5604.0.5836.15.15.0.0.0.0.68.731.15.15.0....0...1ac.1.64.img..0.15.730...0i10k1j0i5i30k1.xC-4eVcyKsc",
       "https://www.google.com/search?site=&tbm=isch&source=hp&biw=1536&bih=895&q=altar+volcano+ecuador&oq=altar+volcano+ecuador&gs_l=img.3...9886.13699.0.14206.21.13.0.8.8.0.69.616.13.13.0....0...1ac.1.64.img..0.13.614...0j0i10k1j0i5i30k1j0i8i30k1j0i24k1j0i30k1.oKGpGIABDMc",
       "https://www.google.com/search?site=&tbm=isch&source=hp&biw=1536&bih=895&q=revelation+artist&oq=revelation+artist&gs_l=img.3...2204.4322.0.4552.17.11.0.6.6.0.70.412.11.11.0....0...1ac.1.64.img..0.15.414...0j0i5i30k1j0i8i30k1j0i24k1.YEqamqwdUB0")
       
b <- c("imbabura","altar","di-revelation artist")


for(k in 1:3){
#define an url at google images
url <- a[k]
#assign a output folder + outputname
dest.folder <- "d:/temp/volcanes"
output.name <- b[k]

#libraries
library(jpeg)
library(rasterImage)
#process web
fol.temp <- paste0(dest.folder,"/temp");dir.create(fol.temp,showWarnings = F)
url <- htmlParse(gsub("https","http",url))
links <- xpathSApply(url,"//a/img",function(x) xmlAttrs(x))
links <- links[2,]
for(i in 1:length(links)){
  download.file(links[i],paste0(paste0(fol.temp,"/"),i,"_temp.jpg"),mode="wb")
}
#mosaic them
files.jpg <- list.files(fol.temp,full.names=T,pattern="_temp.jpg$")
file.output <- paste0(dest.folder,"/",output.name,".jpg")
jpeg(file.output)
par(mfrow=c(5,4),mar=c(1,1,1,1))
for(i in 1:length(files.jpg)){
  jpg <- readJPEG(files.jpg[i])
  res <- dim(jpg)[1:2]
  plot(1,1,xlim=c(1,res[2]),ylim=c(1,res[1]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[2],res[1])
}
dev.off()
unlink(fol.temp,recursive = T, force = T)
}











