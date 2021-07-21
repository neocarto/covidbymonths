# Map update

date <- Sys.Date()
d <- substr(date,9,10)
m <- substr(date,6,7)
y <- substr(date,1,4)

if (d == "01"){
  
  unlink("maps/relative/rad/*")
  unlink("maps/relative/rea/*")
  unlink("maps/relative/hosp/*")
  unlink("maps/relative/dc/*")
  
}

if (d != "01"){
  file.remove(paste0("maps/absolute/rad/rad_",y,"-",m,".png"))  
  file.remove(paste0("maps/relative/rad/rad_",y,"-",m,".png"))
  file.remove(paste0("maps/absolute/rea/rea_",y,"-",m,".png"))  
  file.remove(paste0("maps/relative/rea/rea_",y,"-",m,".png"))
  file.remove(paste0("maps/absolute/hosp/hosp_",y,"-",m,".png"))  
  file.remove(paste0("maps/relative/hosp/hosp_",y,"-",m,".png"))
  file.remove(paste0("maps/absolute/dc/dc_",y,"-",m,".png"))  
  file.remove(paste0("maps/relative/dc/dc_",y,"-",m,".png"))
}

source("src/src_bymonth_dc.R")
source("src/src_bymonth_hosp.R")
source("src/src_bymonth_rea.R")
source("src/src_bymonth_rad.R")

# csv files

mois <- c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre")
x <- rev(list.files("maps/absolute/rad"))
x <- substr(x,5,11)
csv <- data.frame(paste0(mois[as.numeric(substr(x,6,8))]," ",as.numeric(substr(x,1,4))),paste0(x,".png"))
colnames(csv) <- c("label","value")
write.csv(csv,"maps/dates.csv", row.names = FALSE)
View(csv)



# List images

urls <- data.frame(var=NA, type=NA, url=NA)[numeric(0), ]
for (i in 1:nrow(csv)){
  urls <- rbind(urls,data.frame(var = "dc", type = "absolute", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/absolute/dc/dc_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "dc", type = "relative", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/relative/dc/dc_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "rad", type = "absolute", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/absolute/rad/rad_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "rad", type = "relative", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/relative/rad/rad_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "hosp", type = "absolute", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/absolute/hosp/hosp_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "hosp", type = "relative", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/relative/hosp/hosp_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "rea", type = "absolute", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/absolute/rea/rea_",csv$value[i])))
  urls <- rbind(urls,data.frame(var = "rea", type = "relative", url=paste0("https://raw.githubusercontent.com/neocarto/covidbymonths/main/maps/relative/rea/rea_",csv$value[i])))
}

write.csv(urls,"maps/url.csv", row.names = FALSE)



