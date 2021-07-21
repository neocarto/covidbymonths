library(sf)
library(cartography)
library(cartogram)
library(tanaka)
library(SpatialPosition)
library(png)

# Metadata

sources <- "Données hospitalières uniquement.\nCartographie : Nicolas Lambert, 2020.\nSource : Insee 2020 & Santé publique France, 2020.\n[https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19]"

# Signature

signature <- readPNG("data/signature.png")
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}


# import

dept <- st_read("data/DEPT.geojson") %>% st_transform(2154)
fr <- st_union(dept)
boxes <- st_read("data/boxes.geojson") %>% st_transform(2154)
remotes <- st_read("data/remotes.geojson") %>% st_transform(2154)
ue <- st_read("data/ue.geojson") %>% st_transform(2154)

# data

pop <- read.csv("data/pop2020.csv", sep = ";", stringsAsFactors = FALSE)

url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"
data <- read.csv(url, sep = ";")
data$month <- substr(data$jour, 1, 7)

dd <- aggregate(data$incid_dc, by=list(Category=data$month), FUN=sum)
colnames(dd) <- c("date","dc")

mois <- c("janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre")
nbmois <- c(31,28,31,30,31,30,31,31,30,31,30,31)

daymin <- aggregate(data$jour, by=list(id=data$month), FUN=min)
daymax <- aggregate(data$jour, by=list(id=data$month), FUN=max)
daymin$x <- as.numeric(substr(daymin$x, 9, 10))
daymax$x <- as.numeric(substr(daymax$x, 9, 10))

data <- aggregate(data$incid_dc, by=list(id1 = data$dep, id2=data$month), FUN=sum)

colnames(data) <- c("dep","date","dc")
data <- merge(x = data, y = pop, by.x = "dep", by.y = "id")
data <- data[,c("date","dep","dc","pop2020")]
data$dcperinh <- data$dc/data$pop2020*100000
csv <- data

dates <- as.character(unique(data$date))  
dates <- dates[order(dates, na.last = NA)]

mypal <- carto.pal(pal1 = "red.pal", n1 = 10)
# display.carto.all(10)

# -----------------------------------------------------------------------------------
# 1 - DOTS
# -----------------------------------------------------------------------------------

max(dd$dc) / 2500

fact <- 5

nb <- length(dates)

for (i in 1:nb) {

#  i = 1
  
date = dates[i]
file <- paste0("maps/absolute/dc/dc_",date,".png")

if (!file.exists(file)){

data <- csv[csv$date == date,]

min <- daymin$x[daymin$id == date]
max <- daymax$x[daymax$id == date]
if (min > 1){lbl <- paste0("(du ",min," au ",max,')')}
if (max <= nbmois[as.numeric(substring(date,6,7))]){lbl <- paste0("(du ",min," au ",max,')')}
if (min == 1 & max == nbmois[as.numeric(substring(date,6,7))]){lbl <- ""}

datelabel <- paste0(mois[as.numeric(substr(date, 6,7))]," ",substr(date, 1,4))
total <- sum(data$dc, na.rm = TRUE)
  
# top15 <- data[order(data$dc, decreasing = TRUE),]
# top15 <- top15[c(1:15),]
# top15 <- merge(dept, top15, by.x = "id", by.y = "dep")

top50 <- data[data$dc >= 50,]
top50 <- merge(dept, top50, by.x = "id", by.y = "dep")

data[,"dc"] <- round(data[,"dc"]/fact,0)

data_unique <- data[data$dc == 1,]
data_multi <-  data[data$dc  > 1,]


if (nrow(data_multi) > 0){
  for (i in 1:dim(data_multi)[1]){
    nb <- as.numeric(data_multi[i,"dc"])[1]
    tmp <- data_multi[i,]
    tmp[,"dc"] <- 1
    for (j in 1:nb){ data_unique <- rbind(data_unique,tmp)}
  }
}

x <- merge(dept, data_unique, by.x = "id", by.y = "dep")

if (nrow(x) != 0){
  d <- cartogram_dorling(st_jitter(x), "dc", l = 45000000, itermax = 1000)
}



# Map

col <- paste0(mypal[5],"99")
coldots <- mypal[6]

png(file, width = 1500, height = 1400, res = 150)
par(mar = c(0,0,0,0), bg="#A8D4EF")
plot(st_geometry(fr), col = "#C8D8E5", border = NA)
plot(st_geometry(ue), col = "#E0D4C6", border = NA, add = T)
plot(st_geometry(remotes), col = "#E0D4C6", border = NA, add = T)
plot(st_geometry(fr) + c(1500, -1500), col ="#00000010", border = NA, add = T)
plot(st_geometry(fr) + c(3000, -3000), col ="#00000010", border = NA, add = T)
plot(st_geometry(fr) + c(4500, -4500), col ="#00000010", border = NA, add = T)
plot(st_geometry(fr) + c(6000, -6000), col ="#00000010", border = NA, add = T)
plot(st_geometry(fr) + c(7500, -7500), col ="#00000010", border = NA, add = T)
plot(st_geometry(fr), col = "#e0d580", border = "#C8D8E5", lwd = 0.4, add = T)
plot(st_geometry(boxes), col = NA, border ="white", lty = 2, add= T)
if (nrow(x) != 0){
  plot(st_geometry(d), col= coldots, border = "#e0d580", lwd = 1, add=T)
}
rect(60000, 7090000, 1300000, 7180000, border = NA, col = "#FFFFFF50")
text(x = 1200000, y = 6400000, "Covid-19", cex = 7.7, pos = 4, font = 2, col="#FFFFFF50", srt=90)
text(x = 70000, y = 6050000, sources, cex = 0.8, pos = 4, font = 3, col = "#30303080") 
text(x = 70000, y = 6150000, total, cex = 6, pos = 4, font = 2, col=col) 
text(x = 70000, y = 7120000, "PERSONNES TUÉES PAR LA COVID-19", cex = 1.5, pos = 4, font = 2, col = col) 
text(x = 1285000, y = 7120000, toupper(datelabel), cex = 1.5, pos = 2, font = 2, col = col) 
text(x = 1285000, y = 7100000, lbl, cex = 0.7, pos = 2, font = 2, col = col) 
text(x = 70000, y = 6230000, "NOMBRE DE MORTS", cex = 0.7, pos = 4, font = 2, col = col) 
if (dim(top50)[1] > 0){
labelLayer(x = top50, txt = "name", col= coldots, cex = 0.7, font = 2,
           halo = TRUE,  bg = "#e0d580", r=0.15, overlap = FALSE, show.lines = FALSE)
}
if (dim(top50)[1] > 0){
labelLayer(x = top50, txt = "name", col= coldots, cex = 0.7, font = 2,
           halo = TRUE,  bg = "#e0d580", r=0.15, overlap = FALSE, show.lines = FALSE)
}
x = 80000
y = 7030000
l = 160000
h = 40000
rect(x, y, x + l, y + h, border = NA, col = col)

if (fact > 1){ text(x = x + 1000, y = y + 18000 , paste0("1 POINT = ",fact," PERS."), cex = 0.7, pos = 4, font = 2, col = "#FFFFFF99") }
logoing_func(signature, x=0.97, y=0.015, size=0.05)
dev.off()
}
}

# -----------------------------------------------------------------------------------
# 2 - SMOOTH
# -----------------------------------------------------------------------------------

n <- 20
crp <- colorRampPalette(mypal)
cols <- crp(n)

# summary(csv$dcperinh)
breaks <- getBreaks(v = csv$dcperinh, nclass = n, method = "fisher")

nb <- length(dates)

for (i in 1:nb) {

  #i = 1
  
  date = dates[i]
  file <- paste0("maps/relative/dc/dc_",date,".png")
  
  if (!file.exists(file)){
  
  x <- csv[csv$date == date,]
  x <- merge(dept,x, by.x = "id", by.y="dep")
  total <- sum(x$dc, na.rm = TRUE)
  x$dc <- x$dc * 100000
  
  top50 <- x[x$dcperinh >= 10,]
  # top50 <- merge(dept, top50, by.x = "id", by.y = "dep")
  
  #top15 <- merge(dept, top15, by.x = "id", by.y = "dep")

  
  min <- daymin$x[daymin$id == date]
  max <- daymax$x[daymax$id == date]
  if (min > 1){lbl <- paste0("(du ",min," au ",max,')')}
  if (max <= nbmois[as.numeric(substring(date,6,7))]){lbl <- paste0("(du ",min," au ",max,')')}
  if (min == 1 & max == nbmois[as.numeric(substring(date,6,7))]){lbl <- ""}
  
  datelabel <- paste0(mois[as.numeric(substr(date, 6,7))]," ",substr(date, 1,4))
  # a <- sum(x$dc, na.rm = TRUE)
  # b <- sum(x$pop2020, na.rm = TRUE)
  # total <- round(a/b)

  
  smooth <- smoothLayer(
    x = x,
    var = "dc",
    var2 = "pop2020",
    breaks = breaks,
    col = cols,
    span = 40000,
    beta = 2,
    mask = st_geometry(x), border =NA ,
    legend.pos = "n", add= F)
  
  for (k in 1:nrow(smooth)){
    smooth[k,"col"] <- cols[findInterval(smooth$center[k], breaks)]
  }
  
  # Map
  
  col <- paste0(mypal[5],"99")
  coldots <- mypal[6]
  
  png(file, width = 1500, height = 1400, res = 150)
  par(mar = c(0,0,0,0), bg="#A8D4EF")
  plot(st_geometry(fr), col = "#C8D8E5", border = NA)
  plot(st_geometry(ue), col = "#E0D4C6", border = NA, add = T)
  plot(st_geometry(remotes), col = "#E0D4C6", border = NA, add = T)
  plot(st_geometry(fr) + c(1500, -1500), col ="#00000010", border = NA, add = T)
  plot(st_geometry(fr) + c(3000, -3000), col ="#00000010", border = NA, add = T)
  plot(st_geometry(fr) + c(4500, -4500), col ="#00000010", border = NA, add = T)
  plot(st_geometry(fr) + c(6000, -6000), col ="#00000010", border = NA, add = T)
  plot(st_geometry(fr) + c(7500, -7500), col ="#00000010", border = NA, add = T)
  plot(st_geometry(fr), col = "#e0d580", border = "#C8D8E5", lwd = 0.4, add = T)
  plot(st_geometry(fr), col = "#E0D4C6", border = "#C8D8E5", lwd = 0.4, add = T)
  plot(st_geometry(boxes), col = NA, border ="white", lty = 2, add= T)
  
  
  # plot(st_geometry(x), col = cols, add  = T, border = NA)
  
  for (j in 1:nrow(smooth)){
    color <-  smooth[j,"col"] %>% st_drop_geometry()
    color <- color$col
    # plot(st_geometry(smooth[j,]) + c(2000, -2000), col="#66606070", border = NA, add=T)
    # plot(st_geometry(smooth[j,]) + c(-2000, 2000), col="#FFFFFF70", border = NA,  add=T)
    
    plot(st_geometry(smooth[j,]) + c(1000, -1000), col="#66606020", border = NA, add=T)
    plot(st_geometry(smooth[j,]) + c(2000, -2000), col="#66606020", border = NA, add=T)
    plot(st_geometry(smooth[j,]) + c(3000, -3000), col="#66606020", border = NA, add=T)
    plot(st_geometry(smooth[j,]) + c(4000, -4000), col="#66606020", border = NA, add=T)

    
    plot(st_geometry(smooth[j,]) + c(-1000, 1000), col="#FFFFFF20", border = NA,  add=T)
    plot(st_geometry(smooth[j,]) + c(-2000, 2000), col="#FFFFFF20", border = NA,  add=T)    
    plot(st_geometry(smooth[j,]) + c(-3000, 3000), col="#FFFFFF20", border = NA,  add=T)
    plot(st_geometry(smooth[j,]) + c(-4000, 4000), col="#FFFFFF20", border = NA,  add=T)
    plot(st_geometry(smooth[j,]), col = color, border = NA, add=T)
  }
  
  if (dim(top50)[1] > 0){
    labelLayer(x = top50, txt = "name", col= "#30313395", cex = 0.7, font = 2,
               halo = FALSE, overlap = FALSE, show.lines = FALSE)  
  }


  
  rect(60000, 7090000, 1300000, 7180000, border = NA, col = "#FFFFFF50")
  # text(x = 1200000, y = 6400000, "Covid-19", cex = 7.7, pos = 4, font = 2, col="#FFFFFF50", srt=90)
  text(x = 70000, y = 6050000, sources, cex = 0.8, pos = 4, font = 3, col = "#30303080")
  text(x = 70000, y = 6150000, total, cex = 6, pos = 4, font = 2, col=col)
  text(x = 70000, y = 7120000, "PERSONNES TUÉES PAR LA COVID-19", cex = 1.5, pos = 4, font = 2, col = col)
  text(x = 1285000, y = 7120000, toupper(datelabel), cex = 1.5, pos = 2, font = 2, col = col)
  text(x = 1285000, y = 7100000, lbl, cex = 0.7, pos = 2, font = 2, col = col)
  #text(x = 70000, y = 6230000, "NOMBRE DE MORTS", cex = 0.7, pos = 4, font = 2, col = col)
  text(x = 70000, y = 6230000, "NOMBRE DE MORTS", cex = 0.7, pos = 4, font = 2, col = col)
 text(x = 70000, y = 6980000, "CALCUL RÉALISÉ\nA PARTIR DES DONNÉES\nPAR DÉPARTEMENT", cex = 0.7, pos = 4, font = 2, col = col)

  x = 80000
  y = 7030000
  l = 220000
  h = 40000
  rect(x, y, x + l, y + h, border = NA, col = col)
  
  text(x = x + 1000, y = y + 18000 , "POUR 100 000 HABITANTS", cex = 0.7, pos = 4, font = 2, col = "#FFFFFF99")
  
  text(x = 1285000, y = y + 18000 , "NOMBRE DE MORTS POUR 100 000 HABITANTS", cex = 0.7, pos = 2, font = 2, col = col)
  
  legendChoro(pos = c(1150000,6470000),
              title.txt  = NA,
              title.cex = 0.8,
              values.cex = 0.6,
              breaks = breaks,
              col = cols,
              nodata = FALSE, 
              frame = FALSE, border = "#FFFFFF50",
              symbol="box")
  
  x = 1201000
  y = 6430000
  l = 50000
  h = 600000
  rect(x, y, x + l, y + h, border = NA, col = "#E0D4C690")
  logoing_func(signature, x=0.97, y=0.015, size=0.05)
  dev.off()

  }
}

