rm(list=ls())
install.packages("dplyr")
library("dplyr")

#part1 help ConservationAI
# set working directory
setwd("E:/learning/durham/mds project/code")

# find list of  photos' information
fphotos <- read.csv("Photo.csv", stringsAsFactors = F)

# find list of required photos
small_mammal <- read.csv("small_mammal.csv", stringsAsFactors = F)
birds <- read.csv("birds.csv", stringsAsFactors = F)
cat <- read.csv("cat.csv", stringsAsFactors = F)
#download photos

#small mammals
small_mammal$dirname <- gsub(pattern = "/home/stevens/misc/dbl0zz14/public_html/components/com_biodiv/uploads/",
                        replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",small_mammal$dirname)
small_mammal$dirname <- gsub(pattern = "/var/www/html/components/com_biodiv/uploads/",
                        replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",small_mammal$dirname)
small_mammal$dirname <- gsub(pattern = "/var/www/html/biodivimages/",
                        replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",small_mammal$dirname)
small_mammal$filename <- as.character(small_mammal$filename)
# now, for every pic, download it to the "pics1" folder
if (!dir.exists("pics1")) dir.create("pics1")
dd1.function <- function(r){
  infile <- sprintf("%s/%s",as.character(r[5]),as.character(r[4]))
  fext <- strsplit(as.character(r[4]),"\\.")[[1]][2]
  outfile <- sprintf("pics1/%08s_%07s.%s",as.character(r[2]),as.character(r[6]),fext)
  res <- try(if (!file.exists(outfile)) download.file(url = infile, destfile = outfile,mode='wb'))
  return(res[1])
}

count<-1
amount<-1000
chart1 <- small_mammal[-c(1:17731),]
while (count<1001) {
  amount <- amount+1
  t <- dd1.function(small_mammal[amount,])
  q <- grepl("Error",as.character(t))
  if(q == FALSE){
    chart1[count,] <- small_mammal[amount,]
    count <- count+1
    
  }
}
dd1.function(small_mammal[2000,])
write.csv(chart1,file="small_mammal_summary.csv")
chart1$sequence_id


#birds
birds$dirname <- gsub(pattern = "/home/stevens/misc/dbl0zz14/public_html/components/com_biodiv/uploads/",
                             replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",birds$dirname)
birds$dirname <- gsub(pattern = "/var/www/html/components/com_biodiv/uploads/",
                             replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",birds$dirname)
birds$dirname <- gsub(pattern = "/var/www/html/biodivimages/",
                             replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",birds$dirname)
birds$filename <- as.character(birds$filename)
# now, for every pic, download it to the "pics1" folder
if (!dir.exists("pics2")) dir.create("pics2")
dd2.function <- function(r){
  infile <- sprintf("%s/%s",as.character(r[5]),as.character(r[4]))
  fext <- strsplit(as.character(r[4]),"\\.")[[1]][2]
  outfile <- sprintf("pics2/%08s_%07s.%s",as.character(r[2]),as.character(r[6]),fext)
  res <- try(if (!file.exists(outfile)) download.file(url = infile, destfile = outfile,mode='wb'))
  return(res[1])
}
dd2.function(birds[1751,])
count<-1
amount<-1
chart2 <- birds[-c(1:3514),]
while (count<1001) {
  amount <- amount+1
  t <- dd2.function(birds[amount,])
  q <- grepl("Error",as.character(t))
  if(q == FALSE){
    chart2[count,] <- birds[amount,]
    count <- count+1
    
  }
}

write.csv(chart2,file="birds_summary.csv")
chart2$sequence_id

#cat
cat$dirname <- gsub(pattern = "/home/stevens/misc/dbl0zz14/public_html/components/com_biodiv/uploads/",
                    replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",cat$dirname)
cat$dirname <- gsub(pattern = "/var/www/html/components/com_biodiv/uploads/",
                    replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",cat$dirname)
cat$dirname <- gsub(pattern = "/var/www/html/biodivimages/",
                    replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",cat$dirname)
cat$filename <- as.character(cat$filename)
# now, for every pic, download it to the "pics3" folder
if (!dir.exists("pics3")) dir.create("pics3")
dd3.function <- function(r){
  infile <- sprintf("%s/%s",as.character(r[5]),as.character(r[4]))
  fext <- strsplit(as.character(r[4]),"\\.")[[1]][2]
  outfile <- sprintf("pics3/%08s_%07s.%s",as.character(r[2]),as.character(r[6]),fext)
  res <- try(if (!file.exists(outfile)) download.file(url = infile, destfile = outfile,mode='wb'))
  return(res[1])
}
dd3.function(cat[176,])
count<-1
amount<-1
chart3 <- cat[-c(1:3000),]
while (count<1001) {
  amount <- amount+1
  t <- dd3.function(cat[amount,])
  q <- grepl("Error",as.character(t))
  if(q == FALSE){
    chart3[count,] <- cat[amount,]
    count <- count+1
    
  }
}

write.csv(chart3,file="cat_summary.csv")
chart3$sequence_id


#download pics that gots wrong last time
SN_summary <- read.csv("SN_summary.csv", stringsAsFactors = F)
if (!dir.exists("pics_wrong")) dir.create("pics_wrong")
SN_summary <- left_join(SN_summary,fphotos,by = "photo_id" )
SN_summary$dirname <- gsub(pattern = "/home/stevens/misc/dbl0zz14/public_html/components/com_biodiv/uploads/",
                    replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",SN_summary$dirname)
SN_summary$dirname <- gsub(pattern = "/var/www/html/components/com_biodiv/uploads/",
                    replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",SN_summary$dirname)
SN_summary$dirname <- gsub(pattern = "/var/www/html/biodivimages/",
                    replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",SN_summary$dirname)
SN_summary$filename <- as.character(SN_summary$filename)
dd4.function <- function(r){
  infile <- sprintf("%s/%s",as.character(r[19]),as.character(r[17]))
  fext <- strsplit(as.character(r[17]),"\\.")[[1]][2]
  outfile <- sprintf("pics_wrong/%08s_%07s.%s",as.character(r[1]),as.character(r[2]),fext)
  res <- try(if (!file.exists(outfile)) download.file(url = infile, destfile = outfile,mode='wb'))
  return(res[1])
}
chart4 <- SN_summary[-c(1:493)]
for (i in 1:493) {
  if (SN_summary$AI[i] == 0){
    if (SN_summary$MW[i] == 1){
      t <- dd4.function(SN_summary[i,])
      chart4[i,] <- SN_summary[i,]
    }
  }
  
}
write.csv(chart4,file="pics_wrong_summary.csv")


# run the new animal classification
animal <- read.csv("Animal.csv", stringsAsFactors = F)
animal <- na.omit(animal)
animal = subset(animal, !animal$sequence_id%in%c("NULL"))
a1 <- animal %>% group_by(sequence_id) %>% do(head(., n = 2))

a0 <- as.data.frame(table(a1$sequence_id))
a0 <- subset(a0,a0$Freq < 2)
a1 <- subset(a1, !a1$sequence_id%in%c(a0$Var1))

a1 = subset(a1, !a1$sequence_id%in%c(small_mammal$sequence_id))
a1 = subset(a1, !a1$sequence_id%in%c(birds$sequence_id))
a1 = subset(a1, !a1$sequence_id%in%c(cat$sequence_id))
a1 = subset(a1, !a1$sequence_id%in%c(SN_summary$sequence_id))


a2 <- a1[seq(1,nrow(a1),2),]
a3 <- a1[seq(0,nrow(a1),2),]
a2$option_name_2 <- a3$option_name
a2$animal_id_2 <- a3$animal_id

#download 10000 pics
if (!dir.exists("pics_new")) dir.create("pics_new")
a2$dirname <- gsub(pattern = "/home/stevens/misc/dbl0zz14/public_html/components/com_biodiv/uploads/",
                           replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",a2$dirname)
a2$dirname <- gsub(pattern = "/var/www/html/components/com_biodiv/uploads/",
                           replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",a2$dirname)
a2$dirname <- gsub(pattern = "/var/www/html/biodivimages/",
                           replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",a2$dirname)
a2$filename <- as.character(a2$filename)
dd5.function <- function(r){
  infile <- sprintf("%s/%s",as.character(r[5]),as.character(r[4]))
  fext <- strsplit(as.character(r[4]),"\\.")[[1]][2]
  outfile <- sprintf("pics_new/%08s_%07s.%s",as.character(r[2]),as.character(r[1]),fext)
  res <- try(if (!file.exists(outfile)) download.file(url = infile, destfile = outfile,mode='wb'))
  return(res[1])
}
chart5 <- a2[-c(1:55128),]

dd5.function(a2[6917,])
count<-1
amount<-1

while (count<10001) {
  amount <- amount+1
  t <- dd5.function(a2[amount,])
  q <- grepl("Error",as.character(t))
  if(q == FALSE){
    chart5[count,] <- a2[amount,]
    count <- count+1
    
  }
}

training <- read.csv("Extras_for_training.csv", stringsAsFactors = F)
chart5 <- subset(chart5, !chart5$sequence_id%in%c(training$sequence_id))
chart5$sequence_id
library(stringr)
chart5$pics_name <- str_c(chart5$sequence_id,chart5$photo_id,sep = "_")
for (i in 1:9891) {
  if (chart5$option_name[i] == chart5$option_name_2[i]){
    chart5$SN_option[i] <- chart5$option_name[i] 
  }
 
}


write.csv(chart5,file="pics_new_summary.csv")

# make a contrast
shengyao <- read.csv("pics_shengyao_summary.csv", stringsAsFactors = F)
ai <- read.csv("Detections.csv", stringsAsFactors = F)
ai$Image.Name1<-gsub('^.{14}','',ai$Image.Name)
ai$Image.Name2 <- gsub('.{4}$','',ai$Image.Name1)
ai$pics_name <- gsub(" ", "", ai$Image.Name2,perl=T)
SY <- left_join(shengyao,ai,by = "pics_name")
SY_1<-data.frame(apply(SY,2,function(x){
  x[is.na(x)] = "Nothing";x}))
SY_2<-SY_1[,c(7,8,9,10,11,12,15)]

SY_2$Detections.[SY_2$Detections. == 'CapraHircus']<- 'Goat'
SY_2$Detections.[SY_2$Detections. == 'CapreolusCapreolus']<- 'Roe deer'
SY_2$Detections.[SY_2$Detections. == 'ErinaceusEuropaeus']<- 'Hedgehog (Western)'
SY_2$Detections.[SY_2$Detections. == 'MelesMeles']<- 'Badger'
SY_2$Detections.[SY_2$Detections. == 'OryctolagusCuniculus']<- 'Rabbit'
SY_2$Detections.[SY_2$Detections. == 'Person']<- 'Human'
SY_2$Detections.[SY_2$Detections. == 'PhasianusColchicus']<- 'Pheasant(common)'
SY_2$Detections.[SY_2$Detections. == 'SciurusCarolinensis']<- 'Grey squirrel'
SY_2$Detections.[SY_2$Detections. == 'VulpesVulpes']<- 'Red fox'
SY_2$Detections.[SY_2$Detections. == 'SciurusVulgaris']<- 'Red squirrel'
SY_2$Detections.[SY_2$Detections. == 'NA']<- 'Nothing'

write.csv(SY_2,file="contrast.csv")

SY <- read.csv("contrast_nobirds.csv", stringsAsFactors = F)




MW1 = subset(SY, SY$MW_class_1 == SY$AI_class)
MW2 = subset(SY, SY$MW_class_2 == SY$AI_class)
write.csv(MW1,file="MW1.csv")
write.csv(MW2,file="MW2.csv")
