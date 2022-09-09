
# download images from a list

# set working directory
setwd("E:/learning/durham/mds project")

# find list of required photos
fphotos <- read.csv("Photo.csv", stringsAsFactors = F)

# now make sure we have the right web addresses in the fphotos object
fphotos$dirname <- gsub(pattern = "/home/stevens/misc/dbl0zz14/public_html/components/com_biodiv/uploads/",
                        replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",fphotos$dirname)
fphotos$dirname <- gsub(pattern = "/var/www/html/components/com_biodiv/uploads/",
                        replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",fphotos$dirname)
fphotos$dirname <- gsub(pattern = "/var/www/html/biodivimages/",
                        replacement = "https://mammalweb.s3-eu-west-1.amazonaws.com/",fphotos$dirname)
fphotos$filename <- as.character(fphotos$filename)

# now, for every pic, download it to the "pics" folder
if (!dir.exists("pics1")) dir.create("pics1")

dd.function <- function(r){
  infile <- sprintf("%s/%s",as.character(r[4]),as.character(r[2]))
  fext <- strsplit(as.character(r[2]),"\\.")[[1]][2]
  outfile <- sprintf("pics1/%08d_%07d.%s",as.numeric(r[10]),as.numeric(r[1]),fext)
  res <- try(if (!file.exists(outfile)) download.file(url = infile, destfile = outfile,mode='wb'))
  return(res[1])
}

count<-1
amount<-20174
chart1 <- fphotos[-c(1:1738415),]
while (count<1001) {
  amount <- amount+1
  t <- dd.function(fphotos[amount,])
  q <- grepl("Error",as.character(t))
  if(q == FALSE){
    chart1[count,] <- fphotos[amount,]
    count <- count+1
    
  }
}
write.csv(chart1,file="data.csv")
chart1$sequence_id

data <- read.csv("data.csv", stringsAsFactors = F)
animal <- read.csv("Animal.csv", stringsAsFactors = F)
rm(list=ls())
animalstatistics <- read.csv("AnimalStatistics.csv", stringsAsFactors = F)

install.packages("dplyr")
library("dplyr")

animal <- animal[,-c(5:10)]
data1 <- left_join(data,animal,by = "photo_id")
table(duplicated(data1$photo_id))
table(duplicated(data1$species))
which(duplicated(data1$photo_id))
data2 <- data1[!duplicated(data1$photo_id),]
animalstatistics <- animalstatistics[,-c(1,4,5)]
names(data2)[21] <- "option_id"

data3 <- merge(data2,animalstatistics,by = "option_id")
table(duplicated(animalstatistics$species))
which(duplicated(animalstatistics$species))
animalstatistics <- animalstatistics[!duplicated(animalstatistics$option_id),]
data3 <- left_join(data2,animalstatistics,by = "option_id")
write.csv(data3,file="species.csv")

cs <- read.csv("species.csv", stringsAsFactors = F)
ai <- read.csv("Detections.csv", stringsAsFactors = F)
ai$Image.Name1 <- gsub('.{20}$','',ai$Image.Name)
ai$Image.Name2<-gsub('^.{9}','',ai$Image.Name1)
ai$Image.Name3<-gsub('^.{2}','',ai$Image.Name2)
ai$photo_id <- ai$Image.Name3
table(duplicated(ai$Image.Name))
which(duplicated(ai$Image.Name))
table(duplicated(ai$photo_id))
which(duplicated(ai$photo_id))
class(cs$photo_id)
ai <- ai[!duplicated(ai$photo_id),]
cs$photo_id <- as.character(cs$photo_id)
contrast <- left_join(cs,ai,by = "photo_id")
newcontrast <- contrast[,c(3,6,12,23,26)]
write.csv(newcontrast,file = "Chart.csv")

chart1 <- read.csv("Chart.csv", stringsAsFactors = F)
chart1[is.na(chart1)] <- "NA"
for(i in (1:1000)){
  if ( chart1[i,5] == "Nothing" 
       & chart1[i,6] == " NA " ){
    chart1[i,7] <- "Yes"
    chart1[i,8] <- "Nothing"
  }
}
 write.csv(chart1,file = "Chart.csv")

chart1 <- read.csv("Chart.csv", stringsAsFactors = F)

chart1[is.na(chart1)] <- "NA"
for(i in (1:1000)){
  if ( chart1[i,6] == "Rabbit" 
       & chart1[i,7] == " OryctolagusCuniculus " ){
    chart1[i,8] <- "Yes"
    chart1[i,9] <- "Rabbit"
  }
}

for(i in (1:1000)){
  if ( chart1[i,6] == "Badger" 
       & chart1[i,7] == " MelesMeles " ){
    chart1[i,8] <- "Yes"
    chart1[i,9] <- "Badger"
  }
}

for(i in (1:1000)){
  if ( chart1[i,6] == "Human" 
       & chart1[i,7] == " Person " ){
    chart1[i,8] <- "Yes"
    chart1[i,9] <- "Human"
  }
}

for(i in (1:1000)){
  if ( chart1[i,6] == "Red fox" 
       & chart1[i,7] == " VulpesVulpes " ){
    chart1[i,8] <- "Yes"
    chart1[i,9] <- "Red fox"
  }
}
write.csv(chart1,file = "Chart.csv")
chart <- read.csv("Chart.csv", stringsAsFactors = F)
chart$ConservationAI_species[chart$ConservationAI_species == ' CapraHircus ']<- 'Livestock'
chart$ConservationAI_species[chart$ConservationAI_species == ' CapreolusCapreolus ']<- 'Roe deer'
chart$ConservationAI_species[chart$ConservationAI_species == ' ErinaceusEuropaeus ']<- 'Hedgehog (Western)'
chart$ConservationAI_species[chart$ConservationAI_species == ' MelesMeles ']<- 'Badger'
chart$ConservationAI_species[chart$ConservationAI_species == ' OryctolagusCuniculus ']<- 'Rabbit'
chart$ConservationAI_species[chart$ConservationAI_species == ' Person ']<- 'Human'
chart$ConservationAI_species[chart$ConservationAI_species == ' PhasianusColchicus ']<- 'Pheasant'
chart$ConservationAI_species[chart$ConservationAI_species == ' SciurusCarolinensis ']<- 'Grey squirrel'
chart$ConservationAI_species[chart$ConservationAI_species == ' VulpesVulpes ']<- 'Red fox'
chart$ConservationAI_species[chart$ConservationAI_species == ' SciurusVulgaris ']<- 'Red squirrel'
chart$ConservationAI_species[chart$ConservationAI_species == ' NA ']<- 'Nothing'


chart[is.na(chart)] <- "NA"

vote_1 <- c()
vote_2 <- c()
vote1 <- as.data.frame(table(chart$sequence_id))
class(vote1[1,1])
#find the the most frequent element
for(i in (1:493)){
  if (vote1$Freq[i] == 1){
    vote_1 <- c(vote_1,as.numeric(as.character(vote1$Var1[i])))
  }
  else {
    vote_2 <- c(vote_2,as.numeric(as.character(vote1$Var1[i])))
  }
}
as.character(vote1$Var1[316])
vote_1 <- na.omit(vote_1)
vote_2 <- na.omit(vote_2)
length(vote_2)
length(vote_1)
class(vote_2)

3978890 %in% vote_2
class(397880)
class(vote_2)
#step for vote1
for (i in 1:1000) {
  if (chart$sequence_id[i] %in% vote_1){
    chart$Mammalweb_classification[i] <- chart$MammalWeb_species[i]
  }
  
}


Mostvotes <- function(seq_id){
  v <- c()
  v <- c(v,chart$MammalWeb_species[which(chart$sequence_id == seq_id)])
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Mostvotes(27173101)
Mostvotes(chart$sequence_id[843])
chart$Mammalweb_classification[843]
# step for vote2 1
for (i in 1:1000) {
  if (chart$sequence_id[i] %in% vote_2){
    if (chart$MammalWeb_species[i] == "NA"){
      chart$Mammalweb_classification[i] <- chart$MammalWeb_species[i-1]
    }
    else if (chart$MammalWeb_species[i] == "Nothing" ){
      chart$Mammalweb_classification[i] <- Mostvotes(chart$sequence_id[i])
    }
    else {
      chart$Mammalweb_classification[i] <- chart$MammalWeb_species[i]
    }
    
    
  }
  
}

#step for vote2 2
for (i in 2:1000) {
  if (chart$Mammalweb_classification[i] == "NA"){
    if (chart$sequence_id[i] %in% vote_2){
    chart$Mammalweb_classification[i] <- chart$MammalWeb_species[i]
  
    }
  }
  
}

#step for vote2 3
for (i in 2:1000) {
  if (chart$Mammalweb_classification[i] == "NA"){
    if (chart$sequence_id[i-1] %in% vote_2){
      chart$Mammalweb_classification[i] <- chart$Mammalweb_classification[i-1]
      
    }
  }
  
}

#step for step 2 4
for (i in 2:1000){
  if (chart$Mammalweb_classification[i] == "NA")
    chart$Mammalweb_classification[i] <- chart$Mammalweb_classification[i-1]
}
table(chart$Mammalweb_classification)
#export
write.csv(chart,file = "SN_Chart.csv")
chart <- read.csv("SN_Chart.csv", stringsAsFactors = F)

chart$My_species[chart$My_species == ' CapraHircus ']<- 'Livestock'
chart$My_species[chart$My_species == ' CapreolusCapreolus ']<- 'Roe deer'
chart$My_species[chart$My_species == 'ErinaceusEuropaeus ']<- 'Hedgehog (Western)'
chart$My_species[chart$My_species == ' MelesMeles ']<- 'Badger'
chart$My_species[chart$My_species == ' OryctolagusCuniculus ']<- 'Rabbit'
chart$My_species[chart$My_species == ' Person ']<- 'Human'
chart$My_species[chart$My_species == ' PhasianusColchicus ']<- 'Pheasant'
chart$My_species[chart$My_species == ' SciurusCarolinensis ']<- 'Grey squirrel'
chart$My_species[chart$My_species == ' VulpesVulpes ']<- 'Red fox'
chart$My_species[chart$My_species == ' SciurusVulgaris ']<- 'Red squirrel'
chart$My_species[chart$My_species == ' NA ']<- 'Nothing'

write.csv(chart,file = "SN_Chart.csv")

chart <- read.csv("SN_Chart.csv", stringsAsFactors = F)
rm(list=ls())
chart[is.na(chart)] <- "NA"
chart<-chart[,-1]
for(i in (1:1000)){
  if ( chart[i,6] == chart[i,7]){
    chart[i,8] <- "Yes"
    chart[i,9] <- chart[i,7]
    chart[i,10] <- "Both"
  }
  else {
    chart[i,8] <- "No"
  }
}

for(i in (1:1000)){
  if ( chart[i,8] == "No"){
    if (chart[i,9] == chart[i,7]){
      chart[i,10] <- "ConservationAI"
      
    }
    else {
      chart[i,10] <- "MammelWeb"
    }
  }
}
summary(chart$Who_is_right)
table(chart$Who_is_right)
write.csv(chart,file = "SN_Chart.csv")
table(chart$Same.)
rm(list=ls())
write.csv(chart,file = "SN_Chart.csv")

