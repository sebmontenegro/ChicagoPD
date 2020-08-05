setwd("~/Dropbox/Project/Code")
#I will load some functions to automate coding in R. (Don't forget to add "?raw=TRUE" at the end of the URL)
devtools::source_url("https://github.com/sebmontenegro/functions/blob/master/preload.R?raw=TRUE")

preload("tidyverse")
preload("readxl")
preload("dplyr")
preload("zoo")
preload("stringr")



# DATA --------------------------------------------------------------------
districts <- c(1:12,14:20,22,24,25)

allegations <- NULL
p.witnesses <- NULL
c.witnesses <- NULL
officer <- NULL

for(i in 1:length(districts)){
  allegations <- rbind.data.frame(allegations,
  cbind(read_excel(paste("~/Dropbox/Project/Data/Complaints/Dis_",districts[i],".xlsx",sep=""), sheet = 2),districts[i]))
  p.witnesses <- rbind.data.frame(p.witnesses,
  cbind(read_excel(paste("~/Dropbox/Project/Data/Complaints/Dis_",districts[i],".xlsx",sep=""), sheet = 3),districts[i]))
  c.witnesses <- rbind.data.frame(c.witnesses,
  cbind(read_excel(paste("~/Dropbox/Project/Data/Complaints/Dis_",districts[i],".xlsx",sep=""), sheet = 4),districts[i]))
  officer <- rbind.data.frame(officer,
  cbind(read_excel(paste("~/Dropbox/Project/Data/Complaints/Dis_",districts[i],".xlsx",sep=""), sheet = 5),districts[i]))
}
colnames(allegations)[ncol(allegations)] <- "District"
colnames(p.witnesses)[ncol(p.witnesses)] <- "District"
colnames(c.witnesses)[ncol(c.witnesses)] <- "District"
colnames(officer)[ncol(officer)] <- "District"
allegations <- allegations[!duplicated(allegations[names(allegations)]),]#Duplicates elimination
p.witnesses <- p.witnesses[!duplicated(p.witnesses[names(p.witnesses)]),]#Duplicates elimination
c.witnesses <- c.witnesses[!duplicated(c.witnesses[names(c.witnesses)]),]#Duplicates elimination
officer <- officer[!duplicated(officer[names(officer)]),]#Duplicates elimination

#Check variables
allegations %>%
  glimpse()
allegations$CRID <- as.numeric(allegations$CRID)
allegations$IncidentDate <- as.Date(allegations$IncidentDate)
allegations <- allegations[-which(is.na(allegations$IncidentDate)==TRUE),]
allegations$IncidentDate <- format(allegations$IncidentDate, "%Y-%m")
allegations$IncidentDate <- paste(allegations$IncidentDate,"-01",sep="")
allegations$StartDate <- format(as.Date(allegations$StartDate), "%Y-%m")
allegations$EndDate <- format(as.Date(allegations$EndDate), "%Y-%m")

x <- levels(factor(allegations$Outcome[grepl("Suspended for", allegations[["Outcome"]])]))
x <- str_split_fixed(x, "for ", n = 2)[, 2]
x <- str_split_fixed(x, " D", n = 2)[, 1]


for (i in 1:length(x)){
  allegations$Outcome[allegations$Outcome==paste("Suspended for",x[i],"Days")]=
  paste(x[i],"Day Suspension")
}

allegations$Outcome[allegations$Outcome=="Suspended over 30 Days"]=
  paste(30,"Day Suspension")

x <- levels(factor(allegations$Outcome[grepl("Day", allegations[["Outcome"]])]))
x <- str_split_fixed(x, " D", n = 2)[, 1]
x <- as.numeric(x)

tmp <- paste(x[which(x>=10&x<=19)],"Day Suspension")
for(i in 1:length(tmp)){
  allegations$Outcome[allegations$Outcome==tmp[i]] <- "10-19 Day Suspension"
}

tmp <- paste(x[which(x>=20&x<=29)],"Day Suspension")
for(i in 1:length(tmp)){
  allegations$Outcome[allegations$Outcome==tmp[i]] <- "20-29 Day Suspension"
}

tmp <- paste(x[which(x>=30&x<=59)],"Day Suspension")
for(i in 1:length(tmp)){
  allegations$Outcome[allegations$Outcome==tmp[i]] <- "30-59 Day Suspension"
}

tmp <- paste(x[which(x>=60&x<=99)],"Day Suspension")
for(i in 1:length(tmp)){
  allegations$Outcome[allegations$Outcome==tmp[i]] <- "60-99 Day Suspension"
}

tmp <- paste(x[which(x>=100)],"Day Suspension")
for(i in 1:length(tmp)){
  allegations$Outcome[allegations$Outcome==tmp[i]] <- "100+ Day Suspension"
}

rm(tmp)

p.witnesses %>%
  glimpse()
p.witnesses$CRID <- as.numeric(p.witnesses$CRID)

c.witnesses %>%
  glimpse()
c.witnesses$CRID <- as.numeric(c.witnesses$CRID)

officer %>%
  glimpse()
officer$ApptDate <- as.Date(officer$ApptDate)


#Now let's organize the data
tmp <- data.frame(table(officer$OfficerID))
names(tmp) <- c("OfficerID","DistrictsWorked")
officer <- merge(tmp,officer,by="OfficerID",all.x = T,all.y = T)
rm(tmp)
officer <- officer[which(duplicated(officer$OfficerID)==FALSE),]

allegations.multi <- allegations[order(allegations[,1],decreasing = F),] #This data set has unique event-officer observations
allegations <- allegations.multi[-which(duplicated(allegations.multi[,1])==TRUE),]
tmp <- data.frame(table(allegations.multi$CRID))
names(tmp) <- c("CRID","officers_involved")
allegations <- merge(allegations,tmp,by="CRID")
allegations.multi <- merge(allegations.multi,tmp,by="CRID")
rm(tmp)
allegations <- allegations[-which(is.na(allegations$Category)|is.na(allegations$Allegation)),]
allegations.multi <- allegations.multi[-which(is.na(allegations.multi$Category)|is.na(allegations.multi$Allegation)),]
rm(districts,x,i)

#Allegation/Officer/Police Witnesses/Complaining Witnesses
all <- merge(allegations.multi[,c(1:4,6,7,12,13,19,26,27)],
             officer[,-c(3,4,8,10,12)],by="OfficerID",all.x=F,all.y=F)#This data set is unique by CRID and OfficerID
tmp <- data.frame(table(p.witnesses$CRID))
names(tmp) <- c("CRID","officers_witness")
all <- merge(all,tmp,by="CRID",all.x = T,all.y = F)
rm(tmp)
all$officers_witness[is.na(all$officers_witness)] <- 0 #If NA, no officer witness

tmp <- data.frame(table(c.witnesses$CRID))
names(tmp) <- c("CRID","comp_witness")
c.witnesses <- merge(c.witnesses,tmp,by="CRID")
rm(tmp)
c.witnesses$random <- runif(nrow(c.witnesses),0,1)
c.witnesses <- c.witnesses[order(c.witnesses[,7]),]
c.witnesses.oneobs <- c.witnesses[-which(duplicated(c.witnesses[,1])==TRUE),]

all <- merge(all,c.witnesses.oneobs[,-c(4,5,7)],by="CRID",all.x = T,all.y = F)

all$comp_witness[is.na(all$comp_witness)] <- 0 #If NA, no complaining witness

all$Gender.y[is.na(all$Gender.y)] <- "No Info"
all$Race.y[is.na(all$Race.y)] <- "No Info"

names(all)[13] <- "OfficerGender"
names(all)[14] <- "OfficerRace"
names(all)[17] <- "OfficerAge"
names(all)[19] <- "WitnessGender"
names(all)[20] <- "WitnessRace"

all$IncidentDate <- as.Date(all$IncidentDate)

all <- all[!is.na(all$IncidentDate),]
all <- all[!is.na(all$ApptDate),]
all <- all[-which((all$IncidentDate<all$ApptDate)==TRUE),] #Removes obs for which allegation happened before appt date

all$IncidentDaysAfterAppt <- all$IncidentDate-all$ApptDate

all <- all[order(all[,2],all[,9]),] #Order by officer and date allegation occurred





tmp <- as.numeric(as.character(data.frame(table(all$OfficerID))[,1]))

all$allegation_count <- NA
for(i in 1:length(tmp)){
  print(i)
  all[which(all$OfficerID==tmp[i]),]$allegation_count <- 1:length(all[which(all$OfficerID==tmp[i]),]$allegation_count)
}

rm(i,tmp)
save.image("~/Dropbox/Project/Code/Data.RData") #Data saved up to this point

# PLOTS -------------------------------------------------------------------
load("~/Dropbox/Project/Code/Data.RData")
setwd("~/Dropbox/Project/Code/Plots")
#Total
png("plot1.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(IncidentDate>=as.Date("1999-01-01")) %>%
  count(Date = factor(IncidentDate)) %>%
  ggplot(aes(x=as.Date(Date),y=n))+
  geom_line(color="skyblue4")+
  #geom_bar(stat = "identity")
  theme_bw()+
  labs(x ="", y = "Number of Allegations")#+geom_vline(xintercept = as.Date("2018-08-01"))
dev.off()

#By district
png("plot2.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(IncidentDate>=as.Date("1999-01-01")) %>%
  count(Date = factor(IncidentDate),District = factor(District)) %>%
  ggplot(aes(x=as.Date(Date),y=n))+
  geom_line(color="skyblue4")+
  facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="", y = "Number of Allegations")
dev.off()

png("plot3.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(IncidentDate>=as.Date("1999-01-01")) %>%
  count(District = factor(District)) %>%
  ggplot(aes(x=reorder(District, -n),y=n))+
  geom_bar(stat = "identity",fill = "skyblue4")+
  theme_bw()+
  labs(x ="District", y = "Number of Allegations")
dev.off()




#Categories
png("plot4_1.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(Finding!="Unknown") %>%
  count(Category = Category, Finding = Finding) %>%
  filter(n>=15) %>%
  ggplot(aes(x=reorder(Category,-n),y=n,fill=Finding))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Category", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()

png("plot4_2.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(Finding!="Unknown") %>%
  count(Category = Category, Finding = Finding, District = District) %>%
  filter(n>=15) %>%
  ggplot(aes(x=reorder(Category,-n),y=n,fill=Finding))+
  geom_bar(stat = "identity")+ facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Category", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()

#Allegation by category
png("plot5.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(Finding!="Unknown"&Allegation!="Miscellaneous") %>%
  count(Allegation = Allegation, Category = Category) %>%
  filter(n>=230) %>%
  arrange(desc(n)) -> tmp
  
  tmp %>%
  ggplot(aes(x=reorder(Allegation,-n),y=n,fill=Category))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Type", y = "Number of Allegations")+coord_flip()+
  #scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()


#Allegation by finding
png("plot6.png",width = 11.69,height = 8.27 ,units = "in",res = 300)
merge(tmp[,-3],
allegations %>%
  filter(Finding!="Unknown"&Allegation!="Miscellaneous") %>%
  count(Finding = Finding, Allegation = Allegation),
by="Allegation",all.x = T,all.y = F) %>%
  ggplot(aes(x=factor(Allegation,levels = tmp$Allegation),y=n,fill=Finding))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Category", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()






#Allegation (most freq categ)
png("plot6.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(Category==levels(forcats::fct_infreq(allegations$Category))[1]&Finding!="Unknown") %>%
  count(Allegation = Allegation,Finding=Finding) %>%
  filter(n>=1) %>%
  ggplot(aes(x=reorder(Allegation,-n),y=n,fill=Finding))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Type", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()

#Allegation (second most freq categ)
png("plot7.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(Category==levels(forcats::fct_infreq(allegations$Category))[2]&Finding!="Unknown") %>%
  count(Allegation = Allegation,Finding=Finding) %>%
  filter(n>=1) %>%
  ggplot(aes(x=reorder(Allegation,-n),y=n,fill=Finding))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Type", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()

#Outcome by finding
png("plot8.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
allegations %>%
  filter(Outcome!="Unknown"&
           #Finding!="No Affidavit"&
           #Finding!="No Cooperation"&
           Finding!="Unknown") %>%
  count(Outcome = factor(Outcome),Finding = factor(Finding))%>%
  group_by(Outcome) %>%
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x=factor(Outcome,level=levels(forcats::fct_infreq(allegations$Outcome))),y=pct,fill=Finding))+
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct*100,0),"%")),position = position_stack(vjust = 0.5), size = 2)+
  theme_bw() + scale_fill_brewer(palette="Dark2")+
  labs(x ="Outcome", y = "Percentage of Allegations")+ coord_flip()+
  theme(legend.position="bottom") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0))
dev.off()


png("plot9.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
all %>%
  filter(Outcome!="Unknown"&
           WitnessRace!="No Info"&
           OfficerRace!="Unknown"&
           Finding!="Unknown") %>%
  mutate(Int = paste0(OfficerRace,"-",WitnessRace)) %>%
  count(Int = Int) %>%
  ggplot(aes(x=reorder(Int,-n),y=n))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Race: Officer-Witness", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
dev.off()


png("plot10.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
all %>%
  filter(Outcome!="Unknown"&
           WitnessRace!="No Info"&
           OfficerRace!="Unknown"&
           Finding!="Unknown") %>%
  mutate(Int = paste0(OfficerRace,"-",WitnessRace)) %>%
  count(Int = factor(Int),Finding = factor(Finding))%>%
  group_by(Int) %>%
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x=factor(Int,level=levels(forcats::fct_infreq(paste0(all$OfficerRace,"-",all$WitnessRace)))),y=pct,fill=Finding))+
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct*100,0),"%")),position = position_stack(vjust = 0.5), size = 2)+
  theme_bw() + scale_fill_brewer(palette="Dark2")+
  labs(x ="Outcome", y = "Percentage of Allegations")+ coord_flip()+
  theme(legend.position="bottom") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0))
dev.off()


png("plot11.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
a <- all %>%
  filter(Outcome!="Unknown"&
           WitnessRace=="Black"&
           OfficerRace!="Unknown"&
           Finding=="Sustained") %>%
  count(Allegation = factor(Allegation)) %>%
  arrange(desc(n)) %>%
  slice_head(n=10)

b <- all %>%
  filter(Outcome!="Unknown"&
           WitnessRace=="Black"&
           OfficerRace!="Unknown"&
           Finding=="Sustained") %>%
  mutate(Int = paste0(OfficerRace,"-",WitnessRace)) %>%
  count(WitnessRace = factor(WitnessRace),Int = factor(Int),Allegation = factor(Allegation))

c <- merge(a,b,by="Allegation",all.x = T,all.y = F)


c %>%
  ggplot(aes(x=factor(Allegation,levels = c(as.character(a$Allegation))),y=n.y,fill=Int))+
  geom_bar(stat = "identity") +
  theme_bw() + #scale_fill_brewer(palette="Dark2")+
  labs(x ="Outcome", y = "Percentage of Allegations")+ coord_flip()+
  theme(legend.position="bottom") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0))
dev.off()



png("plot12.png",height = 11.69,width = 8.27 ,units = "in",res = 300)
a <- all %>%
  filter(Outcome!="Unknown"&
           WitnessRace=="White"&
           OfficerRace!="Unknown"&
           Finding=="Sustained") %>%
  count(Allegation = factor(Allegation)) %>%
  arrange(desc(n)) %>%
  slice_head(n=10)

b <- all %>%
  filter(Outcome!="Unknown"&
           WitnessRace=="White"&
           OfficerRace!="Unknown"&
           Finding=="Sustained") %>%
  mutate(Int = paste0(OfficerRace,"-",WitnessRace)) %>%
  count(WitnessRace = factor(WitnessRace),Int = factor(Int),Allegation = factor(Allegation))

c <- merge(a,b,by="Allegation",all.x = T,all.y = F)


c %>%
  ggplot(aes(x=factor(Allegation,levels = c(as.character(a$Allegation))),y=n.y,fill=Int))+
  geom_bar(stat = "identity") +
  theme_bw() + #scale_fill_brewer(palette="Dark2")+
  labs(x ="Outcome", y = "Percentage of Allegations")+ coord_flip()+
  theme(legend.position="bottom") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0))
dev.off()


