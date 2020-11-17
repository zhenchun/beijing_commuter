
require(dplyr)
require(ggplot2)
require(rcompanion)
require(pairwiseCI)


setwd("C:/Users/zy125/Box Sync/PhD/Beijing")

##############################################################################
#############load bus data###################################################
###############################################################################

path <- "C:/Users/zy125/Box Sync/PhD/Beijing/bus"
file.names <- dir(path, pattern =".txt")
setwd(path)
bus <- list()

for (f in file.names) {
  print(f)
  MDdat2 <- importMD_dm(f)
  MDdat2filt <- filterMDv4(MDdat2[[1]])
  bus[[f]] <- MDdat2filt[[1]][, c(1,4,5,6,7,8,16)]
}


for (n in 1:18){
  bus[[n]]['id'] = n
}
bus.all = do.call(rbind, bus)

bus_summary<-bus.all %>%
  group_by(id) %>%
  summarize(a.mean = mean(Number), g.mean=gm_mean(Number),median=median(Number), 
            max=max(Number), min=min(Number), sd=sd(Number),q5=quantile(Number, probs = 0.05),
            q25=quantile(Number, probs = 0.25), q75=quantile(Number, probs = 0.75),
            q95=quantile(Number, probs = 0.95), length=length(Number))



bus_summary_size<-bus.all %>%
  group_by(id) %>%
  dplyr::summarize(mean = mean(Size),median=median(Size), 
                   max=max(Size), min=min(Size))



#################################################################################
###########################load car data######################################### 
################################################################################


path <- "C:/Users/zy125/Box Sync/PhD/Beijing/car"
file.names <- dir(path, pattern =".txt")
setwd(path)
car <- list()

for (f in file.names) {
  print(f)
  MDdat2 <- importMD_dm(f)
  MDdat2filt <- filterMDv4(MDdat2[[1]])
  car[[f]] <- MDdat2filt[[1]][, c(1,4,5,6,7,8,16)]
}


for (n in 1:16){
  car[[n]]['id'] = n
}
car.all = do.call(rbind, car)

car_summary<-car.all %>%
  group_by(id) %>%
  summarize(a.mean = mean(Number), g.mean=gm_mean(Number),median=median(Number), 
            max=max(Number), min=min(Number), sd=sd(Number),q5=quantile(Number, probs = 0.05),
            q25=quantile(Number, probs = 0.25), q75=quantile(Number, probs = 0.75),
            q95=quantile(Number, probs = 0.95), length=length(Number))

car_summary_size<-car.all %>%
  group_by(id) %>%
  dplyr::summarize(mean = mean(Size),median=median(Size), 
                   max=max(Size), min=min(Size))

######################################################################################################
#################################load bicycle data#####################################################
########################################################################################################


path <- "C:/Users/zy125/Box Sync/PhD/Beijing/bicycle"
file.names <- dir(path, pattern =".txt")
setwd(path)
bicycle <- list()

for (f in file.names) {
  print(f)
  MDdat2 <- importMD_dm(f)
  MDdat2filt <- filterMDv4(MDdat2[[1]])
  bicycle[[f]] <- MDdat2filt[[1]][, c(1,4,5,6,7,8,16)]
}


for (n in 1:10){
  bicycle[[n]]['id'] = n
}
bicycle.all = do.call(rbind, bicycle)

bicycle_summary<-bicycle.all %>%
  group_by(id) %>%
  summarize(a.mean = mean(Number), g.mean=gm_mean(Number),median=median(Number), 
            max=max(Number), min=min(Number), sd=sd(Number),q5=quantile(Number, probs = 0.05),
            q25=quantile(Number, probs = 0.25), q75=quantile(Number, probs = 0.75),
            q95=quantile(Number, probs = 0.95), length=length(Number))


bicycle_summary_size<-bicycle.all %>%
  group_by(id) %>%
  dplyr::summarize(mean = mean(Size),median=median(Size), 
                   max=max(Size), min=min(Size))

###################################################################################################
################load subway data###################################################################
###################################################################################################


path <- "C:/Users/zy125/Box Sync/PhD/Beijing/output/subway"
file.names <- dir(path, pattern =".txt")
setwd(path)
subway<- list()

for (f in file.names) {
  print(f)
  MDdat2 <- importMD_dm(f)
  MDdat2filt <- filterMDv4(MDdat2[[1]])
  subway[[f]] <- MDdat2filt[[1]][, c(1,4,5,6,7,8,16)]
}


for (n in 1:21){
  subway[[n]]['id'] = n
}
subway.all = do.call(rbind, subway)


##############################################################################################################################
#subway data id==7&12 have small amount of data available. Thus, deleted.id=14 has strange data inclusion. deleted as well.###
###############################################################################################################################
subway.all1<-subway.all[-which(subway.all$id==7 | subway.all$id==12| subway.all$id==14),]

subway_summary<-subway.all1 %>%
  group_by(id) %>%
  dplyr::summarize(a.mean = mean(Number), g.mean=gm_mean(Number),median=median(Number), 
                   max=max(Number), min=min(Number), sd=sd(Number),q5=quantile(Number, probs = 0.05),
                   q25=quantile(Number, probs = 0.25), q75=quantile(Number, probs = 0.75),
                   q95=quantile(Number, probs = 0.95), length=length(Number))

subway_summary_size<-subway.all1 %>%
  group_by(id) %>%
  dplyr::summarize(mean = mean(Size),median=median(Size), 
                   max=max(Size), min=min(Size))


##############################################################################
##################combine travel modes#######################################
##################################################################################

bus$type<-"bus"
bus<-cbind(bus_info, bus_summary)

car$type<-"car"
car<-cbind(car_info, car_summary)


bicycle$type<-"bicycle"
bicycle<-cbind(bicycle_info, bicycle_summary)


subway$type<-"subway"
subway<-cbind(subway_info, subway_summary)


beijing<-rbind(bus, car, bicycle, subway)

######plot####################################################################################

tiff("Figure1.tiff", units="in", width=8, height=6, res=300)

ggplot(beijing_3, aes( type, mean, fill=pattern))+geom_boxplot()+theme_bw()+
  scale_fill_manual(values=c("#336699", "#cc9999"))+ 
  scale_x_discrete(labels= c("Bicycle (n=10)", "Bus (n=18)","Car (n=16)","Subway (n=19)"))+labs(x=NULL, y=expression(paste("UFP Number Concentration (Particle Number / ", cm^3, ")")), fill="Averages Type")+
  theme(text = element_text(size=14),legend.position="bottom")
dev.off()



beijing.peak<-beijing[which(beijing$pattern=="peak"),]

ggplot(beijing.peak, aes(type, a.mean))+geom_boxplot()+theme_bw()


beiing_count_peak<-beijing.peak %>%
  group_by(type) %>% 
  dplyr::summarize(n= n(), mean=mean(a.mean) )

ggplot(subway_vild, aes(a.mean,pattern))+geom_boxplot()


ggplot(beijing.peak, aes(direction, a.mean))+geom_boxplot()


beijing.peak %>%
  group_by(type) %>% 
  dplyr::summarize(n= n(), mean=mean(a.mean),con.sd=sd(a.mean),
                   min=min(a.mean), median=median(a.mean),
                   max=max(a.mean), 
                   len=mean(length), len.sd=sd(length))

beijing.peak %>%
  group_by(type) %>% 
  dplyr::summarize(n= n(), mean=mean(g.mean),con.sd=sd(g.mean),
                   min=min(g.mean), median=median(g.mean),
                   max=max(g.mean), 
                   len=mean(length), len.sd=sd(length))

beijing %>%
  group_by(type, period, pattern) %>% 
  dplyr::summarize(n= n(), total=sum(length), 
                   len=mean(length), len.sd=sd(length))

beijing%>%
  group_by(type) %>% 
  dplyr::summarize(n= n(), total=sum(length), 
                   len=mean(length), len.sd=sd(length))
beijing %>%
  group_by(type, date) %>% 
  dplyr::summarize(n=count(date))

beijing %>%
  group_by(period, type) %>% 
  dplyr::summarize(n= n())

subway.min.num<-subway.all1 %>%
  group_by(time = cut(PosixTime, breaks="1 min")) %>%
  dplyr::summarize(con = mean(Number), id2=unique(id))

subway_micro_summary$environment<-factor(subway_micro_summary$environment, levels= c("zgc", "underground", "transfer","overground", "sd") )

subway_micro_summary$type[subway_micro_summary$environment== "zgc"|subway_micro_summary$environment== "sd"]<-"Entrance"

subway_micro_summary$type[subway_micro_summary$environment== "underground"|subway_micro_summary$environment== "overground"]<-"Carriage"

subway_micro_summary$type[subway_micro_summary$environment== "transfer"]<-"Tunnel"


tiff("Figure2.tiff", units="in", width=8, height=6, res=300)

ggplot(subway_micro_summary, aes(environment,a.mean, fill=type))+geom_boxplot()+theme_bw()+
  labs(x=NULL, y=expression(paste("UFP Number Concentration (Particle Number / ", cm^3, ")")), fill="Micro-Environments Type")+
  scale_x_discrete(labels= c("ZGC (n=19)", "Underground (n=19)","Transfer (n=19)","Overground (n=19)", "SD (n=19)"))+
  theme(text = element_text(size=14),legend.position="bottom")
dev.off()


subway_micro_summary<-subway_micro %>%
  group_by(environment, journey) %>%
  dplyr::summarize(n=n(), a.mean = mean(con), g.mean=gm_mean(con),length=length(con))


subway_micro_summary_envir<-subway_micro_summary %>%
  group_by(environment) %>%
  summarize(time=mean(length), amean = mean(a.mean), amax=max(a.mean),
            amin=min(a.mean),gmean=mean(g.mean),gmax=max(g.mean), gmin=min(g.mean),n=n())





tem_hum_bp_summary<-tem_hum_bp %>%
  summarize(tem.mean=mean(TEMP), tem.min=min(TEMP), tem.max=max(TEMP), tem.sd=sd(TEMP),
            rh.mean=mean(RH), rh.min=min(RH), rh.max=max(RH), rh.sd=sd(RH),
            bp.mean=mean(BP), bp.min=min(BP), bp.max=max(BP), bp.sd=sd(BP))




wd_ws_1$hour<-cut(wd_ws_1$date, breaks="hour")

wd_ws_hour<-wd_ws_1 %>% 
  group_by(hour) %>% 
  dplyr::summarize(wd=mean(WD), ws=mean(WS))



beijing_amean<-beijing[, c(1,3,4,5,6,7,8,9,10)]
beijing_gmean<-beijing[, c(1,3,4,5,6,7,8,9,11)]


colnames(beijing_amean)[9]<-"mean"
beijing_amean$average<-"arithmetic"
colnames(beijing_gmean)[9]<-"mean"
beijing_gmean$average<-"geometric"
beijing_1<-rbind(beijing_amean, beijing_gmean)


pairwiseCI(a.mean~type, data=commuter, method="Param.diff")

ci.ratio<-pairwiseCI(a.mean~type, data=commuter, method="Param.ratio")

write.csv(ci.ratio, "ci.ratio.csv")


pd <- position_dodge(width = 1)


tiff("Figure3.tiff", units="in", width=8, height=6, res=300)

ggplot(CI, aes(y=estimate, x=comparison))+
  geom_pointrange(aes(ymax=upper, ymin=lower), position = pd, size=0.8)+
  geom_hline(yintercept = 1)+theme_classic()+
  labs(x=NULL, y="Ratio")+theme(text = element_text(size=14))
dev.off()


file.name<- dir(getwd(), pattern =".txt")
col_md<-importMD_dm(file="5519I422_output.txt")
col_md_filt<- filterMDv4(col_md[[1]])
col_md_dat <- col_md_filt[[1]][, c(1,4,5)]


col_md_min<-col_md_dat %>%
  group_by(time = cut(PosixTime, breaks="1 mins")) %>%
  summarize(con = mean(Number))

col_md_min$five<- as.POSIXct(ceiling(as.numeric(col_md_min$PosixTime)/(5 * 60))*(5*60), origin='1970-01-01')
col_md_five <- aggregate(con ~ five, col_md_min, mean)


PNSD <- read_csv("~/Box Sync/Beijing/bj_data/PNSD.csv", col_types = cols(`NaN` = col_datetime(format = "%d/%m/%Y %H:%M:%S")))

ref_dat<-PNSD[,c(1,2)]
colnames(ref_dat)<-c("time","number")

ref_col<-ref_dat[ref_dat$time>="2015-05-19 09:45:00" & ref_dat$time<= "2015-05-19 12:35:00",]

colnames(ref_dat)<-c("time","number")
col<-merge(ref_col, col_md_five, by="time")


ggplot(col, aes(number.x, number.y))+geom_point()+
  geom_smooth(method=lm, se=FALSE)


#10.7-600 nm
PNSD3 <- read_csv("~/Box Sync/Beijing/bj_data/PNSD3.csv", col_types = cols(`NaN` = col_datetime(format = "%d/%m/%Y %H:%M:%S")))

ref_dat3<-PNSD3[,c(1,46)]
colnames(ref_dat3)<-c("time","number")

ref_col3<-ref_dat3[ref_dat3$time>="2015-05-19 09:45:00" & ref_dat3$time<= "2015-05-19 12:35:00",]


col3<-merge(ref_col3, col_md_five, by="time")


#10.7-300 nm
PNSD4 <- read_csv("C:/Users/zy125/Box Sync/PhD/Beijing/bj_data/10_300.csv", col_types = cols(`NaN` = col_datetime(format = "%d/%m/%Y %H:%M:%S")))

ref_dat4<-PNSD4[,c(1,44)]
colnames(ref_dat4)<-c("time","number")

ref_col4<-ref_dat4[ref_dat4$time>="2015-05-19 09:45:00" & ref_dat4$time<= "2015-05-19 12:35:00",]


col4<-merge(ref_col4, col_md_five, by="time")


#08/31/2020 updated to include the 10nm-300nm data as reference data
ref_dat_hr<-ref_dat4 %>%
  group_by(time = cut(time, breaks="1 hour")) %>%
  dplyr::summarize(con = mean(number), gmean=gm_mean(number))

#journey data include all commuter types

journey_bicycle<-journey[which(journey$type=="bicycle"),]

cor(journey_bicycle$pek.a.mean, journey_bicycle$g.mean, use="complete.obs")


journey1<-journey[-c(61,62),] #remove nas





sum_ratio<-groupwiseMean (ratio ~ type, data=journey1, conf=0.95, digits=3)

ggplot(sum_ratio, aes(x = Mean, y = type))+geom_errorbar(aes(xmin = Trad.lower,
                                                            xmax = Trad.upper),
                                                        width = 0.05, size  = 1) +
  geom_point(shape = 15,size  = 4) + theme_bw() + theme(axis.title   = element_text(face  = "bold")) +
  
  ylab("Commuter Types")+geom_vline(xintercept=1)+theme_classic()








