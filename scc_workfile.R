########################################################
##
##
##
##
##
##
##
##
##
##
########################################################
#data formatting
require(reshape2)
require(plyr)
require(sqldf)
#graphics
require(ggplot2)
require(GGally)
require(scales)
#modeling packages
require(lme4)#linear mixed effects models
require(ordinal)#ordinal modeling package
require(arm)#ordinal modeling package
# require(boot)
# require(compiler)
# require(parallel)


setwd('~/Projects/SCC') #set working dir to appropriate location

###READ AND PROCESS DATA
scc.data <- read.csv('SCC_DB.csv') #read scc data

#Column names
col.labels <- c('dem.num','res.num','date.adopt','new.res.strict','year.adopt','new.res',
                'new.dem','civil.war','sit.type','region','start.date','end.date','duration',
                'continuation','post','intensity','num.war.parties','foreign','battle.deaths',
                'pol.instab.1.yr','pol.instab.3.yr','pol.shock','MAGFAIL','MAGFAIL.lag',
                'MAGFIGHT','MAGFIGHT.lag','MAGAREA','MAGAREA.lag','non.renew.nat.res',
                'lootable.nat.res','proximate.civil.war','proximate.civil.war.num',
                'depth.of.demand.final','depth.of.demand.coder.1','depth.of.demand.coder.2',
                'sub.org.sanctions.committee','sub.org.ajudicative.com',
                'sub.org.investigative.trib','sub.org.crim.trib','sub.org.other.commission',
                'sub.org.old.new','SC.mission','field.pres.trad.peace.op',
                'field.pres.multi.peace.op','field.pres.civ.peace.building',
                'field.pres.transitional.admin','field.pres.civ.pol','field.pres.civ.obvs',
                'field.pres.commission','field.pres.consular.commission','field.pres.SRSG',
                'field.pres.UN.mediation','field.pres.panel.of.experts','field.pres.ICC',
                'field.pres.old.new','peace.op','peace.op.old.new','UN.armed','UN.unarmed',
                'auth.non.UN','unauth.non.UN','SC.seizure.relationship','SC.seizure.period',
                'P5.consensus.2.year','P5.consensus.4.year','prior.res.situation',
                'prior.res.demand.full','prior.res.demand.part','prior.consent',
                'prior.SG.report','res.intro.relationship','abstentions.CHI','absentions.FRA',
                'absentions.RUS','absentions.UKG','absentions.USA','E10.negative',
                'absent.vote','yes.vote','absention.vote','no.vote','mandatory.horatory',
                'dem.type.military.conduct','dem.type.humanitarian','dem.type.governence',
                'dem.type.external','dem.type.cooperation.with.UN','functional.req',
                'characterizationint.peace.and.security','ref.op.chp.VI','ref.op.chp.VII',
                'ref.op.chp.VIII','ref.pream.chp.VII','ref.pream.chp.VIII',
                'ref.charter.article','ref.other.treaties','incentives.benefits',
                'threat.benefits','threat.force','sanctions.none','sanctions.imposed',
                'sanctions.previous','sanctions.threat','sanctions.threat.other',
                'sanct.type.general.trade','sanct.type.arms','sanct.type.oil',
                'sanct.type.natural.resource','sanct.type.financial','sanct.type.travel',
                'sanct.type.targeted','sanct.type.diplomatic','monit.mech.council',
                'monit.mech.commission','monit.mech.consular.commission',
                'monit.mech.subcommission','monit.mech.committee','monit.mech.subcommittee',
                'monit.mech.panel.of.experts','monit.mech.UN.field','monit.mech.SC',
                'monit.mech.pres.SC','monit.mech.SG','monit.mech.other.UN',
                'monit.mech.other.int.org','monit.mech.state',
                'monit.mech.fact.finding.mission','monit.mech.monitoring.mech.old.new',
                'monitoring.mech.dummy','sg.report.request','UN.organ.report.request',
                'member.state.report.request','all.states.report.request',
                'regional.org.report.request','multilateral.peace.force.report.request',
                'sunset.clause','short.term.final','medium.term.final','short.term.coder.1',
                'medium.term.coder.1','c','medium.term.coder.2')

#Remove extraneous data
scc.data.trimmed <- scc.data[,c(1:2,4,6,16,7:8,14,17:69,71,73:80,82:93,114:173)] #removed text of demand, coder and participant variables

#Bind column names
colnames(scc.data.trimmed) <- col.labels

#truncate at 2003
scc.data.trimmed <- scc.data.trimmed[which(scc.data.trimmed$year.adopt < 2004),]

#ID's amd non-factors
scc.data.trimmed$dem.num <- prettyNum(scc.data.trimmed$dem.num,digits=10)#variable tags
scc.data.trimmed$res.num <- as.factor(scc.data.trimmed$res.num)#resolution tags
scc.data.trimmed$date.adopt <- as.Date(scc.data.trimmed$date.adopt, "%m/%d/%y")#date adopted
scc.data.trimmed$start.date <- as.Date(scc.data.trimmed$start.date,"%m/%d/%y")#start date of war
scc.data.trimmed$end.date <- as.Date(scc.data.trimmed$end.date,"%m/%d/%y")#end date of war

#iterate through factor variables
array <- c(4:10,15,18,20:31,33:55,62,64:65,68:76,83:142) #all factor variables
tmp.df <- scc.data.trimmed[,-array] #partition factor and non-factor variables
namelist <- colnames(tmp.df) #column names

for(i in 1:length(array)) {
  x <- array[i]
  tmp <- as.factor(scc.data.trimmed[,x])
  name <- colnames(scc.data.trimmed)[x]
  namelist <- c(namelist,name)
  tmp.df <- cbind(tmp.df,tmp)
}

colnames(tmp.df) <- namelist #add column names
scc.data.trimmed <- tmp.df
#remove cruft
rm(tmp.df)
rm(array)
rm(col.labels)
rm(i)
rm(name)
rm(namelist)
rm(tmp)
rm(x)


###GENERATE FIGURES
data.plot <- scc.data.trimmed[,c(3,9,28,31,33,48,83:87,137:138)]

#generate index numbers of variable of interest
year <- which(colnames(data.plot)=="year.adopt")
short <- which(colnames(data.plot)=="short.term.final")
medium <- which(colnames(data.plot)=="medium.term.final")
mil <- which(colnames(data.plot)=="dem.type.military.conduct")
hum <- which(colnames(data.plot)=="dem.type.humanitarian")
gov <- which(colnames(data.plot)=="dem.type.governence")
ext <- which(colnames(data.plot)=="dem.type.external")
coop <- which(colnames(data.plot)=="dem.type.cooperation.with.UN")
dod <- which(colnames(data.plot)=="depth.of.demand.final")
factions <- which(colnames(data.plot)=="num.war.parties")
date <- which(colnames(data.plot)=="date.adopt")
war.name <- which(colnames(data.plot)=="civil.war")
region <- which(colnames(data.plot)=="region")


#Figure 1 - Variation in civil war parties compliance with demands of the Security Council over time
figure.1.data <- data.plot[,c(year,short,medium)]
figure.1.data <- ddply(figure.1.data, .(year.adopt), summarise,
                       mean.shrt = mean(as.numeric(short.term.final),na.rm=TRUE),
                       mean.med = mean(as.numeric(medium.term.final),na.rm=TRUE))
figure.1.data <- melt(figure.1.data[1:15,],id='year.adopt')
figure.1.plot <- ggplot(figure.1.data,aes(as.Date(figure.1.data$year.adopt, "%Y"),value))
figure.1.plot <- figure.1.plot + geom_line(aes(color=variable))
figure.1.plot <- figure.1.plot + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
figure.1.plot <- figure.1.plot + labs(title = 'Compliance Over Time',x='Year',y='Mean compliance score')
figure.1.plot <- figure.1.plot + scale_y_continuous(limits=c(1,4)) + theme_bw()

#Figure 2 - Variation in civil-war parties compliance with demands of the Security Council by the thematic type of the demand
figure.2.data <-  data.plot[,c(mil,hum,gov,ext,coop,dod,short,medium)] #extract data
#convert factors to numerics
figure.2.data[,6:8] <- apply(figure.2.data[,6:8], 2, function(x) as.numeric(as.character(x)))
#separate component thematic categories
military <- figure.2.data[which(figure.2.data$dem.type.military.conduct == 1),6:8]
humanitarian <- figure.2.data[which(figure.2.data$dem.type.humanitarian == 1),6:8]
governance <- figure.2.data[which(figure.2.data$dem.type.governence == 1),6:8]
external <- figure.2.data[which(figure.2.data$dem.type.external == 1),6:8]
cooperation <- figure.2.data[which(figure.2.data$dem.type.cooperation.with.UN == 1),6:8]
#summarise
military <- ddply(military, .(), summarise,
                  mean.dod = mean(depth.of.demand.final,na.rm=TRUE),
                  sd.dod = sd(depth.of.demand.final,na.rm=TRUE),
                  mean.shrt = mean(short.term.final,na.rm=TRUE),
                  sd.shrt = sd(short.term.final,na.rm=TRUE),
                  mean.med = mean(medium.term.final,na.rm=TRUE),
                  sd.med = sd(medium.term.final,na.rm=TRUE))
humanitarian <- ddply(humanitarian, .(), summarise,
                      mean.dod = mean(depth.of.demand.final,na.rm=TRUE),
                      sd.dod = sd(depth.of.demand.final,na.rm=TRUE),
                      mean.shrt = mean(short.term.final,na.rm=TRUE),
                      sd.shrt = sd(short.term.final,na.rm=TRUE),
                      mean.med = mean(medium.term.final,na.rm=TRUE),
                      sd.med = sd(medium.term.final,na.rm=TRUE))
governance <- ddply(governance, .(), summarise,
                    mean.dod = mean(depth.of.demand.final,na.rm=TRUE),
                    sd.dod = sd(depth.of.demand.final,na.rm=TRUE),
                    mean.shrt = mean(short.term.final,na.rm=TRUE),
                    sd.shrt = sd(short.term.final,na.rm=TRUE),
                    mean.med = mean(medium.term.final,na.rm=TRUE),
                    sd.med = sd(medium.term.final,na.rm=TRUE))
external <- ddply(external, .(), summarise,
                  mean.dod = mean(depth.of.demand.final,na.rm=TRUE),
                  sd.dod = sd(depth.of.demand.final,na.rm=TRUE),
                  mean.shrt = mean(short.term.final,na.rm=TRUE),
                  sd.shrt = sd(short.term.final,na.rm=TRUE),
                  mean.med = mean(medium.term.final,na.rm=TRUE),
                  sd.med = sd(medium.term.final,na.rm=TRUE))
cooperation <- ddply(cooperation, .(), summarise,
                     mean.dod = mean(depth.of.demand.final,na.rm=TRUE),
                     sd.dod = sd(depth.of.demand.final,na.rm=TRUE),
                     mean.shrt = mean(short.term.final,na.rm=TRUE),
                     sd.shrt = sd(short.term.final,na.rm=TRUE),
                     mean.med = mean(medium.term.final,na.rm=TRUE),
                     sd.med = sd(medium.term.final,na.rm=TRUE))
#ids
military$.id <- 'Military'
humanitarian$.id <- 'Humanitarian'
governance$.id <- 'Internal governance'
external$.id <- 'External politics'
cooperation$.id <- 'Cooperation with the UN'

figure.2.data <- rbind(military, humanitarian, governance, external, cooperation)#recombine aggregated stats
#clean
rm(military)
rm(humanitarian)
rm(governance)
rm(external)
rm(cooperation)

#plot grouped bar chart on melted data
figure.2.plot <- ggplot(melt(figure.2.data[,c(1,2,4,6)],id='.id'), aes(x=.id,y=value,fill=variable))
figure.2.plot <- figure.2.plot + geom_bar(stat="identity", position=position_dodge(), colour="black")
figure.2.plot <- figure.2.plot + labs(title = 'Compliance With Thematic Typs',x='Thematic type of conduct requested',y='Mean score')
figure.2.plot <- figure.2.plot + scale_y_continuous(limits=c(0,2.5)) + theme_bw()

#Figure 3 - Variation in civil war parties’ compliance with demands by depth of demand over time
#extract data
figure.3.data <- data.plot[,c(year,dod)]
#subset into levels to process
low <- figure.3.data[which(figure.3.data$depth.of.demand.final==1),]
med <- figure.3.data[which(figure.3.data$depth.of.demand.final==2),]
high <- figure.3.data[which(figure.3.data$depth.of.demand.final==3),]
#convert dod level to 1s for counting
med$depth.of.demand.final <- 1
high$depth.of.demand.final <- 1
#summarise counts
low <- ddply(low, .(year.adopt), summarise,
             sum = sum(as.numeric(depth.of.demand.final),na.rm=TRUE))
med <- ddply(med, .(year.adopt), summarise,
             sum = sum(as.numeric(depth.of.demand.final),na.rm=TRUE))
high <- ddply(high, .(year.adopt), summarise,
              sum = sum(as.numeric(depth.of.demand.final),na.rm=TRUE))

#fix anomolies
tmp <- low[1,]
tmp[1,1] <- 1989
tmp[1,2] <- 0
low <- rbind(tmp,low)

tmp <- med[1:2,]
tmp[2,1] <- 1990
tmp[2,2] <- 0
med <- rbind(tmp,med[2:14,])

#add dod id
low$dod <- 'Low'
med$dod <- 'Medium'
high$dod <- 'High'

#recombine data
figure.3.data <- rbind(low,med,high)

#plot line graph
figure.3.plot <- ggplot(figure.3.data,aes(as.Date(figure.3.data$year.adopt, "%Y"),sum))
figure.3.plot <- figure.3.plot + geom_line(aes(color=dod))
figure.3.plot <- figure.3.plot + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
figure.3.plot <- figure.3.plot + labs(title = 'Demands by depth over time',x='Year',y='Number of demands')
figure.3.plot <- figure.3.plot + scale_y_continuous(limits=c(0,100)) + theme_bw()
#clean
rm(high)
rm(med)
rm(low)
rm(tmp)

#Figure 4 - Party Breakdown
#extract and bin data
col.labels <- c('year','num')
figure.4.data <- data.plot[,factions]
tmp <- as.numeric(as.character(data.plot[,year]))
figure.4.data <- data.frame(cbind(tmp,figure.4.data))
colnames(figure.4.data) <- col.labels
figure.4.data <- figure.4.data[which(figure.4.data$year < 2004),]
figure.4.data[is.na(figure.4.data)] <- 0
figure.4.data <- figure.4.data[,2]
figure.4.data <- cut(figure.4.data, breaks=c(-1,1,2.1,9,Inf),labels=c('None','2 parties','3-9 parties','10+ parties'))


col.labels <- c('variable','value')
val.array <- summary(figure.4.data)
total <- as.numeric(val.array[1]) + as.numeric(val.array[2]) + as.numeric(val.array[3]) + as.numeric(val.array[4])
tmp <- as.numeric(val.array[1]/total)
figure.4.data <- data.frame('None',tmp)
colnames(figure.4.data) <- col.labels

tmp <- as.numeric(val.array[2]/total)
tmp <- data.frame('2 parties',tmp)
colnames(tmp) <- col.labels
figure.4.data <- rbind(figure.4.data,tmp)

tmp <- as.numeric(val.array[3]/total)
tmp <- data.frame('3-9 parties',tmp)
colnames(tmp) <- col.labels
figure.4.data <- rbind(figure.4.data,tmp)

tmp <- as.numeric(val.array[4]/total)
tmp <- data.frame('10+ parties',tmp)
colnames(tmp) <- col.labels
figure.4.data <- rbind(figure.4.data,tmp)

rm(tmp)
rm(col.labels)
rm(val.array)

figure.4.plot <- ggplot(figure.4.data,aes(x=factor(1),y=value,fill = factor(variable))) 
figure.4.plot <- figure.4.plot + geom_bar() + coord_polar(theta="y") + theme_bw()

rm(coop)
rm(dod)
rm(ext)
rm(factions)
rm(gov)
rm(hum)
rm(medium)
rm(mil)
rm(short)
rm(total)
rm(year)
rm(region)
rm(war.name)
rm(date)

#####ANALYSIS SECTION
#extract data subset for analysis
data.model <- scc.data.trimmed[,c(3,8:11,14:17,28,31,33:34,38,46:48,57:59,72:73,75,78:87,97:99,103,105:106,108:129,137:138)]
data.model <- data.model[!is.na(data.model$medium.term.final),]

#Creat new variables
#Depth of demand dummies
data.model$dod.low <- as.numeric(as.character(data.model$depth.of.demand.final))
data.model$dod.low[data.model$dod.low  != 1] <- 0
data.model$dod.low <- as.factor(data.model$dod.low)

data.model$dod.med <- as.numeric(as.character(data.model$depth.of.demand.final))
data.model$dod.med[data.model$dod.med != 2] <- 0
data.model$dod.med[data.model$dod.med == 2] <- 1
data.model$dod.med <- as.factor(data.model$dod.med)

data.model$dod.high <- as.numeric(as.character(data.model$depth.of.demand.final))
data.model$dod.high[data.model$dod.high != 3] <- 0
data.model$dod.high[data.model$dod.high == 3] <- 1
data.model$dod.high <- as.factor(data.model$dod.high)

#convert shock to negative shock dummy
data.model$pol.shock <- as.numeric(as.character(data.model$pol.shock))
data.model$pol.shock[data.model$pol.shock > 0] <- 0
data.model$pol.shock[data.model$pol.shock < 0] <- 1

#create intensity dummies from factors
data.model$intensity.low <- as.numeric(as.character(data.model$intensity))
data.model$intensity.low[data.model$intensity.low  != 1] <- 0
data.model$intensity.low <- as.factor(data.model$intensity.low)

data.model$intensity.med <- as.numeric(as.character(data.model$intensity))
data.model$intensity.med[data.model$intensity.med != 2] <- 0
data.model$intensity.med[data.model$intensity.med == 2] <- 1
data.model$intensity.med <- as.factor(data.model$intensity.med)

data.model$intensity.high <- as.numeric(as.character(data.model$intensity))
data.model$intensity.high[data.model$intensity.high != 3] <- 0
data.model$intensity.high[data.model$intensity.high == 3] <- 1
data.model$intensity.high <- as.factor(data.model$intensity.high)

#Collate UN and total PK
data.model$total.un.pk <- data.model$UN.armed + data.model$UN.unarmed
data.model$total.pk <- data.model$UN.armed + data.model$UN.unarmed + data.model$auth.non.UN + data.model$unauth.non.UN

#Create un and non-un monitoring dummies
data.model$monit.un <- as.numeric(as.character(data.model$monit.mech.commission)) + as.numeric(as.character(data.model$monit.mech.committee)) + as.numeric(as.character(data.model$monit.mech.panel.of.experts)) + as.numeric(as.character(data.model$monit.mech.UN.field)) + as.numeric(as.character(data.model$monit.mech.SC)) + as.numeric(as.character(data.model$monit.mech.SG)) + as.numeric(as.character(data.model$monit.mech.other.UN))
data.model$monit.un[data.model$monit.un > 0] <- 1
data.model$monit.un <- as.factor(data.model$monit.un)
data.model$monit.non.un <- as.numeric(as.character(data.model$monit.mech.other.int.org)) + as.numeric(as.character(data.model$monit.mech.state))
data.model$monit.non.un[data.model$monit.non.un > 0] <- 1
data.model$monit.non.un <- as.factor(data.model$monit.non.un)

#Convert factor incentive variable to dummy pos.inc is 8 level factor -- need to convert to dummy of no(1) and yes(rest) positive incentives
data.model$incentives.benefits <- as.numeric(as.character(data.model$incentives.benefits))
data.model$incentives.benefits[data.model$incentives.benefits == 1] <- 0
data.model$incentives.benefits[data.model$incentives.benefits > 1] <- 1
data.model$incentives.benefits <- as.factor(data.model$incentives.benefits)

#convert threat variabel into component dummies
data.model$threat.benefits <- as.numeric(as.factor(data.model$threat.benefits))
data.model$threat.fin.benefits <- data.model$threat.benefits
data.model$threat.pko <- data.model$threat.benefits
data.model$threat.other <- data.model$threat.benefits

data.model$threat.fin.benefits[data.model$threat.fin.benefits == 1] <- 0
data.model$threat.fin.benefits[data.model$threat.fin.benefits == 3] <- 0
data.model$threat.fin.benefits[data.model$threat.fin.benefits == 4] <- 0
data.model$threat.fin.benefits[data.model$threat.fin.benefits == 6] <- 0
data.model$threat.fin.benefits[data.model$threat.fin.benefits > 0] <- 1
data.model$threat.fin.benefits <- as.factor(data.model$threat.fin.benefits)

data.model$threat.pko[data.model$threat.pko == 1] <- 0
data.model$threat.pko[data.model$threat.pko == 2] <- 0
data.model$threat.pko[data.model$threat.pko == 4] <- 0
data.model$threat.pko[data.model$threat.pko == 5] <- 0
data.model$threat.pko[data.model$threat.pko > 0] <- 1
data.model$threat.pko <- as.factor(data.model$threat.pko)

data.model$threat.other[data.model$threat.other == 1] <- 0
data.model$threat.other[data.model$threat.other == 2] <- 0
data.model$threat.other[data.model$threat.other == 3] <- 0
data.model$threat.other[data.model$threat.other > 0] <- 1
data.model$threat.other <- as.factor(data.model$threat.other)

data.model$threat.benefits[data.model$threat.benefits == 1] <- 0
data.model$threat.benefits[data.model$threat.benefits > 0] <- 1
data.model$threat.benefits <- as.factor(data.model$threat.benefits)

#convert to threat of force dummy
data.model$threat.force <- as.numeric(as.character(data.model$threat.force))
data.model$threat.force[data.model$threat.force == 1] <- 0
data.model$threat.force[data.model$threat.force > 0] <- 1
data.model$threat.force <- as.factor(data.model$threat.force)

#create general threat dummy
data.model$threat <- as.numeric(as.character(data.model$threat.benefits)) + as.numeric(as.character(data.model$threat.force)) + as.numeric(as.character(data.model$sanctions.threat))
data.model$threat[data.model$threat > 0] <- 1
data.model$threat <- as.factor(data.model$threat)

#creat dummies of sc missions
data.model$SC.mission <- as.numeric(as.factor(data.model$SC.mission))
data.model$sc.mission <- data.model$SC.mission
data.model$sc.mission.prior <- data.model$SC.mission
data.model$sc.mission.posterior <- data.model$SC.mission

data.model$sc.mission[data.model$sc.mission == 1] <- 0
data.model$sc.mission[data.model$sc.mission > 0] <- 1
data.model$sc.mission <- as.factor(data.model$sc.mission)

data.model$sc.mission.prior[data.model$sc.mission.prior == 1] <- 0
data.model$sc.mission.prior[data.model$sc.mission.prior == 3] <- 0
data.model$sc.mission.prior[data.model$sc.mission.prior > 0] <- 1
data.model$sc.mission.prior <- as.factor(data.model$sc.mission.prior)

data.model$sc.mission.posterior[data.model$sc.mission.posterior == 1] <- 0
data.model$sc.mission.posterior[data.model$sc.mission.posterior == 2] <- 0
data.model$sc.mission.posterior[data.model$sc.mission.posterior > 0] <- 1
data.model$sc.mission.posterior <- as.factor(data.model$sc.mission.posterior)

#Create sanctions dummy
data.model$sanctions <- as.numeric(as.character(data.model$sanct.type.general.trade)) +  as.numeric(as.character(data.model$sanct.type.arms)) +  as.numeric(as.character(data.model$sanct.type.natural.resource)) + as.numeric(as.character(data.model$sanct.type.financial)) + as.numeric(as.character(data.model$sanct.type.travel)) + as.numeric(as.character(data.model$sanct.type.targeted)) + as.numeric(as.character(data.model$sanct.type.diplomatic))
data.model$sanctions[data.model$sanctions > 0] <- 1
data.model$sanctions <- as.factor(data.model$sanctions)

#P-5 consensus variables
data.model <- cbind(data.model,scc.data.trimmed[,78:82])
data.model$p5.abstentions.current <- as.numeric(as.character(data.model$abstentions.CHI)) + as.numeric(as.character(data.model$absentions.FRA)) + as.numeric(as.character(data.model$absentions.RUS)) + as.numeric(as.character(data.model$absentions.UKG)) + as.numeric(as.character(data.model$absentions.USA))
data.model$p5.abstentions.current[data.model$p5.abstentions.current > 0] <- 1
data.model$p5.abstentions.current <- as.factor(data.model$p5.abstentions.current)

data.model$P5.consensus.2.year <- as.numeric(as.character(data.model$P5.consensus.2.year))
data.model$P5.consensus.4.year <- as.numeric(as.character(data.model$P5.consensus.4.year))

data.model$p5.vetoes.2yr <- data.model$P5.consensus.2.year
data.model$p5.vetoes.2yr[data.model$p5.vetoes.2yr == 1] <- 0
data.model$p5.vetoes.2yr[data.model$p5.vetoes.2yr == 2] <- 0
data.model$p5.vetoes.2yr[data.model$p5.vetoes.2yr == 3] <- 0
data.model$p5.vetoes.2yr[data.model$p5.vetoes.2yr > 0] <- 1
data.model$p5.vetoes.2yr <- as.factor(data.model$p5.vetoes.2yr)

data.model$p5.vetoes.4yr <- data.model$P5.consensus.4.year
data.model$p5.vetoes.4yr[data.model$p5.vetoes.4yr == 1] <- 0
data.model$p5.vetoes.4yr[data.model$p5.vetoes.4yr == 2] <- 0
data.model$p5.vetoes.4yr[data.model$p5.vetoes.4yr == 3] <- 0
data.model$p5.vetoes.4yr[data.model$p5.vetoes.4yr > 0] <- 1
data.model$p5.vetoes.4yr <- as.factor(data.model$p5.vetoes.4yr)

data.model$p5.abstentions.past.2yr <- data.model$P5.consensus.2.year
data.model$p5.abstentions.past.2yr[data.model$p5.abstentions.past.2yr == 1] <- 0
data.model$p5.abstentions.past.2yr[data.model$p5.abstentions.past.2yr == 4] <- 0
data.model$p5.abstentions.past.2yr[data.model$p5.abstentions.past.2yr == 5] <- 0
data.model$p5.abstentions.past.2yr[data.model$p5.abstentions.past.2yr > 0] <- 1
data.model$p5.abstentions.past.2yr <- as.factor(data.model$p5.abstentions.past.2yr)

data.model$p5.abstentions.past.4yr <- data.model$P5.consensus.4.year
data.model$p5.abstentions.past.4yr[data.model$p5.abstentions.past.4yr == 1] <- 0
data.model$p5.abstentions.past.4yr[data.model$p5.abstentions.past.4yr == 4] <- 0
data.model$p5.abstentions.past.4yr[data.model$p5.abstentions.past.4yr == 5] <- 0
data.model$p5.abstentions.past.4yr[data.model$p5.abstentions.past.4yr > 0] <- 1
data.model$p5.abstentions.past.4yr <- as.factor(data.model$p5.abstentions.past.4yr)

data.model$p5.abstentions.change <- as.numeric(as.character(data.model$p5.abstentions.current)) - as.numeric(as.character(data.model$p5.abstentions.past.2yr))

data.model$shrt.comp <- as.ordered(data.model$shrt.comp)
data.model$med.comp <- as.ordered(data.model$med.comp)

#final formatting
data.model <- data.model[,c(11:12,10,1,62:63,17,64:66,29:33,3:4,16,5,14:15,2,67:69,61,46,49,51:53,55:56,57:58,72:73,19:20,6:9,70:71,34,81,38:44,77,35,74:76,36:37,78:80,82:87,23)]

col.labels <- c('war.name','region','year','date',
                'shrt.comp','med.comp','dod','dod.low','dod.med','dod.high',
                'mil','hum','gov','ext','coop',
                'factions','battle.death','prox.war','prox.war.num','shock','lootable','intensity','intensity.low','intensity.med','intensity.high',
                'monitoring','monit.un.1','monit.un.2','monit.un.3','monit.un.4','monit.un.5','monit.un.6','monit.un.7','monit.int.org','monit.state',
                'monit.un','monit.non.un','trad.pko','multi.pko','un.armed','un.unarmed','auth.non.un','unauth.non.un','total.un.pk','total.pk',
                'pos.inc','sanctions','sanctions.general','sanctions.arms','sanctions.res','sanctions.financial','sanctions.travel','sanctions.targeted',
                'sanctions.diplo','threat','threat.benefits','threat.fin.benefits','threat.pko','threat.other','threat.force','threat.sanctions',
                'sc.mission','sc.mission.prior','sc.mission.posterior','p5.abstentions.current','p5.vetoes.2yr','p5.vetoes.4yr','p5.abstentions.past.2yr',
                'p5.abstentions.past.4yr','p5.abstentions.change','prior.consent')

colnames(data.model) <- col.labels

  
#########generate index numbers of variable of interest
###clusters
war.name <- which(colnames(data.model)=="civil.war")
region <- which(colnames(data.model)=="region")
year <- which(colnames(data.model)=="year.adopt")
date <- which(colnames(data.model)=="date.adopt")

###coompliance
shrt.comp <- which(colnames(data.model)=="short.term.final")
med.comp <- which(colnames(data.model)=="medium.term.final")
dod <- which(colnames(data.model)=="depth.of.demand.final")
dod.low <- which(colnames(data.model)=="dod.low")
dod.med <- which(colnames(data.model)=="dod.med")
dod.high <- which(colnames(data.model)=="dod.high")

###thematic
mil <- which(colnames(data.model)=="dem.type.military.conduct")
hum <- which(colnames(data.model)=="dem.type.humanitarian")
gov <- which(colnames(data.model)=="dem.type.governence")
ext <- which(colnames(data.model)=="dem.type.external")
coop <- which(colnames(data.model)=="dem.type.cooperation.with.UN")
  
###ecology
factions <- which(colnames(data.model)=="num.war.parties")
battle.death <- which(colnames(data.model)=="battle.deaths")
prox.war <- which(colnames(data.model)=="proximate.civil.war")
prox.war.num <- which(colnames(data.model)=="proximate.civil.war.num")
shock <- which(colnames(data.model)=="pol.shock")
lootable <- which(colnames(data.model)=="lootable.nat.res")
intensity <- which(colnames(data.model)=="intensity")
intensity.low <- which(colnames(data.model)=="intensity.low")
intensity.med <- which(colnames(data.model)=="intensity.med")
intensity.high <- which(colnames(data.model)=="intensity.high")

###un strategy
monitoring <- which(colnames(data.model)=="monitoring.mech.dummy")
trad.pko <- which(colnames(data.model)=="field.pres.trad.peace.op")
multi.pko <- which(colnames(data.model)=="field.pres.multi.peace.op")
un.armed <- which(colnames(data.model)=="UN.armed")
un.unarmed <- which(colnames(data.model)=="UN.unarmed")
auth.non.un <- which(colnames(data.model)=="auth.non.UN")
unauth.non.un <- which(colnames(data.model)=="unauth.non.UN")
total.un.pk <- which(colnames(data.model)=="total.un.pk")
total.pk <- which(colnames(data.model)=="total.pk")
monit.un.1 <- which(colnames(data.model)=="monit.mech.commission")
monit.un.2 <- which(colnames(data.model)=="monit.mech.committee")
monit.un.3 <- which(colnames(data.model)=="monit.mech.panel.of.experts")
monit.un.4 <- which(colnames(data.model)=="monit.mech.UN.field")
monit.un.5 <- which(colnames(data.model)=="monit.mech.SC")
monit.un.6 <- which(colnames(data.model)=="monit.mech.SG")
monit.un.7 <- which(colnames(data.model)=="monit.mech.other.UN")
monit.int.org <- which(colnames(data.model)=="monit.mech.other.int.org")
monit.state <- which(colnames(data.model)=="monit.mech.state")
monit.un <- which(colnames(data.model)=='monit.un')
monit.non.un <- which(colnames(data.model)=='monit.non.un')
pos.inc <- which(colnames(data.model)=="incentives.benefits")
sanctions <- which(colnames(data.model)=="sanctions")
sanctions.general <- which(colnames(data.model)=="sanct.type.general.trade")
sanctions.arms <- which(colnames(data.model)=="sanct.type.arms")
sanctions.res <- which(colnames(data.model)=="sanct.type.natural.resource")
sanctions.financial <- which(colnames(data.model)=="sanct.type.financial")
sanctions.travel <- which(colnames(data.model)=="sanct.type.travel")
sanctions.targeted <- which(colnames(data.model)=="sanct.type.targeted")
sanctions.diplo <- which(colnames(data.model)=="sanct.type.diplomatic")
threat <- which(colnames(data.model)=="threat")
threat.benefits <- which(colnames(data.model)=="threat.benefits")
threat.fin.benefits <- which(colnames(data.model)=="threat.fin.benefits")
threat.pko <- which(colnames(data.model)=="threat.pko")
threat.other <- which(colnames(data.model)=="threat.other")
threat.force <- which(colnames(data.model)=="threat.force")
threats.sanctions <- which(colnames(data.model)=="sanctions.threat")
sc.mission <- which(colnames(data.model)=="sc.mission")
sc.mission.prior <- which(colnames(data.model)=="sc.mission.prior")
sc.mission.posterior <- which(colnames(data.model)=="sc.mission.posterior")

#great powers
p5.abstentions.current <- which(colnames(data.model)=="p5.abstentions.current")
p5.vetoes.2yr <- which(colnames(data.model)=="p5.vetoes.2yr")
p5.vetoes.4yr <- which(colnames(data.model)=="p5.vetoes.4yr")
p5.abstentions.past.2yr <- which(colnames(data.model)=="p5.abstentions.past.2yr")
p5.abstentions.past.4yr <- which(colnames(data.model)=="p5.abstentions.past.4yr")
p5.abstentions.change <- which(colnames(data.model)=="p5.abstentions.change")

#ongoing
prior.consent <- which(colnames(data.model)=="prior.consent")

#plot distributions
figure.5.1.data <- data.frame(hist(as.numeric(as.character(data.model$shrt.comp)),breaks=c(0.5,1.5,2.5,3.5,4.5))[2])
figure.5.1.data <- cbind(figure.5.1.data,cat=1:4)
figure.5.1.plot <- ggplot(figure.5.1.data,aes(x=cat,y=counts)) + geom_bar(stat='identity') + theme_bw()
figure.5.1.plot <- figure.5.1.plot + labs(title = 'Distribution of Short-term Compliance Scores',x='Short-term compliance score',y='Number of demands')

figure.5.2.data <- data.frame(hist(as.numeric(as.character(data.model$med.comp)),breaks=c(0.5,1.5,2.5,3.5,4.5))[2])
figure.5.2.data <- cbind(figure.5.2.data,cat=1:4)
figure.5.2.plot <- ggplot(figure.5.2.data,aes(x=cat,y=counts)) + geom_bar(stat='identity') + theme_bw()
figure.5.2.plot <- figure.5.2.plot + labs(title = 'Distribution of Medium-term Compliance scores',x='Short-term compliance score',y='Number of demands')

figure.5.3.data <- data.frame(hist(as.numeric(as.character(data.model$dod)),breaks=c(0.5,1.5,2.5,3.5))[2])
figure.5.3.data <- cbind(figure.5.3.data,cat=1:3)
figure.5.3.plot <- ggplot(figure.5.3.data,aes(x=cat,y=counts)) + geom_bar(stat='identity') + theme_bw()
figure.5.1.plot <- figure.5.1.plot + labs(title = 'Distribution of Depth of Demand Scores',x='Depth of Demand score',y='Number of demands')


# Model 1.1 – control for region and year
model.1.1 <- polr(shrt.comp ~ monitoring + trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.2yr + p5.abstentions.past.2yr +
       p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.1.1.ctable <- data.frame(coef(summary(model.1.1)))
model.1.1.ctable <- model.1.1.ctable[c(1:17,41:43),]
p <- pnorm(abs(model.1.1.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.1.1.ctable <- cbind(model.1.1.ctable, 'p.value' = p)
model.1.1.ci <- confint.default(model.1.1)
model.1.1.ci <- exp(model.1.1.ci[1:18,])
OR <- exp(coef(model.1.1))
OR <- OR[1:18,]
model.1.1.ci <- cbind(OR,model.1.1.ci)

write.csv(model.1.1.ctable,'models/model.1.1.ctable.csv')
write.csv(model.1.1.ci,'models/model.1.1.ci.csv')

# Model 1.2 – control for region and year
model.1.2 <- polr(med.comp ~ monitoring + trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.2yr + p5.abstentions.past.2yr +
                    p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.1.2.ctable <- data.frame(coef(summary(model.1.2)))
model.1.2.ctable <- model.1.2.ctable[c(1:18,41:43),]
p <- pnorm(abs(model.1.2.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.1.2.ctable <- cbind(model.1.2.ctable, 'p.value' = p)
model.1.2.ci <- confint.default(model.1.2)
model.1.2.ci <- exp(model.1.2.ci[1:18,])
OR <- exp(coef(model.1.2))
OR <- OR[1:18,]
model.1.2.ci <- cbind(OR,model.1.2.ci)

write.csv(model.1.2.ctable,'models/model.1.2.ctable.csv')
write.csv(model.1.2.ci,'models/model.1.2.ci.csv')

# Model 2.1 – control for region and year
model.2.1 <- polr(shrt.comp ~ trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change +
                    prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.2.1.ctable <- data.frame(coef(summary(model.2.1)))
model.2.1.ctable <- model.2.1.ctable[c(1:17,40:42),]
p <- pnorm(abs(model.2.1.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.2.1.ctable <- cbind(model.2.1.ctable, 'p.value' = p)
model.2.1.ci <- confint.default(model.2.1)
model.2.1.ci <- exp(model.2.1.ci[1:17,])
OR <- exp(coef(model.2.1))
OR <- OR[1:17,]
model.2.1.ci <- cbind(OR,model.2.1.ci)

write.csv(model.2.1.ctable,'models/model.2.1.ctable.csv')
write.csv(model.2.1.ci,'models/model.2.1.ci.csv')

# Model 2.2 – control for region and year
model.2.2 <- polr(med.comp ~ trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change +
                    prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.2.2.ctable <- data.frame(coef(summary(model.2.2)))
model.2.2.ctable <- model.2.2.ctable[c(1:17,40:42),]
p <- pnorm(abs(model.2.2.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.2.2.ctable <- cbind(model.2.2.ctable, 'p.value' = p)
model.2.2.ci <- confint.default(model.2.2)
model.2.2.ci <- exp(model.2.2.ci[1:17,])
OR <- exp(coef(model.2.2))
OR <- OR[1:17,]
model.2.2.ci <- cbind(OR,model.2.2.ci)

write.csv(model.2.2.ctable,'models/model.2.2.ctable.csv')
write.csv(model.2.2.ci,'models/model.2.2.ci.csv')

# Model 3.1 – cluster on region and year
model.3.1 <- polr(shrt.comp ~ monitoring + trad.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.4yr + p5.abstentions.past.4yr + p5.abstentions.change +
                   prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.3.1.ctable <- data.frame(coef(summary(model.3.1)))
model.3.1.ctable <- model.3.1.ctable[c(1:17,40:42),]
p <- pnorm(abs(model.3.1.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.3.1.ctable <- cbind(model.3.1.ctable, 'p.value' = p)
model.3.1.ci <- confint.default(model.3.1)
model.3.1.ci <- exp(model.3.1.ci[1:17,])
OR <- exp(coef(model.3.1))
OR <- OR[1:17,]
model.3.1.ci <- cbind(OR,model.3.1.ci)

write.csv(model.3.1.ctable,'models/model.3.1.ctable.csv')
write.csv(model.3.1.ci,'models/model.3.1.ci.csv')

# Model 3.2 – cluster on region and year
model.3.2 <- polr(med.comp ~ monitoring + trad.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.4yr + p5.abstentions.past.4yr + p5.abstentions.change +
                    prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.3.2.ctable <- data.frame(coef(summary(model.3.2)))
model.3.2.ctable <- model.3.2.ctable[c(1:17,40:42),]
p <- pnorm(abs(model.3.2.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.3.2.ctable <- cbind(model.3.2.ctable, 'p.value' = p)
model.3.2.ci <- confint.default(model.3.2)
model.3.2.ci <- exp(model.3.2.ci[1:17,])
OR <- exp(coef(model.3.2))
OR <- OR[1:17,]
model.3.2.ci <- cbind(OR,model.3.2.ci)

write.csv(model.3.2.ctable,'models/model.3.2.ctable.csv')
write.csv(model.3.2.ci,'models/model.3.2.ci.csv')

# Model 4.1 – cluster on region and year
model.4.1 <- polr(shrt.comp ~ monitoring + trad.pko + sanctions.general + sanctions.arms + sanctions.res +  sanctions.financial + sanctions.travel + sanctions.targeted + sanctions.diplo +
                    threat.benefits + threat.fin.benefits + threat.pko + threat.other + threat.force + threat.sanctions + pos.inc + sc.mission.prior + 
                    p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high +
                    dod.med + dod.high + mil + hum + gov + ext + coop + region + year, data=data.model, Hess = TRUE)

model.4.1.ctable <- data.frame(coef(summary(model.4.1)))
model.4.1.ctable <- model.4.1.ctable[c(1:32,55:57),]
p <- pnorm(abs(model.4.1.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.4.1.ctable <- cbind(model.4.1.ctable, 'p.value' = p)
model.4.1.ci <- confint.default(model.4.1)
model.4.1.ci <- exp(model.4.1.ci[1:32,])
OR <- exp(coef(model.4.1))
OR <- OR[1:32,]
model.4.1.ci <- cbind(OR,model.4.1.ci)

write.csv(model.4.1.ctable,'models/model.4.1.ctable.csv')
write.csv(model.4.1.ci,'models/model.4.1.ci.csv')

# Model 4.2 – cluster on region and year
model.4.2 <- polr(med.comp ~ monitoring + trad.pko + sanctions.general + sanctions.arms + sanctions.res +  sanctions.financial + sanctions.travel + sanctions.targeted + sanctions.diplo +
                    threat.benefits + threat.fin.benefits + threat.pko + threat.other + threat.force + threat.sanctions + pos.inc + sc.mission.prior + 
                    p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + lootable + dod.med + dod.high +
                    dod.med + dod.high + mil + hum + gov + ext + coop + region + year, data=data.model, Hess = TRUE)

model.4.2.ctable <- data.frame(coef(summary(model.4.2)))
model.4.2.ctable <- model.4.2.ctable[c(1:32,55:57),]
p <- pnorm(abs(model.4.2.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.4.2.ctable <- cbind(model.4.2.ctable, 'p.value' = p)
model.4.2.ci <- confint.default(model.4.2)
model.4.2.ci <- exp(model.4.2.ci[1:32,])
OR <- exp(coef(model.4.2))
OR <- OR[1:32,]
model.4.2.ci <- cbind(OR,model.4.2.ci)

write.csv(model.4.2.ctable,'models/model.4.2.ctable.csv')
write.csv(model.4.2.ci,'models/model.4.2.ci.csv')

# Model 5.1 – cluster on year
model.5.1 <- polr(shrt.comp ~ monitoring + trad.pko +  multi.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change +
                    prior.consent + intensity.med + intensity.high + prox.war + shock + factions + lootable + dod.med + dod.high + mil + hum + gov + ext + coop +
                    year, data=data.model, Hess = TRUE)

model.5.1.ctable <- data.frame(coef(summary(model.5.1)))
model.5.1.ctable <- model.5.1.ctable[c(1:24,39:41),]
p <- pnorm(abs(model.5.1.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.5.1.ctable <- cbind(model.5.1.ctable, 'p.value' = p)
model.5.1.ci <- confint.default(model.5.1)
model.5.1.ci <- exp(model.5.1.ci[1:24,])
OR <- exp(coef(model.5.1))
OR <- OR[1:24,]
model.5.1.ci <- cbind(OR,model.5.1.ci)

write.csv(model.5.1.ctable,'models/model.5.1.ctable.csv')
write.csv(model.5.1.ci,'models/model.5.1.ci.csv')

# Model 5.2 – cluster on year
model.5.2 <- polr(med.comp ~ monitoring + trad.pko +  multi.pko + sanctions + threat + pos.inc + sc.mission.prior + p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change +
                    prior.consent + intensity.med + intensity.high + prox.war + shock + factions + lootable + dod.med + dod.high + mil + hum + gov + ext + coop +
                    year, data=data.model, Hess = TRUE)

model.5.2.ctable <- data.frame(coef(summary(model.5.2)))
model.5.2.ctable <- model.5.2.ctable[c(1:24,39:41),]
p <- pnorm(abs(model.5.2.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.5.2.ctable <- cbind(model.5.2.ctable, 'p.value' = p)
model.5.2.ci <- confint.default(model.5.2)
model.5.2.ci <- exp(model.5.2.ci[1:24,])
OR <- exp(coef(model.5.2))
OR <- OR[1:24,]
model.5.2.ci <- cbind(OR,model.5.2.ci)

write.csv(model.5.2.ctable,'models/model.5.2.ctable.csv')
write.csv(model.5.2.ci,'models/model.5.2.ci.csv')

# Model 6.1 – cluster on region and year
model.6.1 <- polr(shrt.comp ~ monit.un + monit.non.un +  trad.pko +  multi.pko + sanctions + threat + pos.inc + sc.mission.prior +
                    p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + factions +
                    lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.6.1.ctable <- data.frame(coef(summary(model.6.1)))
model.6.1.ctable <- model.6.1.ctable[c(1:20,43:45),]
p <- pnorm(abs(model.6.1.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.6.1.ctable <- cbind(model.6.1.ctable, 'p.value' = p)
model.6.1.ci <- confint.default(model.6.1)
model.6.1.ci <- exp(model.6.1.ci[1:20,])
OR <- exp(coef(model.6.1))
OR <- OR[1:20,]
model.6.1.ci <- cbind(OR,model.6.1.ci)

write.csv(model.6.1.ctable,'models/model.6.1.ctable.csv')
write.csv(model.6.1.ci,'models/model.6.1.ci.csv')

# Model 6.2 – cluster on region and year
model.6.2 <- polr(med.comp ~ monit.un + monit.non.un +  trad.pko +  multi.pko + sanctions + threat + pos.inc + sc.mission.prior +
                    p5.vetoes.2yr + p5.abstentions.past.2yr + p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + factions +
                    lootable + dod.med + dod.high + region + year, data=data.model, Hess = TRUE)

model.6.2.ctable <- data.frame(coef(summary(model.6.2)))
model.6.2.ctable <- model.6.2.ctable[c(1:20,43:45),]
p <- pnorm(abs(model.6.2.ctable[, "t.value"]), lower.tail = FALSE) * 2
model.6.2.ctable <- cbind(model.6.2.ctable, 'p.value' = p)
model.6.2.ci <- confint.default(model.6.2)
model.6.2.ci <- exp(model.6.2.ci[1:20,])
OR <- exp(coef(model.6.2))
OR <- OR[1:20,]
model.6.2.ci <- cbind(OR,model.6.2.ci)

write.csv(model.6.2.ctable,'models/model.6.2.ctable.csv')
write.csv(model.6.2.ci,'models/model.6.2.ci.csv')





model.1.clmm <- clmm(shrt.comp ~ monitoring + trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + (p5.vetoes.2yr + p5.abstentions.past.2yr) +
          p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + dod.med + dod.high +
            (1 | region), data = data.model, nAGQ = 10)

model.1.glmer <- glmer(shrt.comp ~ monitoring + trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + (p5.vetoes.2yr + p5.abstentions.past.2yr) +
           p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + dod.med + dod.high +
             (1 | war.name), data = data.model, family = binomial, nAGQ = 10)


model.1.clm <- clm(shrt.comp ~ monitoring + trad.pko + multi.pko + sanctions + threat + pos.inc + sc.mission.prior + (p5.vetoes.2yr + p5.abstentions.past.2yr) +
           p5.abstentions.change + prior.consent + intensity.med + intensity.high + prox.war + shock + dod.med + dod.high +
           (region), data = data.model, nAGQ = 10)












#clean
rm(war.name)
rm(region)
rm(year)
rm(date)
rm(shrt.comp)
rm(med.comp)
rm(dod)
rm(dod.low)
rm(dod.med)
rm(dod.high)
rm(mil)
rm(hum)
rm(gov)
rm(ext)
rm(coop)
rm(factions)
rm(battle.death)
rm(prox.war)
rm(prox.war.num)
rm(shock)
rm(lootable)
rm(intensity)
rm(intensity.low)
rm(intensity.med)
rm(intensity.high)
rm(monitoring)
rm(monit.un.1)
rm(monit.un.2)
rm(monit.un.3)
rm(monit.un.4)
rm(monit.un.5)
rm(monit.un.6)
rm(monit.un.7)
rm(monit.int.org)
rm(monit.state)
rm(monit.un)
rm(monit.non.un)
rm(trad.pko)
rm(multi.pko)
rm(un.armed)
rm(un.unarmed)
rm(auth.non.un)
rm(unauth.non.un)
rm(total.un.pk)
rm(total.pk)
rm(pos.inc)
rm(sanctions)
rm(sanctions.general)
rm(sanctions.arms)
rm(sanctions.res)
rm(sanctions.financial)
rm(sanctions.travel)
rm(sanctions.targeted)
rm(sanctions.diplo)
rm(threat)
rm(threat.benefits)
rm(threat.fin.benefits)
rm(threat.pko)
rm(threat.other)
rm(threat.force)
rm(threats.sanctions)
rm(sc.mission)
rm(sc.mission.prior)
rm(sc.mission.posterior)
rm(p5.abstentions.current)
rm(p5.vetoes.2yr)
rm(p5.vetoes.4yr)
rm(p5.abstentions.past.2yr)
rm(p5.abstentions.past.4yr)
rm(p5.abstentions.change)
rm(prior.consent)

rm(col.labels)