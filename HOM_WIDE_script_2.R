################################################################################################3
###
###
###SCript for intercatch format
###
###by:Gersom Costas
###
################################################################################################3
###
###
### 00 INTRODUCION

### 01  "HI": header information

### 03. "SD": Species data
  ### 03.1  Length Distribution   LD
  ### 03.2  ALK
  ### 03.3  Abundance by age (catage) . thousands#####
    ### 03.3.a  Subdivision  8cw######8cw
    ### 03.3.b  Subdivision  8ce######8ce
    ### 03.3.c  Subdivision  8ab######8ab

### 02   "SI": Species information
  ### 02.1  Reported vs unreported (unallocated)
    ### 02.1.a. TABLE: All catches ( reported & no repoprted) by metier quarter, div
    ### 02.1.b. TABLE:Fleets that have non reported catch (unallocated)- catches by quarter
    ### 02.1.c. TABLE: Fleets that have non reported catch(unallocated catch)
    ### 02.1.d. TABLE: unallocated by division
  ### 02.2 SUMMARIZING TABLES
    ### 02.2.1  catch by quarter and metier-- ALL REPORTED *NO REPORTED
    ### 02.2.2  SAMPLED abundance by quarter and metier###ABUNDANCES. ONLY iN REPORTED*
    ### 02.2.3 COMBINED SAMPLED + CATCHES

### 04.  BUILDING MATRICES
  ### 04.1   BY FLEETS
    ### 04.1.1 ARTISANAL
      ### 04.1.1.a  TABLES
      ### 04.1.1.b  Ratio samped vs non-sampled (Artisanal)
      ### 04.1.1.c  Matrices by subdivision (Artisanal)
    ### 04.1.2  ARRASTRE(TRAWL)
      ### 04.1.2.a TABLES
      ### 04.1.2.b  Ratio reported vs unreported (unallocated) (Arrastre)
      ### 04.1.2.c  Ratio sampled vs non-sampled (Arrastre)
      ### 04.1.2.d  Matrices by subdivision (Arrastre)
    ### 04.1.3  PURSE SEINE (CERCO)
      ### 04.1.3.a  TABLES
      ### 04.1.3.b    Matrices by subdivision (CERCO)
      ### 04.1.3.c    Matrices by subdivision (CERCO)**********
    ### 04.1.4 Catage summarazing  #BY FLEET
  ### 04.2  Catage matrices
    ### 04.2.1   BY SUBDIVISIONS
    ### 04.2.2   BY DIVISION

### 05  Catches by subdiv & quarter

### 06 Samplig intensity

#####################################################################################
#####################################################################################
#install.packages("plyr")
#install.packages("tidyr")
#install.packages("dplyr")
#browseVignettes(package="dplyr")# muestra ejemplos del uso del package dplyr
#library (dplyr)

library (plyr)
library (dplyr)
library(tidyr)
#####################################################################################
#####################################################################################

### 00 INTRODUCION

#Importar archivo formato intercatch

#IC_HOM_W_2016 <- read.table("IC_HOM_W_2016.csv",header=F,sep=",",na.strings =c( "","-9"))
#IC_HOM_W_2016 <- read.table("2016 DC WGWIDE hom-west ES landings.csv",header=F,sep=",",na.strings ="")
IC_HOM_W_2016 <- read.table("2017 DC WGWIDE hom.27.2a4a5b6a7a-ce-k8 ES_IEO landings a Gerson.csv",header=F,sep=",",na.strings ="")
#colnames(IC_HOM_W_2016)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")         


head(IC_HOM_W_2016)
str(IC_HOM_W_2016)



######################################################################
######################################################################
######################################################################
###01.     "HI": header information

IC_HOM_W_2016.HI <- filter(IC_HOM_W_2016, IC_HOM_W_2016[,1]=="HI")%>%   droplevels()
IC_HOM_W_2016.HI <- Filter(function(x)!all(is.na(x)), IC_HOM_W_2016.HI)    #Remove empty columns in dataframe
colnames(IC_HOM_W_2016.HI)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","UnitEffort","Effort","AreaQualifier")
#IC_HOM_W_2016.HI <- Filter(function(x)!all(is.na(x)), IC_HOM_W_2016.HI)    #Remove empty columns in dataframe
IC_HOM_W_2016.HI[IC_HOM_W_2016.HI==-9]<-sqrt(-1)  # replace -9 to NA
#select(DF, -colname) ?

str(IC_HOM_W_2016.HI)
summary(IC_HOM_W_2016.HI)
head(IC_HOM_W_2016.HI)



######################################################################
######################################################################
######################################################################
###03.     "SD": Species data   


#Hacemos subset para crear archivo con distribuciones de talla por division y season
IC_HOM_W_2016.SD <- filter(IC_HOM_W_2016, IC_HOM_W_2016[,1]=="SD")%>%
droplevels()

#IC_HOM_W_2016.SD <- Filter(function(x)!all(is.na(x)), IC_HOM_W_2016.SD)    #Remove empty columns in dataframe
#ponemos nombres a las variable del archivo intercatch
colnames(IC_HOM_W_2016.SD)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")

IC_HOM_W_2016.SD[IC_HOM_W_2016.SD==-9]<-sqrt(-1) # replace -9 to NA
str(IC_HOM_W_2016.SD)

IC_HOM_W_2016.SD$AgeLength<- as.numeric(as.character(IC_HOM_W_2016.SD$AgeLength))# pasamos la variable talla a foprmato numerico. . Al importarlo automaticament le habÃ?a asignado formato factor

summary(IC_HOM_W_2016.SD)
head(IC_HOM_W_2016.SD)

### 03.1  Length Distribution   LD

#Obtenemos un archivode distribucion de tallas por metier, division y quarter.

LD<-IC_HOM_W_2016.SD %>%
select(Stock,Fleet,FishingArea,Season,AgeLength, NumberCaught,UnitAgeOrLength) %>%
arrange(Stock,desc(Fleet,FishingArea,Season,AgeLength,UnitAgeOrLength)) %>%
group_by(Stock,Fleet,FishingArea,Season,AgeLength,UnitAgeOrLength) %>%
summarise(canum = sum(NumberCaught, na.rm=T))

###mutate(Fleet=gsub(">","BIG", Fleet))
head(LD,3)


### 03.2  ALK
#Importamos las claves .
#Las claves deben de estar por division y quarter. En principio las importamos con formato: X, a0, a1, ......aXplus

alkHOM_2016_8cw_1S <- read.csv("hmack_alk_8cw_s1_16.csv")
alkHOM_2016_8cw_2S <- read.csv("hmack_alk_8cw_s2_16.csv")
alkHOM_2016_8ce_1S <- read.csv("hmack_alk_8ce_s1_16.csv")
alkHOM_2016_8ce_2S <- read.csv("hmack_alk_8ce_s2_16.csv")
alkHOM_2016_8ab_1S <- read.csv("hmack_alk_8ab_s1_16.csv")
alkHOM_2016_8ab_2S <- read.csv("hmack_alk_8ab_s2_16.csv")

#8cw
rownames(alkHOM_2016_8cw_1S)<-alkHOM_2016_8cw_1S[ ,1]
alkHOM_2016_8cw_1S[is.na(alkHOM_2016_8cw_1S)]<-0
alkHOM_2016_8cw_1S<-rename(alkHOM_2016_8cw_1S, AgeLength=X)
alkHOM_2016_8cw_1S<-do.call(rbind, replicate(2, alkHOM_2016_8cw_1S, simplify=FALSE))#replicate matrix
alkHOM_2016_8cw_1S$FishingArea<-"27.8.c.w"
alkHOM_2016_8cw_1S$Season<-rep(1:2, each=51)# length alk 51 cm #aÃ±adimos las columna Season; trimestre
alkHOM_2016_8cw_1S

rownames(alkHOM_2016_8cw_2S)<-alkHOM_2016_8cw_2S[ ,1]
alkHOM_2016_8cw_2S[is.na(alkHOM_2016_8cw_2S)]<-0
alkHOM_2016_8cw_2S<-rename(alkHOM_2016_8cw_2S, AgeLength=X)
alkHOM_2016_8cw_2S<-do.call(rbind, replicate(2, alkHOM_2016_8cw_2S, simplify=FALSE))#replicate matrix
alkHOM_2016_8cw_2S$FishingArea<-"27.8.c.w"
alkHOM_2016_8cw_2S$Season<-rep(3:4, each=51)# length alk 51 cm
alkHOM_2016_8cw_2S

#8ce
rownames(alkHOM_2016_8ce_1S)<-alkHOM_2016_8ce_1S[ ,1]
alkHOM_2016_8ce_1S[is.na(alkHOM_2016_8ce_1S)]<-0
alkHOM_2016_8ce_1S<-rename(alkHOM_2016_8ce_1S, AgeLength=X)
alkHOM_2016_8ce_1S<-do.call(rbind, replicate(2, alkHOM_2016_8ce_1S, simplify=FALSE))#replicate matrix
alkHOM_2016_8ce_1S$FishingArea<-"27.8.c.e"
alkHOM_2016_8ce_1S$Season<-rep(1:2, each=51)# length alk 51 cm #aÃ±adimos las columna Season; trimestre
alkHOM_2016_8ce_1S

rownames(alkHOM_2016_8ce_2S)<-alkHOM_2016_8ce_2S[ ,1]
alkHOM_2016_8ce_2S[is.na(alkHOM_2016_8ce_2S)]<-0
alkHOM_2016_8ce_2S<-rename(alkHOM_2016_8ce_2S, AgeLength=X)
alkHOM_2016_8ce_2S<-do.call(rbind, replicate(2, alkHOM_2016_8ce_2S, simplify=FALSE))#replicate matrix
alkHOM_2016_8ce_2S$FishingArea<-"27.8.c.e"
alkHOM_2016_8ce_2S$Season<-rep(3:4, each=51)# length alk 51 cm
alkHOM_2016_8ce_2S

#8ab
rownames(alkHOM_2016_8ab_1S)<-alkHOM_2016_8ab_1S[ ,1]
alkHOM_2016_8ab_1S[is.na(alkHOM_2016_8ab_1S)]<-0
alkHOM_2016_8ab_1S<-rename(alkHOM_2016_8ab_1S, AgeLength=X)
alkHOM_2016_8ab_1S<-do.call(rbind, replicate(2, alkHOM_2016_8ab_1S, simplify=FALSE))#replicate matrix
alkHOM_2016_8ab_1S$FishingArea<-"27.8.ab"
alkHOM_2016_8ab_1S$Season<-rep(1:2, each=51)# length alk 51 cm #aÃ±adimos las columna Season; trimestre
alkHOM_2016_8ab_1S

rownames(alkHOM_2016_8ab_2S)<-alkHOM_2016_8ab_2S[ ,1]
alkHOM_2016_8ab_2S[is.na(alkHOM_2016_8ab_2S)]<-0
alkHOM_2016_8ab_2S<-rename(alkHOM_2016_8ab_2S, AgeLength=X)
alkHOM_2016_8ab_2S<-do.call(rbind, replicate(2, alkHOM_2016_8ab_2S, simplify=FALSE))#replicate matrix
alkHOM_2016_8ab_2S$FishingArea<-"27.8.ab"
alkHOM_2016_8ab_2S$Season<-rep(3:4, each=51)# length alk 51 cm
alkHOM_2016_8ab_2S


# splitting 8a 8b 8d
##8a
alkHOM_2016_8a_1S<-alkHOM_2016_8ab_1S%>%mutate(FishingArea= revalue( c("27.8.ab"="27.8.a")))
alkHOM_2016_8a_2S<-alkHOM_2016_8ab_2S%>%mutate(FishingArea= revalue( c("27.8.ab"="27.8.a")))

##8b
alkHOM_2016_8b_1S<-alkHOM_2016_8ab_1S%>%mutate(FishingArea= revalue( c("27.8.ab"="27.8.b")))
alkHOM_2016_8b_2S<-alkHOM_2016_8ab_2S%>%mutate(FishingArea= revalue( c("27.8.ab"="27.8.b")))

##8d
#alkHOM_2016_8d_1S<-alkHOM_2016_8ab_1S%>%mutate(FishingArea= revalue( c("27.8.ab"="27.8.d")))
#alkHOM_2016_8d_2S<-alkHOM_2016_8ab_2S%>%mutate(FishingArea= revalue( c("27.8.ab"="27.8.d")))


#una vez importados
#do.call(rbind, replicate(2, alkHOM_2016_1S, simplify=FALSE))#replicate matrix
alk_HOMW_16<-bind_rows(alkHOM_2016_8cw_1S,alkHOM_2016_8ce_1S,alkHOM_2016_8a_1S,alkHOM_2016_8b_1S,alkHOM_2016_8cw_2S,alkHOM_2016_8ce_2S,alkHOM_2016_8a_2S,alkHOM_2016_8b_2S)
alk_HOMW_16$FishingArea<-as.factor(alk_HOMW_16$FishingArea)


# ALK by quarter area ##*porcentage

alk<-alk_HOMW_16 %>%
select(starts_with("a", ignore.case=F))%>% # argument ignore case to be sensible capital letter
mutate(sum=rowSums(.,na.rm=T))%>%
bind_cols(select(alk_HOMW_16,FishingArea,Season,AgeLength))%>%
gather(age, num, -FishingArea, -Season, -AgeLength, -sum,na.rm = T)%>%
mutate(perc=num/sum)%>%
select(-num)%>%
spread(age,perc)%>%
select(AgeLength,Season,FishingArea,sum,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15plus)%>%
arrange(FishingArea,Season,  AgeLength)

head(alk,3)
rm(alkHOM_2016_8a_1S,alkHOM_2016_8a_2S,alkHOM_2016_8b_1S,alkHOM_2016_8b_2S)


###03.3  Abundance by age (catage) .

###03.3.a  Subdivision  8cw######8cw


#by area, metier age.  thousands#######
catage8cw<-left_join(filter(LD,FishingArea=="27.8.c.w" ),filter(alk,FishingArea=="27.8.c.w" ))%>%
ungroup()%>%
  mutate_at(vars(a0:a15plus),funs(.*canum*1))%>%  #% esta en thousands k
select(-AgeLength)%>%
group_by(Fleet,FishingArea,Season)%>%
summarise_at(vars(a0:a15plus),funs(sum(. ,na.rm=T)))%>%
arrange(Fleet, FishingArea,Season)%>%
data.frame()%>%
mutate(Flota= revalue( Fleet,c("GNS_DEF_60-79_0_0"="artisanal", "GNS_DEF_>=100_0_0"="artisanal","GNS_DEF_80-99_0_0"="artisanal", "GTR_DEF_60-79_0_0"="artisanal", "LHM_DEF_0_0_0" ="artisanal", "LHM_DWS_0_0_0" ="artisanal","LHM_SPF_0_0_0"="artisanal", "LLS_DEF_0_0_0"="artisanal", "MIS_MIS_0_0_0_HC"="artisanal","OTB_DEF_>=55_0_0"="arrastre",  "OTB_MCD_>=55_0_0"="arrastre",  "OTB_MPD_>=55_0_0" ="arrastre","OTB_DEF_>=70_0_0"="arrastre" ,"PS_SPF_0_0_0"="cerco","PTB_MPD_>=55_0_0"="arrastre"  )))%>% 
select(Fleet, Flota,FishingArea:a15plus)%>%  
arrange(Fleet, FishingArea,Season)

head(catage8cw)


##mean length by area, metier age (  L(cm)  )#
lmeanage_8cw<-left_join(filter(LD,FishingArea=="27.8.c.w" ),filter(alk,FishingArea=="27.8.c.w" ))%>%
group_by(Fleet,FishingArea,Season)%>%
mutate_at(vars(a0:a15plus),funs(.*canum*1))%>%  #% esta en thousands k
mutate(meanlength=AgeLength+0.5)%>%
group_by(Fleet,FishingArea,Season)%>%
mutate_at(vars(a0:a15plus),funs(.*meanlength))%>%
arrange(Fleet, FishingArea,Season)%>%
select(-Stock, -AgeLength,- UnitAgeOrLength,-canum,-sum,-meanlength )%>%
group_by(Fleet,FishingArea,Season)%>%  
summarise_all(funs(sum(.,na.rm=T )))%>%
data.frame()%>%
arrange(Fleet, FishingArea,Season)%>%
group_by(Fleet,FishingArea,Season)%>% 
gather(Age, cm, a0:a15plus)%>%
arrange(FishingArea, Fleet, Season)

lmeanage_8cw<-left_join(lmeanage_8cw,  arrange(gather(catage8cw,Age, n, a0:a15plus), FishingArea,Fleet, Season))%>% 
  mutate(lgth_mean=cm/n)%>% 
  select(-cm,-n)%>%
  arrange(FishingArea, Fleet, Season)

head(lmeanage_8cw)


##mean weight by area, metier age ( gr  ) weight#

#CHECKING
a<-0.01291              
b<- 2.8545
wmeanage_8cw<-left_join(filter(LD,FishingArea=="27.8.c.w" ),filter(alk,FishingArea=="27.8.c.w" ))%>%
group_by(Fleet,FishingArea,Season)%>%
mutate_at(vars(a0:a15plus),funs(.*canum*1))%>%  #% esta en thousands k
mutate(meanlength=AgeLength+0.5)%>%
group_by(Fleet,FishingArea,Season)%>%
mutate(meanweight=a*meanlength^b) %>% #%  weight by length cm
mutate_at(vars(a0:a15plus),funs(.*meanweight))%>%
arrange(Fleet, FishingArea,Season)%>%
select(-Stock, -AgeLength,- UnitAgeOrLength,-canum,-sum,-meanlength,-meanweight )%>%
summarise_all(funs(sum(.,na.rm=T )))%>%
data.frame()%>%
arrange(Fleet, FishingArea,Season)%>%
group_by(Fleet,FishingArea,Season)%>%  
gather(Age, wgt, a0:a15plus)%>%
arrange(FishingArea, Fleet, Season)

wmeanage_8cw<-left_join(wmeanage_8cw,  arrange(gather(catage8cw,Age, n, a0:a15plus), FishingArea,Fleet, Season))%>% 
  mutate(wght_mean=wgt/n)%>% 
  select(-wgt,-n)%>%
  arrange(FishingArea, Fleet, Season)

wmeanage_8cw

###03.3.b  Subdivision  8ce######8ce

#by area, metier age 
catage8ce<-left_join(filter(LD,FishingArea=="27.8.c.e" ),filter(alk,FishingArea=="27.8.c.e" ))%>%
  ungroup()%>%
  mutate_at(vars(a0:a15plus),funs(.*canum*1))%>%  #% esta en thousands k
  select(-AgeLength)%>%
  group_by(Fleet,FishingArea,Season)%>%
  summarise_at(vars(a0:a15plus),funs(sum(. ,na.rm=T)))%>%
  arrange(Fleet, FishingArea,Season)%>%
  data.frame()%>%
  mutate(Flota= revalue( Fleet,c("GNS_DEF_60-79_0_0"="artisanal", "GNS_DEF_>=100_0_0"="artisanal","GNS_DEF_80-99_0_0"="artisanal", "GTR_DEF_60-79_0_0"="artisanal", "LHM_DEF_0_0_0" ="artisanal", "LHM_DWS_0_0_0" ="artisanal","LHM_SPF_0_0_0"="artisanal", "LLS_DEF_0_0_0"="artisanal", "MIS_MIS_0_0_0_HC"="artisanal","OTB_DEF_>=55_0_0"="arrastre",  "OTB_MCD_>=55_0_0"="arrastre",  "OTB_MPD_>=55_0_0" ="arrastre","OTB_DEF_>=70_0_0"="arrastre" ,"PS_SPF_0_0_0"="cerco","PTB_MPD_>=55_0_0"="arrastre"  )))%>% 
  select(Fleet, Flota,FishingArea:a15plus)%>%  
  arrange(Fleet, FishingArea,Season)

head(catage8ce)


##mean length by area, metier age (  L(cm)  )######8ce#
lmeanage_8ce<-left_join(filter(LD,FishingArea=="27.8.c.e" ),filter(alk,FishingArea=="27.8.c.e" ))%>%
  group_by(Fleet,FishingArea,Season)%>%
  mutate_at(vars(a0:a15plus),funs(.*canum*1))%>%  #% esta en thousands k
  mutate(meanlength=AgeLength+0.5)%>%
  group_by(Fleet,FishingArea,Season)%>%
  mutate_at(vars(a0:a15plus),funs(.*meanlength))%>%
  arrange(Fleet, FishingArea,Season)%>%
  select(-Stock, -AgeLength,- UnitAgeOrLength,-canum,-sum,-meanlength )%>%
  group_by(Fleet,FishingArea,Season)%>%  
  summarise_all(funs(sum(.,na.rm=T )))%>%
  data.frame()%>%
  arrange(Fleet, FishingArea,Season)%>%
  group_by(Fleet,FishingArea,Season)%>% 
  gather(Age, cm, a0:a15plus)%>%
  arrange(FishingArea, Fleet, Season)

lmeanage_8ce<-left_join(lmeanage_8ce,  arrange(gather(catage8ce,Age, n, a0:a15plus), FishingArea,Fleet, Season))%>% 
  mutate(lgth_mean=cm/n)%>% 
  select(-cm,-n)%>%
  arrange(FishingArea, Fleet, Season)

head(lmeanage_8ce)


##mean weight by area, metier age ( gr  ) weight##

#CHECKING
a<-0.01291              
b<- 2.8545
wmeanage_8ce<-left_join(filter(LD,FishingArea=="27.8.c.e" ),filter(alk,FishingArea=="27.8.c.e" ))%>%
  group_by(Fleet,FishingArea,Season)%>%
  mutate_at(vars(a0:a15plus),funs(.*canum*1))%>%  #% esta en thousands k
  mutate(meanlength=AgeLength+0.5)%>%
  group_by(Fleet,FishingArea,Season)%>%
  mutate(meanweight=a*meanlength^b) %>% #%  weight by length cm
  mutate_at(vars(a0:a15plus),funs(.*meanweight))%>%
  arrange(Fleet, FishingArea,Season)%>%
  select(-Stock, -AgeLength,- UnitAgeOrLength,-canum,-sum,-meanlength,-meanweight )%>%
  summarise_all(funs(sum(.,na.rm=T )))%>%
  data.frame()%>%
  arrange(Fleet, FishingArea,Season)%>%
  group_by(Fleet,FishingArea,Season)%>%  
  gather(Age, wgt, a0:a15plus)%>%
  arrange(FishingArea, Fleet, Season)

wmeanage_8ce<-left_join(wmeanage_8ce,  arrange(gather(catage8ce,Age, n, a0:a15plus), FishingArea,Fleet, Season))%>% 
  mutate(wght_mean=wgt/n)%>% 
  select(-wgt,-n)%>%
  arrange(FishingArea, Fleet, Season)

wmeanage_8ce


###03.3.c  Subdivision  8ab######8ab

###there isn't catch sampled +++++++++++++++++++++++++++++++




######################################################################
######################################################################
######################################################################
###  02.   "SI": Species information

IC_HOM_W_2016.SI <- filter(IC_HOM_W_2016, IC_HOM_W_2016[,1]=="SI")%>%   droplevels()
IC_HOM_W_2016.SI <- Filter(function(x)!all(is.na(x)), IC_HOM_W_2016.SI)    #Remove empty columns in dataframe
colnames(IC_HOM_W_2016.SI)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","varCATON","InfoFleet","InfoStockCoordinator","InfoGeneral")
IC_HOM_W_2016.SI[IC_HOM_W_2016.SI==-9]<-sqrt(-1)  # replace -9 to NA
str(IC_HOM_W_2016.SI)
summary(IC_HOM_W_2016.SI)
head(IC_HOM_W_2016.SI)


###02.1  Reported vs unreported (unallocated)

#Creating non reported part in intercatch file
intercatch_nonreport_format<-IC_HOM_W_2016.SI%>%filter(ReportingCategory=="N")%>%group_by(RecordType, Country, Year, SeasonType, Season,AreaType, FishingArea, DepthRange, Species, Stock, CatchCategory, ReportingCategory, DataToFrom, Usage, SamplesOrigin ,QualityFlag, UnitCATON, OffLandings, varCATON, InfoFleet, InfoStockCoordinator, InfoGeneral)%>%summarise(CATON=sum(CATON,na.rm=T))%>%mutate(Fleet="ALL")%>%select(RecordType:Season,Fleet,AreaType:UnitCATON,CATON,OffLandings:InfoGeneral)%>%data.frame()

intercatch_nonreport_format$varCATON[is.nan(intercatch_nonreport_format$varCATON)]<-(-9)
intercatch_nonreport_format$OffLandings[is.nan(intercatch_nonreport_format$OffLandings)]<-(-9)

write.csv(intercatch_nonreport_format, "IC_HOM_W_2016.SI_non.reported.csv" , row.names =F)

#CatchCategory:
# L =  Landings. Landings above minimum size.
# B =  BMS Landings. Landings below minimum size, BMS.
# D = Discards. part of the catch thrown overboard into the sea and not registered in  logbook.
# C =Catch , no separation in the information of landings or discards.
# R = Logbook Registered Discard. Relevant for stocks under landing obligation.  are under the exemption rules (e.g. minimis).


###02.1.a. TABLE: All catches ( reported & no repoprted) by metier quarter, div

IC_HOM_W_2016.SI%>%group_by(Fleet,FishingArea, CatchCategory,ReportingCategory)%>%summarise(sum(CATON, na.rm=T))


###02.1.b. TABLE: Fleets that have non reported catch (unallocated)- catches by quarter 

IC_HOM_W_2016.SI%>%group_by(Fleet,FishingArea, CatchCategory,ReportingCategory,Season)%>%filter(ReportingCategory=="N")%>%select(Fleet,FishingArea, CatchCategory,ReportingCategory,Season,CATON)


###02.1.c. TABLE: Fleets that have non reported catch(unallocated catch)

non_R<-  IC_HOM_W_2016.SI%>%filter(ReportingCategory=="N")%>%select(Fleet) %>%unique()
non_R<-as.vector(as.matrix(non_R))
non_R
#[1] "OTB_DEF_>=55_0_0" "OTB_MPD_>=55_0_0" "PS_SPF_0_0_0"     "PTB_MPD_>=55_0_0"

#ReportingCategory:
# M = Misreported,
# N = Non-reported,
# R = Reported,
#RM = Reported and misreported,
#RN = Reported and non reported,
# S = SOP corrections

###02.1.d. TABLE: unallocated by division
IC_HOM_W_2016.SI%>% filter(ReportingCategory=="N")%>%filter(FishingArea=="27.8.c.e")#solo arrastre
IC_HOM_W_2016.SI%>% filter(ReportingCategory=="N")%>%filter(FishingArea=="27.8.c.w")#arrastre + cerco
IC_HOM_W_2016.SI%>% filter(ReportingCategory=="N")%>%filter(FishingArea=="27.8.b")#cerco
IC_HOM_W_2016.SI%>% filter(ReportingCategory=="N")%>%filter(FishingArea=="27.8.a")#naad



### 02.2 SUMMARIZING TABLES

### 02.2.1  catch by quarter and metier-- ALL REPORTED *NO REPORTED****CATCHES (t)  

catchby<-  IC_HOM_W_2016.SI%>%group_by(Fleet,FishingArea, ReportingCategory, Season)%>%select(Fleet,FishingArea,ReportingCategory,Season,CATON)%>%spread(Season, CATON)%>%ungroup()%>%mutate(Flota=Fleet)%>%mutate(Flota= revalue( Fleet,c("GNS_DEF_60-79_0_0"="artisanal", "GNS_DEF_>=100_0_0"="artisanal","GNS_DEF_80-99_0_0"="artisanal", "GTR_DEF_60-79_0_0"="artisanal", "LHM_DEF_0_0_0" ="artisanal", "LHM_DWS_0_0_0" ="artisanal","LHM_SPF_0_0_0"="artisanal", "LLS_DEF_0_0_0"="artisanal", "MIS_MIS_0_0_0_HC"="artisanal","OTB_DEF_>=55_0_0"="arrastre",  "OTB_DEF_70-99_0_0"="arrastre",  "OTB_MPD_>=55_0_0" ="arrastre","OTB_DEF_>=70_0_0"="arrastre" ,"PS_SPF_0_0_0"="cerco","PTB_MPD_>=55_0_0"="arrastre" )))%>%  arrange(Flota, ReportingCategory,FishingArea)%>%replace(is.na( .), 0)
catchby

write.csv(catchby,"catchbyfleet_SP.csv", row.names =F,na = "")

### 02.2.2  SAMPLED abundance by quarter and metier###ABUNDANCES. ONLY iN REPORTED***ABUNDANCES (n) ****

sampledby<-bind_rows(catage8ce,catage8cw)%>%group_by(Fleet,FishingArea,  Season)%>%ungroup()%>%mutate(Flota=Fleet)%>%mutate(Flota= revalue( Fleet,c("GNS_DEF_60-79_0_0"="artisanal", "GNS_DEF_>=100_0_0"="artisanal","GNS_DEF_80-99_0_0"="artisanal", "GTR_DEF_60-79_0_0"="artisanal", "LHM_DEF_0_0_0" ="artisanal", "LHM_DWS_0_0_0" ="artisanal","LHM_SPF_0_0_0"="artisanal", "LLS_DEF_0_0_0"="artisanal", "MIS_MIS_0_0_0_HC"="artisanal","OTB_DEF_>=55_0_0"="arrastre",  "OTB_DEF_70-99_0_0"="arrastre",  "OTB_MPD_>=55_0_0" ="arrastre","OTB_DEF_>=70_0_0"="arrastre" ,"PS_SPF_0_0_0"="cerco","PTB_MPD_>=55_0_0"="arrastre"  )))%>%group_by(Fleet,FishingArea,  Season)%>%ungroup()
sampledby%>% select(a0:a15plus) %>% rowSums(na.rm=TRUE) ->sampledby$total#number individuals by metier
sampledby<-sampledby%>%select(Fleet,Flota,FishingArea,Season,total)%>%spread(Season, total)%>%arrange(Flota,Fleet, FishingArea)
head(sampledby)
sampledby# only 8ce + 8cw


### 02.2.3 COMBINED SAMPLED + CATCHES
#sampledcatch only refered to reported catch

sampledcatch<-left_join(sampledby%>%`colnames<-`(c("Fleet","Flota", "FishingArea", "SQ1" , "SQ2" ,"SQ3","SQ4" ) ),filter(catchby%>% `colnames<-`(c( "Fleet","FishingArea", "ReportingCategory", "CQ1" , "CQ2" ,"CQ3","CQ4","Flota" )),ReportingCategory=="R"))%>%arrange(Flota,Fleet, FishingArea)
sampledcatch


#table catch and sampled abundance by metier and quarter.
### Para ver donde hacer ponderaciones en orden captura y abundancia con muestreo ##ABUNDANCES**

##reported
table_sintetic_rep<-left_join(catchby,sampledby, by=c("Fleet","FishingArea","Flota" ))%>%arrange(FishingArea,Fleet,Flota,ReportingCategory)%>%filter(ReportingCategory=="R")%>%arrange(Flota,Fleet, FishingArea)
table_sintetic_rep

# if no reported not sampled
#length distribution are only over REPORTED catch (not included NON-REPORTED). non-reported have not LD


##AD-HOC#################################

### 04  BUIBING MATRICES

### 04.1   BY FLEETS

### 04.1.1 ARTISANAL

### 04.1.1.a  TABLES

#all artisanal metiers (VIRTUAL)all metiers with sampled catch (VIRTUAL). SQ  sampled catches by quarter; CQ  catches by quarter

table_sintetic_rep%>%filter(Flota=="artisanal")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  

#sampled artisanal metiers and div   (VIRTUAL) only metiers  with sampled catches##ABUNDANCES (VIRTUAL)

table_sintetic_rep%>%filter(Flota=="artisanal")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%select(SQ1:SQ4)%>%mutate(tot=rowSums(., na.rm=T))%>%bind_cols(select(filter(table_sintetic_rep, Flota=="artisanal"),Fleet,FishingArea,ReportingCategory, Flota))%>%filter(tot>0)%>%select(Fleet, FishingArea)
#sampled metiers  
#   GNS_DEF_60-79_0_0    27.8.c.e;  GNS_DEF_60-79_0_0    27.8.c.w; GNS_DEF_80-99_0_0    27.8.c.e; GNS_DEF_80-99_0_0    27.8.c.w

#  no sampled artisanal  metiers  (VIRTUAL)

table_sintetic_rep%>%filter(Flota=="artisanal")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%select(SQ1:SQ4)%>%mutate(tot=rowSums(., na.rm=T))%>%bind_cols(select(filter(table_sintetic_rep, Flota=="artisanal"),Fleet,FishingArea,ReportingCategory, Flota))%>%filter(tot==0)%>%select(Fleet, FishingArea)  
#  no sampled metiers

#GNS_DEF_>=100_0_0    27.8.c.e
#2  GNS_DEF_>=100_0_0    27.8.c.w
#3  GTR_DEF_60-79_0_0    27.8.c.e
#4  GTR_DEF_60-79_0_0    27.8.c.w
#5      LHM_DEF_0_0_0    27.8.c.e
#6      LHM_DEF_0_0_0    27.8.c.w
#7      LHM_SPF_0_0_0    27.8.c.e
#8      LLS_DEF_0_0_0    27.8.c.e
#9      LLS_DEF_0_0_0    27.8.c.w
#10  MIS_MIS_0_0_0_HC    27.8.c.e
#11  MIS_MIS_0_0_0_HC    27.8.c.w
#12  MIS_MIS_0_0_0_HC    27.8.d.2

##8.c.w  sampled artisanal. catage; SQ  sampled catches by quarter; CQ  catches by quarter

table_sintetic_rep%>%filter(Flota=="artisanal", FishingArea=="27.8.c.w")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) ) 


##8.c.e sampled artisanal. catage ; SQ  sampled catches by quarter; CQ  catches by quarter 

table_sintetic_rep%>%filter(Flota=="artisanal", FishingArea=="27.8.c.e")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) ) 
#sampled metiers  *********************8ce 8cw
#  GNS_DEF_60-79_0_0    27.8.c.e -27.8.c.w ,  GNS_DEF_80-99_0_0    27.8.c.e-27.8.c.w 

### 04.1.1.b  Ratio samped vs non-sampled (Artisanal)

##ajuste de capt no sampled-ratio para catage

catchnosamp_art<-table_sintetic_rep%>%filter(Flota=="artisanal")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) ) %>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e") %>%filter(is.na(SQ1)|is.na(SQ2)|is.na(SQ3)|is.na(SQ4))%>% select(-SQ1,-SQ2,-SQ3,-SQ4,-ReportingCategory, -Flota)%>%gather(Season,catch,3:6)%>%mutate(Season= revalue(Season, c("CQ1"="1","CQ2"="2","CQ3"="3","CQ4"="4")))%>%  group_by(FishingArea, Season)%>%summarise(catchnosamp=sum(catch,na.rm=T))

rationosamp_art<-catchby%>%filter(Flota=="artisanal")%>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e")%>%ungroup()%>%data.frame()%>%select(-ReportingCategory)%>%gather(Season,catch,3:6)%>%mutate(Season= revalue(Season, c("X1"="1","X2"="2","X3"="3","X4"="4")))%>%  group_by(FishingArea, Season)%>%summarise(catch=sum(catch,na.rm=T))%>%left_join(catchnosamp_art)%>%replace(is.na(.), 0)%>%mutate(ratio=(catchnosamp/(catch-catchnosamp))+1)%>%data.frame()%>%  mutate(Season=as.factor(as.character(Season)))%>%select(-catch,-catchnosamp)

### 04.1.1.c  Matrices by subdivision (Artisanal)
####CAT@GE

##8cw

catage_art_8cw<-select(filter(catage8cw, FishingArea=="27.8.c.w", Fleet=="GNS_DEF_60-79_0_0"|Fleet=="GNS_DEF_80-99_0_0"),Fleet,Season,a0:a15plus)%>%group_by(Season)%>%summarise_at(vars(a0:a15plus), funs(sum))%>%gather(Age, n, a0:a15plus)%>%arrange(Season)%>%data.frame()
catage_art_8cw$Season<-factor(as.character(catage_art_8cw$Season))
head(catage_art_8cw)

lmeanage_art_8cw<-filter(lmeanage_8cw, FishingArea=="27.8.c.w", Fleet=="GNS_DEF_60-79_0_0"|Fleet=="GNS_DEF_80-99_0_0")%>%arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8cw,Age, n, a0:a15plus), Flota=="artisanal"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=lgth_mean*n)%>%group_by(Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%mutate(lgth_mean=sumprod/n_sum)%>%select(Season,Age,lgth_mean)%>%data.frame()%>%mutate(Age = as.factor(as.character(Age)))
lmeanage_art_8cw$Age<- factor(lmeanage_art_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_art_8cw<-arrange(lmeanage_art_8cw,Season,Age)%>%data.frame()
head(lmeanage_art_8cw)

wmeanage_art_8cw<-filter(wmeanage_8cw, FishingArea=="27.8.c.w", Fleet=="GNS_DEF_60-79_0_0"|Fleet=="GNS_DEF_80-99_0_0")%>%arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8cw,Age, n, a0:a15plus), Flota=="artisanal"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=wght_mean*n)%>%group_by(Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%mutate(wght_mean=sumprod/n_sum)%>%select(Season,Age,wght_mean)%>%data.frame()%>%mutate(Age = as.factor(as.character(Age)))
wmeanage_art_8cw$Age<- factor(wmeanage_art_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
wmeanage_art_8cw<-arrange(wmeanage_art_8cw,Season,Age)%>%data.frame()
  
head(wmeanage_art_8cw)

##ajuste a ratio de no sampled

catage_art_8cw<-catage_art_8cw%>%  left_join(filter(rationosamp_art,FishingArea=="27.8.c.w" ))%>%  select(-FishingArea)%>%mutate(n=n*ratio)%>%  select(-ratio)%>%data.frame()
head(catage_art_8cw)


##8ce

catage_art_8ce<-select(filter(catage8ce, FishingArea=="27.8.c.e", Fleet=="GNS_DEF_60-79_0_0"|Fleet=="GNS_DEF_80-99_0_0"),Fleet,Season,a0:a15plus)%>%group_by(Season)%>%summarise_at(vars(a0:a15plus), funs(sum))%>%gather(Age, n, a0:a15plus)%>%arrange(Season)%>%data.frame()
catage_art_8ce$Season<-factor(as.character(catage_art_8ce$Season))
head(catage_art_8ce)

lmeanage_art_8ce<-filter(lmeanage_8ce, FishingArea=="27.8.c.e", Fleet=="GNS_DEF_60-79_0_0"|Fleet=="GNS_DEF_80-99_0_0")%>%arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8ce,Age, n, a0:a15plus), Flota=="artisanal"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=lgth_mean*n)%>%group_by(Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%mutate(lgth_mean=sumprod/n_sum)%>%select(Season,Age,lgth_mean)%>%data.frame()%>%mutate(Age = as.factor(as.character(Age)))
lmeanage_art_8ce$Age<- factor(lmeanage_art_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_art_8ce<-arrange(lmeanage_art_8ce,Season,Age)%>%data.frame()
head(lmeanage_art_8ce)

wmeanage_art_8ce<-filter(wmeanage_8ce, FishingArea=="27.8.c.e", Fleet=="GNS_DEF_60-79_0_0"|Fleet=="GNS_DEF_80-99_0_0")%>%arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8ce,Age, n, a0:a15plus), Flota=="artisanal"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=wght_mean*n)%>%group_by(Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%mutate(wght_mean=sumprod/n_sum)%>%select(Season,Age,wght_mean)%>%data.frame()%>%mutate(Age = as.factor(as.character(Age)))
wmeanage_art_8ce$Age<- factor(wmeanage_art_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
wmeanage_art_8ce<-arrange(wmeanage_art_8ce,Season,Age)%>%data.frame()

head(wmeanage_art_8ce)

##ajuste a ratio de no sampled

catage_art_8ce<-catage_art_8ce%>%  left_join(filter(rationosamp_art,FishingArea=="27.8.c.e" ))%>%  select(-FishingArea)%>%mutate(n=n*ratio)%>%  select(-ratio)%>%data.frame()
head(catage_art_8ce)


##  8ab NON


### 04.1.2  ARRASTRE(TRAWL)

#Metiers:
#OTB_DEF_>=70_0_0 <-Arrastre de fondo con puertas dirigido demersales  mallas superiores a 70 mm Aguas atlánticas francesas (divisiones ICES VIIIabd)
# OTB_DEF_BIG=55_0_0<-Arrastre de CantÃ¡brico-noroeste y aguas portuguesas "baca" para  captura  especies demersales.
#OTB_MPD_BIG=55_0_0   Arrastre de CantÃ¡brico-noroeste realizado con arte tipo "jurelera"  dirigido a peces pelÃ¡gicos
#PTB_MPD_BIG=55_0_0   Pareja de CantÃ¡brico-noroeste
##OTB_DEF_>=55_0_0<-"baca" Arrastre de CantÃ¡brico-noroeste y aguas portuguesas  


### 04.1.2.a TABLES

#all trawl metiers.   all trawl metiers  reported all div*****VIRTUAL***; SQ  sampled catches by quarter; CQ  catches by quarter

table_sintetic_rep%>%filter(Flota=="arrastre")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  
#  OTB_DEF_>=55_0_0    27.8.c.e               
#2  OTB_DEF_>=55_0_0    27.8.c.w                
#3  OTB_DEF_>=70_0_0      27.8.a                 
#4  OTB_DEF_>=70_0_0      27.8.b                 
#5 OTB_DEF_70-99_0_0    27.7.j.2                 
#6  OTB_MPD_>=55_0_0    27.8.c.e                 
#7  OTB_MPD_>=55_0_0    27.8.c.w                 
#8  PTB_MPD_>=55_0_0    27.8.c.e                 
#9  PTB_MPD_>=55_0_0    27.8.c.w                

#sampled trawl metiers  reported.  VIRTUAL sampled metiers catches + abundances

table_sintetic_rep%>%filter(Flota=="arrastre")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%group_by( Fleet,FishingArea, ReportingCategory ,Flota)%>%summarise(totSQ=sum(SQ1,SQ2,SQ3,SQ4, na.rm=T)) %>%left_join(filter(table_sintetic_rep, Flota=="arrastre"))%>%filter(totSQ>0)%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"Total_SQ","SQ1" , "SQ2" ,"SQ3","SQ4" ) ) 
#sampled trawl metiers  reported
#  OTB_DEF_>=55_0_0    27.8.c.e 
#  OTB_DEF_>=55_0_0    27.8.c.w 
# OTB_MPD_>=55_0_0    27.8.c.e
# OTB_MPD_>=55_0_0    27.8.c.w 


#not sampled trawl metiers  reported VIRTUAL##*********truco de los tildes en colnames**********

table_sintetic_rep%>%filter(Flota=="arrastre")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%group_by( Fleet,FishingArea, ReportingCategory ,Flota)%>%summarise(totSQ=sum(SQ1,SQ2,SQ3,SQ4)) %>%left_join(filter(table_sintetic_rep, Flota=="arrastre"))%>%filter(is.na(totSQ))%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "Flota","totSQ", "CQ1", "CQ2", "CQ3", "CQ4" , "SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%select(Fleet, FishingArea,SQ1,SQ2,SQ3,SQ4)
#  no sampled metiers:

#   OTB_DEF_>=70_0_0      27.8.a    
#   OTB_DEF_>=70_0_0      27.8.b 
#   OTB_MPD_>=55_0_0    27.8.c.e  SQ3
#  OTB_DEF_70-99_0_0    27.7.j.2    
#  PTB_MPD_>=55_0_0    27.8.c.e 
#  PTB_MPD_>=55_0_0    27.8.c.w  

### 04.1.2.b  Ratio reprted vs unreported (unallocated) (Arrastre)
##AÃ?RATIO UNREPORTED   unallocated

catchby%>%filter(ReportingCategory=="N"&Flota=="arrastre")%>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e")

#all(report+ unallocated)
catchby%>%filter(Flota=="arrastre")%>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e")%>%arrange(Fleet, FishingArea)

sampledcatch%>%filter(Flota=="arrastre")##ojo OTB_MPD_>=55_0_0<<<<<< Q3 trim no sampled

#table sampled or not
table_sintetic_rep%>%filter(Flota=="arrastre")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) ) %>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e") 


### 04.1.2.c  Ratio sampled vs non-sampled (Arrastre)
##ajuste de capt no sampled-ratio para catage

catchnosamp_arr<-table_sintetic_rep%>%filter(Flota=="arrastre")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) ) %>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e") %>%  filter(is.na(SQ1)|is.na(SQ2)|is.na(SQ3)|is.na(SQ4))

#solucion AD-HOC. problema de metier con sampled y non-sampled quarter a la vez
catchnosamp_arr[1,c(4,5,7)]<-0
catchnosamp_arr
catchnosamp_arr<-catchnosamp_arr%>%  select(-SQ1,-SQ2,-SQ3,-SQ4,-ReportingCategory, -Flota)%>%gather(Season,catch,3:6)%>%mutate(Season= revalue(Season, c("CQ1"="1","CQ2"="2","CQ3"="3","CQ4"="4")))%>%  group_by(FishingArea, Season)%>%summarise(catchnosamp=sum(catch,na.rm=T)) 
  
catchnosamp_arr

rationosamp_arr<-catchby%>%filter(Flota=="arrastre")%>%filter(FishingArea=="27.8.c.w"|FishingArea=="27.8.c.e")%>%ungroup()%>%data.frame()%>%select(-ReportingCategory)%>%gather(Season,catch,3:6)%>%mutate(Season= revalue(Season, c("X1"="1","X2"="2","X3"="3","X4"="4")))%>%  group_by(FishingArea, Season)%>%summarise(catch=sum(catch,na.rm=T))%>%left_join(catchnosamp_arr)%>%replace(is.na(.), 0)%>%mutate(ratio=(catchnosamp/(catch-catchnosamp))+1)%>%data.frame()%>%  mutate(Season=as.factor(as.character(Season)))%>%select(-catch,-catchnosamp)
rationosamp_arr


### 04.1.2.d  Matrices by subdivision

###8cw

 catchbynonrep_arr_8cw<-catchby%>%gather(Season,catch,4:7)%>%
   filter(Flota=="arrastre"&FishingArea=="27.8.c.w"&ReportingCategory=="N")%>%
   arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
   group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
   summarise( non_rep=sum(catch,na.rm=T))
 
##ratio capt reporten and no-reported 
catchbytot_ratio_arr_8cw<- catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="arrastre"&FishingArea=="27.8.c.w")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( total=sum(catch,na.rm=T))%>%left_join(catchbynonrep_arr_8cw)%>%replace(is.na(.), 0)%>%
  mutate(ratio=(non_rep/(total-non_rep))+1)%>%data.frame()%>%
  mutate(Season=as.factor(as.character(Season)))
catchbytot_ratio_arr_8cw

####CAT@GE

##with non reported catch<- catage_arr_8cw
catage_arr_8cw<-select(filter(catage8cw, FishingArea=="27.8.c.w", Flota=="arrastre"),Fleet,Season,a0:a15plus)%>%gather(Age, n, a0:a15plus)%>%arrange(Fleet,Season)%>%mutate(Season=as.factor(as.character(Season)))
catage_arr_8cw$Age<- factor(catage_arr_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
catage_arr_8cw<-catage_arr_8cw%>%left_join(catchbytot_ratio_arr_8cw)%>%
  select(-total,-non_rep,-FishingArea, -Flota)%>%mutate(n=n*ratio)%>%
  select(-ratio)%>%group_by(Season,Age)%>%summarise(n=sum(n,na.rm=T))%>%data.frame()

catage_arr_8cw


lmeanage_arr_8cw<-filter(lmeanage_8cw, FishingArea=="27.8.c.w",Flota=="arrastre")%>%
  arrange(Season,Fleet)%>%
  left_join(arrange(filter(gather(catage8cw,Age, n, a0:a15plus), Flota=="arrastre"), Fleet, Season))%>%
  select(-FishingArea)%>%mutate(prod=lgth_mean*n)%>%group_by(Flota,Season, Age)%>%
  summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(lgth_mean=sumprod/n_sum)%>%select(Season,Age,lgth_mean)%>%
  data.frame()%>%mutate(Age = as.factor(as.character(Age)))
lmeanage_arr_8cw$Age<- factor(lmeanage_arr_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_arr_8cw<-arrange(lmeanage_arr_8cw,Season,Age)%>%data.frame()

lmeanage_arr_8cw


wmeanage_arr_8cw<-filter(wmeanage_8cw, FishingArea=="27.8.c.w", Flota=="arrastre")%>%
  arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8cw,Age, n, a0:a15plus), Flota=="arrastre"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=wght_mean*n)%>%
  group_by(Flota,Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(wght_mean=sumprod/n_sum)%>%select(Season,Age,wght_mean)%>%
  data.frame()%>%mutate(Age = as.factor(as.character(Age)))
wmeanage_arr_8cw$Age<- factor(wmeanage_arr_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
wmeanage_arr_8cw<-arrange(wmeanage_arr_8cw,Season,Age)%>%data.frame()

wmeanage_arr_8cw

##ajuste a ratio de no sampled teoricamente tenia que ser antes pero es asi (metiers vs total)

catage_arr_8cw<-catage_arr_8cw%>%  left_join(filter(rationosamp_arr,FishingArea=="27.8.c.w" ))%>%  select(-FishingArea)%>%mutate(n=n*ratio)%>%  select(-ratio)%>%data.frame()


###8ce

catchbynonrep_arr_8ce<-catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="arrastre"&FishingArea=="27.8.c.e"&ReportingCategory=="N")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( non_rep=sum(catch,na.rm=T))

##ratio capt reporten and no-reported 
catchbytot_ratio_arr_8ce<- catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="arrastre"&FishingArea=="27.8.c.e")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( total=sum(catch,na.rm=T))%>%left_join(catchbynonrep_arr_8ce)%>%replace(is.na(.), 0)%>%
  mutate(ratio=(non_rep/(total-non_rep))+1)%>%data.frame()%>%
  mutate(Season=as.factor(as.character(Season)))
catchbytot_ratio_arr_8ce

##with non reported catch<- catage_arr_8ce

catage_arr_8ce<-select(filter(catage8ce, FishingArea=="27.8.c.e", Flota=="arrastre"),Fleet,Season,a0:a15plus)%>%gather(Age, n, a0:a15plus)%>%arrange(Fleet,Season)%>%mutate(Season=as.factor(as.character(Season)))
catage_arr_8ce$Age<- factor(catage_arr_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
catage_arr_8ce<-catage_arr_8ce%>%left_join(catchbytot_ratio_arr_8ce)%>%
  select(-total,-non_rep,-FishingArea, -Flota)%>%mutate(n=n*ratio)%>%
  select(-ratio)%>%group_by(Season,Age)%>%summarise(n=sum(n,na.rm=T))%>%data.frame()

catage_arr_8ce


lmeanage_arr_8ce<-filter(lmeanage_8ce, FishingArea=="27.8.c.e",Flota=="arrastre")%>%
  arrange(Season,Fleet)%>%
  left_join(arrange(filter(gather(catage8ce,Age, n, a0:a15plus), Flota=="arrastre"), Fleet, Season))%>%
  select(-FishingArea)%>%mutate(prod=lgth_mean*n)%>%group_by(Flota,Season, Age)%>%
  summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(lgth_mean=sumprod/n_sum)%>%select(Season,Age,lgth_mean)%>%
  data.frame()%>%mutate(Age = as.factor(as.character(Age)))
lmeanage_arr_8ce$Age<- factor(lmeanage_arr_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_arr_8ce<-arrange(lmeanage_arr_8ce,Season,Age)%>%data.frame()

lmeanage_arr_8ce


wmeanage_arr_8ce<-filter(wmeanage_8ce, FishingArea=="27.8.c.e", Flota=="arrastre")%>%
  arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8ce,Age, n, a0:a15plus), Flota=="arrastre"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=wght_mean*n)%>%
  group_by(Flota,Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(wght_mean=sumprod/n_sum)%>%select(Season,Age,wght_mean)%>%
  data.frame()%>%mutate(Age = as.factor(as.character(Age)))
wmeanage_arr_8ce$Age<- factor(wmeanage_arr_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
wmeanage_arr_8ce<-arrange(wmeanage_arr_8ce,Season,Age)%>%data.frame()

wmeanage_arr_8ce

##ajuste a ratio de no sampled

catage_arr_8ce<-catage_arr_8ce%>%  left_join(filter(rationosamp_arr,FishingArea=="27.8.c.e" ))%>%  select(-FishingArea)%>%mutate(n=n*ratio)%>%  select(-ratio)%>%data.frame()


### 04.1.3  PURSE SEINE (CERCO)

#Metiers:   PS_SPF_0_0_0

### 04.1.3.a  TABLES

#all purseseiners metiers.  all purseseiners metiers  reported all div**VIRTUAL**SQ  sampled catches by quarter; CQ  catches by quarter

table_sintetic_rep%>%filter(Flota=="cerco")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%group_by( Fleet,FishingArea, ReportingCategory ,Flota)%>%summarise(totCQ=sum(CQ1,CQ2,CQ3,CQ4, na.rm=T)) %>%left_join(filter(table_sintetic_rep, Flota=="cerco"))%>%filter(totCQ>0)%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory","Flota","totC", "CQ1", "CQ2", "CQ3", "CQ4" , "SQ1" , "SQ2" ,"SQ3","SQ4" ) ) 


#sampled purseseiners metiers  reported.  VIRTUAL sampled metiers catches + abundances; SQ  sampled catches by quarter; CQ  catches by quarter
table_sintetic_rep%>%filter(Flota=="cerco")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%group_by( Fleet,FishingArea, ReportingCategory ,Flota)%>%summarise(tot_SQ=sum(SQ1,SQ2,SQ3,SQ4, na.rm=T)) %>%left_join(filter(table_sintetic_rep, Flota=="cerco"))%>%filter(tot_SQ>0)%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory","Flota","tot_SQ", "CQ1", "CQ2", "CQ3", "CQ4" , "SQ1" , "SQ2" ,"SQ3","SQ4" ) ) 
#sampled trawl metiers  reported
#PS_SPF_0_0_0    27.8.c.e 27.8.c.w 

#not sampled purseseiners metiers  ***reported ****VIRTUAL not sampled; SQ  sampled catches by quarter; CQ  catches by quarter

table_sintetic_rep%>%filter(Flota=="cerco")%>%ungroup()%>%data.frame()%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "CQ1", "CQ2", "CQ3", "CQ4" , "Flota" ,"SQ1" , "SQ2" ,"SQ3","SQ4" ) )  %>%group_by( Fleet,FishingArea, ReportingCategory ,Flota)%>%summarise(totSQ=sum(SQ1,SQ2,SQ3,SQ4)) %>%left_join(filter(table_sintetic_rep, Flota=="cerco"))%>%filter(is.na(totSQ))%>%`colnames<-`(c("Fleet","FishingArea","ReportingCategory", "Flota","tot_SQ", "CQ1", "CQ2", "CQ3", "CQ4" , "SQ1" , "SQ2" ,"SQ3","SQ4" ) ) 


#   solo en 27.8.b 
#  no sampled metiers
#   PS_SPF_0_0_0    27.8.b

#All sampled

sampledcatch%>%filter(Flota=="cerco")##o


### 04.1.3.b    Matrices by subdivision (CERCO)

#####ponderar no reported con reported (unallocated)

###8cw

##AÃ?RATIO UNREPORTED
#catches : reported and no-reported
catchby%>%filter(ReportingCategory=="N"&Flota=="cerco"&FishingArea=="27.8.c.w")
catchby%>%filter(ReportingCategory=="R"&Flota=="cerco"&FishingArea=="27.8.c.w"&ReportingCategory=="R"&Flota=="cerco")

catchbynonrep_psn_8cw<-catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="cerco"&FishingArea=="27.8.c.w"&ReportingCategory=="N")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( non_rep=sum(catch,na.rm=T))

##ratio capt reporten and no-reported 

catchbytot_ratio_psn_8cw<- catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="cerco"&FishingArea=="27.8.c.w")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( total=sum(catch,na.rm=T))%>%left_join(catchbynonrep_psn_8cw)%>%
  mutate(ratio=(non_rep/(total-non_rep))+1)%>%data.frame()%>%
  mutate(Season=as.factor(as.character(Season)))

catchbytot_ratio_psn_8cw


####CAT@GE

catage_psn_8cw<-select(filter(catage8cw, FishingArea=="27.8.c.w", Flota=="cerco"),Fleet,Season,a0:a15plus)%>%gather(Age, n, a0:a15plus)%>%arrange(Fleet,Season)%>%
  mutate(Season=as.factor(as.character(Season)))
catage_psn_8cw$Age<- factor(catage_psn_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
catage_psn_8cw<-catage_psn_8cw%>%left_join(catchbytot_ratio_psn_8cw)%>%
  select(-total,-non_rep,-FishingArea, -Flota)%>%mutate(n=n*ratio)%>%
  select(-ratio)%>%group_by(Season,Age)%>%summarise(n=sum(n,na.rm=T))%>%data.frame()

catage_psn_8cw

lmeanage_psn_8cw<-filter(lmeanage_8cw, FishingArea=="27.8.c.w",Flota=="cerco")%>%
  arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8cw,Age, n, a0:a15plus), Flota=="cerco"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=lgth_mean*n)
lmeanage_psn_8cw$Age<- factor(lmeanage_psn_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_psn_8cw<-lmeanage_psn_8cw%>%group_by(Flota,Season, Age)%>%
  summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(lgth_mean=sumprod/n_sum)%>%select(Flota,Season,Age,lgth_mean)%>%data.frame()%>%mutate(Age = as.factor(as.character(Age)))
lmeanage_psn_8cw$Age<- factor(lmeanage_psn_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_psn_8cw<-arrange(lmeanage_psn_8cw,Season,Age)%>%data.frame()

lmeanage_psn_8cw


wmeanage_psn_8cw<-filter(wmeanage_8cw, FishingArea=="27.8.c.w", Flota=="cerco")%>%
  arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8cw,Age, n, a0:a15plus), Flota=="cerco"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=wght_mean*n)%>%
  group_by(Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(wght_mean=sumprod/n_sum)%>%select(Season,Age,wght_mean)%>%data.frame()%>%
  mutate(Age = as.factor(as.character(Age)))
wmeanage_psn_8cw$Age<- factor(wmeanage_psn_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
wmeanage_psn_8cw<-arrange(wmeanage_psn_8cw,Season,Age)%>%data.frame()

wmeanage_psn_8cw


###8ce 

##AÃ?RATIO UNREPORTED

#catches : reported and no-reported
catchby%>%filter(ReportingCategory=="N"&Flota=="cerco"&FishingArea=="27.8.c.e")#no hay non rep
catchby%>%filter(ReportingCategory=="R"&Flota=="cerco"&FishingArea=="27.8.c.e"&ReportingCategory=="R"&Flota=="cerco")

catchbynonrep_psn_8ce<-catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="cerco"&FishingArea=="27.8.c.e"&ReportingCategory=="N")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( non_rep=sum(catch,na.rm=T))##no hay

##ratio capt reporten and no-reported 
catchbytot_ratio_psn_8ce<- catchby%>%gather(Season,catch,4:7)%>%
  filter(Flota=="cerco"&FishingArea=="27.8.c.e")%>%
  arrange(Fleet, FishingArea,ReportingCategory,Season)%>%
  group_by(Fleet, FishingArea,Flota, Season)%>%select(-ReportingCategory)%>%
  summarise( total=sum(catch,na.rm=T))%>%left_join(catchbynonrep_psn_8ce)%>% 
  replace(is.na(.), 0) %>%
  mutate(ratio=(non_rep/(total-non_rep))+1)%>%data.frame()%>%
  mutate(Season=as.factor(as.character(Season)))

catchbytot_ratio_psn_8ce

####CAT@GE


catage_psn_8ce<-select(filter(catage8ce, FishingArea=="27.8.c.e", Flota=="cerco"),Fleet,Season,a0:a15plus)%>%gather(Age, n, a0:a15plus)%>%arrange(Fleet,Season)%>%
  mutate(Season=as.factor(as.character(Season)))
catage_psn_8ce$Age<- factor(catage_psn_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
catage_psn_8ce<-catage_psn_8ce%>%left_join(catchbytot_ratio_psn_8ce)%>%
  select(-total,-non_rep,-FishingArea, -Flota)%>%mutate(n=n*ratio)%>%
  select(-ratio)%>%group_by(Season,Age)%>%summarise(n=sum(n,na.rm=T))%>%data.frame()

catage_psn_8ce

lmeanage_psn_8ce<-filter(lmeanage_8ce, FishingArea=="27.8.c.e",Flota=="cerco")%>%
  arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8ce,Age, n, a0:a15plus), Flota=="cerco"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=lgth_mean*n)
lmeanage_psn_8ce$Age<- factor(lmeanage_psn_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_psn_8ce<-lmeanage_psn_8ce%>%group_by(Flota,Season, Age)%>%
  summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(lgth_mean=sumprod/n_sum)%>%select(Flota,Season,Age,lgth_mean)%>%data.frame()%>%mutate(Age = as.factor(as.character(Age)))
lmeanage_psn_8ce$Age<- factor(lmeanage_psn_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
lmeanage_psn_8ce<-arrange(lmeanage_psn_8ce,Season,Age)%>%data.frame()

lmeanage_psn_8ce


wmeanage_psn_8ce<-filter(wmeanage_8ce, FishingArea=="27.8.c.e", Flota=="cerco")%>%
  arrange(Season,Fleet)%>%left_join(arrange(filter(gather(catage8ce,Age, n, a0:a15plus), Flota=="cerco"), Fleet, Season))%>%select(-FishingArea)%>%mutate(prod=wght_mean*n)%>%
  group_by(Season, Age)%>%summarise(sumprod=sum(prod,na.rm=T),n_sum=sum(n,na.rm=T))%>%
  mutate(wght_mean=sumprod/n_sum)%>%select(Season,Age,wght_mean)%>%data.frame()%>%
  mutate(Age = as.factor(as.character(Age)))
wmeanage_psn_8ce$Age<- factor(wmeanage_psn_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
wmeanage_psn_8ce<-arrange(wmeanage_psn_8ce,Season,Age)%>%data.frame()

wmeanage_psn_8ce

### 04.1.3.c    Matrices by subdivision (CERCO)

##NO HAY falta no rep 8w



### 04.1.4 Catage summarazing  #BY FLEET


catage_arr_8c<-left_join(catage_arr_8ce,catage_arr_8cw, by=c("Season", "Age"))%>%group_by(Season, Age)%>%summarise(n=sum(n.x,n.y, na.rm=T))
catage_psn_8c <-left_join(catage_psn_8ce,catage_psn_8cw, by=c("Season", "Age"))%>%group_by(Season, Age)%>%summarise(n=sum(n.x,n.y, na.rm=T))

catage_art_8c <-left_join(catage_art_8ce,catage_art_8cw, by=c("Season", "Age"))%>%group_by(Season, Age)%>%summarise(n=sum(n.x,n.y, na.rm=T))

write.csv(catage_arr_8c,"catage_arr_8c_ieo_SP.csv", row.names =F, na="")
write.csv(catage_art_8c,"catage_art_8c_ieo_SP.csv", row.names =F, na="")
write.csv(catage_psn_8c,"catage_psn_8c_ieo_SP.csv", row.names =F, na="")

### 04.2  Catage matrices

### 04.2.1   BY SUBDIVISIONS
####CAT@GE

###8ce

#catage_psn_8ce
#catage_arr_8ce
#catage_art_8ce
catage_tot_8ce<-bind_cols(catage_art_8ce,catage_arr_8ce,catage_psn_8ce)%>%data.frame()%>%
  select(-Age1,-Age2,-Season1,-Season2)%>%group_by(Season, Age)%>% 
  replace(is.na(.), 0) %>%mutate(n_8ce=n+n1+n2) %>%
  select(-n,-n1,-n2)%>%data.frame()%>%
  mutate(Season=as.factor(as.character(Season)))
catage_tot_8ce$Age<- factor(catage_tot_8ce$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
catage_tot_8ce<-arrange(catage_tot_8ce,Season,Age)%>%data.frame()
head(catage_tot_8ce)

#lmeanage_psn_8ce
#lmeanage_arr_8ce
#lmeanage_art_8ce
lmeanage_tot_8ce<-bind_cols(lmeanage_art_8ce,catage_art_8ce,lmeanage_arr_8ce,catage_arr_8ce,lmeanage_psn_8ce,catage_psn_8ce)%>%
  data.frame()%>%select(-Age1,-Age2,-Season1,-Season2,-Flota,-Season3,-Age3,-Flota1,-Season4,-Age4,-Season5,-Age5)%>% 
  replace(is.na(.), 0)%>% mutate(lgth_mean_8ce=lgth_mean*n+lgth_mean1*n1+lgth_mean2*n2) %>%
  select(-n,-n1,-n2,-lgth_mean,-lgth_mean1,-lgth_mean2)%>%
  mutate(lgth_mean_8ce=lgth_mean_8ce/catage_tot_8ce[ ,"n_8ce"])%>%
  data.frame()%>%  mutate(Season=as.factor(as.character(Season)))
head(lmeanage_tot_8ce)

#wmeanage_psn_8ce
#wmeanage_arr_8ce
#wmeanage_art_8ce
wmeanage_tot_8ce<-bind_cols(wmeanage_art_8ce,catage_art_8ce,wmeanage_arr_8ce,catage_arr_8ce,wmeanage_psn_8ce,catage_psn_8ce)%>%
  data.frame()%>%select(-Age1,-Age2,-Season1,-Season2,-Flota,-Season3,-Age3,-Season4,-Age4,-Season5,-Age5)%>%
  replace(is.na(.), 0)%>% 
  mutate(wght_mean_8ce=wght_mean*n+wght_mean1*n1+wght_mean2*n2) %>%
  select(-n,-n1,-n2,-wght_mean,-wght_mean1,-wght_mean2)%>%
  mutate(wght_mean_8ce=wght_mean_8ce/catage_tot_8ce[ ,"n_8ce"])%>%
  data.frame()%>%  mutate(Season=as.factor(as.character(Season)))

#catage_tot_8ce
#lmeanage_tot_8ce
#wmeanage_tot_8ce

Ca_8ce_16_ieo<-left_join(catage_tot_8ce,lmeanage_tot_8ce)%>%left_join(wmeanage_tot_8ce)
  
str(Ca_8ce_16_ieo)
 
#write.csv(Ca_8ce_16_ieo, "Ca_8ce_16_SP_ieo.csv", row.names =F)


 ###8cw
 
 #catage_psn_8cw
 #catage_arr_8cw
 #catage_art_8cw
catage_tot_8cw<-bind_cols(catage_art_8cw,catage_arr_8cw,catage_psn_8cw)%>%data.frame()%>%
   select(-Age1,-Age2,-Season1,-Season2)%>%group_by(Season, Age)%>% 
   replace(is.na(.), 0) %>%mutate(n_8cw=n+n1+n2) %>%
   select(-n,-n1,-n2)%>%data.frame()%>%
   mutate(Season=as.factor(as.character(Season)))
catage_tot_8cw$Age<- factor(catage_tot_8cw$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
 catage_tot_8cw<-arrange(catage_tot_8cw,Season,Age)%>%data.frame()
 head(catage_tot_8cw)
 
 #lmeanage_psn_8cw
 #lmeanage_arr_8cw
 #lmeanage_art_8cw
lmeanage_tot_8cw<-bind_cols(lmeanage_art_8cw,catage_art_8cw,lmeanage_arr_8cw,catage_arr_8cw,lmeanage_psn_8cw,catage_psn_8cw)%>%
   data.frame()%>%select(-Age1,-Age2,-Season1,-Season2,-Flota,-Season3,-Age3,-Flota1,-Season4,-Age4,-Season5,-Age5)%>% 
   replace(is.na(.), 0)%>% mutate(lgth_mean_8cw=lgth_mean*n+lgth_mean1*n1+lgth_mean2*n2) %>%
   select(-n,-n1,-n2,-lgth_mean,-lgth_mean1,-lgth_mean2)%>%
   mutate(lgth_mean_8cw=lgth_mean_8cw/catage_tot_8cw[ ,"n_8cw"])%>%
   data.frame()%>%  mutate(Season=as.factor(as.character(Season)))
 
 #wmeanage_psn_8cw
 #wmeanage_arr_8cw
 #wmeanage_art_8cw
wmeanage_tot_8cw<-bind_cols(wmeanage_art_8cw,catage_art_8cw,wmeanage_arr_8cw,catage_arr_8cw,wmeanage_psn_8cw,catage_psn_8cw)%>%
   data.frame()%>%select(-Age1,-Age2,-Season1,-Season2,-Flota,-Season3,-Age3,-Season4,-Age4,-Season5,-Age5)%>%
   replace(is.na(.), 0)%>% 
   mutate(wght_mean_8cw=wght_mean*n+wght_mean1*n1+wght_mean2*n2) %>%
   select(-n,-n1,-n2,-wght_mean,-wght_mean1,-wght_mean2)%>%
   mutate(wght_mean_8cw=wght_mean_8cw/catage_tot_8cw[ ,"n_8cw"])%>%
   data.frame()%>%  mutate(Season=as.factor(as.character(Season)))
 head(wmeanage_tot_8cw)

 #catage_tot_8cw
 #lmeanage_tot_8cw
 #wmeanage_tot_8cw
 
Ca_8cw_16_ieo<-left_join(catage_tot_8cw,lmeanage_tot_8cw)%>%left_join(wmeanage_tot_8cw)
 
str(Ca_8cw_16_ieo)

#write.csv(Ca_8cw_16_ieo, "Ca_8cw_16_SP_ieo.csv", row.names =F)
 

### 04.2.2   BY DIVISION

 #catage_tot_8cw
 #catage_tot_8ce
catage_8c_16<-bind_cols(catage_tot_8cw,catage_tot_8ce)%>%data.frame()%>%
   select(-Age1,-Season1)%>%group_by(Season, Age)%>% 
   replace(is.na(.), 0) %>%mutate(n_8c=n_8cw+n_8ce) %>%
   select(-n_8cw,-n_8ce)%>%data.frame()%>%
   mutate(Season=as.factor(as.character(Season)))
 
catage_8c_16$Age<- factor(catage_8c_16$Age,levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15plus"))
catage_8c_16<-arrange(catage_8c_16,Season,Age)%>%data.frame()
 
 #lmeanage_tot_8cw
 #lmeanage_tot_8ce
lmeanage_8c_16<-bind_cols(lmeanage_tot_8cw,catage_tot_8cw,lmeanage_tot_8ce,catage_tot_8ce)%>%
   data.frame()%>%select(-Age1,-Age2,-Season1,-Season2,-Season3,-Age3)%>% 
   replace(is.na(.), 0)%>% mutate(lgth_mean_8c=lgth_mean_8cw*n_8cw+lgth_mean_8ce*n_8ce) %>%
   select(-n_8ce,-n_8cw,-lgth_mean_8cw,-lgth_mean_8ce)%>%
   mutate(lgth_mean_8c=lgth_mean_8c/catage_8c_16[ ,"n_8c"])%>%
   data.frame()%>%  mutate(Season=as.factor(as.character(Season)))
 
 #wmeanage_tot_8cw
 #wmeanage_tot_8ce
 wmeanage_8c_16<-bind_cols(wmeanage_tot_8cw,catage_tot_8cw,wmeanage_tot_8ce,catage_tot_8ce)%>%
   data.frame()%>%select(-Age1,-Age2,-Season1,-Season2,-Season3,-Age3)%>%
   replace(is.na(.), 0)%>% 
   mutate(wght_mean_8c=wght_mean_8cw*n_8cw+wght_mean_8ce*n_8ce) %>%
   select(-n_8cw,-n_8ce,-wght_mean_8cw,-wght_mean_8ce)%>%
   mutate(wght_mean_8c=wght_mean_8c/catage_8c_16[ ,"n_8c"])%>%
   data.frame()%>%  mutate(Season=as.factor(as.character(Season)))

 
 #catage_tot_8cw
 #lmeanage_tot_8cw
 #wmeanage_tot_8cw
 Ca_8c_16_ieo<-left_join(catage_8c_16,lmeanage_8c_16)%>%left_join(wmeanage_8c_16)
 
 str(Ca_8c_16_ieo)
 
 write.csv(Ca_8c_16_ieo, "Ca_8c_16_SP_ieo a ver.csv", row.names =F)
 

### 05  Catches by subdiv & quarter

catch_8abcd_16_ieo<-  catchby%>%`colnames<-`(c("Fleet", "FishingArea","ReportingCategory", "Q1" , "Q2" ,"Q3","Q4","Flota" ) )%>%select(-Fleet,-Flota)%>%mutate(FishingArea=revalue(FishingArea , c("27.8.c.e" ="27.8.c","27.8.c.w" ="27.8.c" )))%>%  group_by(FishingArea, ReportingCategory)%>%summarise_at(vars(Q1:Q4),funs(sum(. ,na.rm=T)) )

  
write.csv(catch_8abcd_16_ieo, "catch_8abcd_16_ieo.csv", row.names =F)



write.csv(catchby, "catch by metier_ieo.csv", row.names =F)
#write.csv(filter(catchby,FishingArea=="VIIIcE"), "output/catch by metier_8cE_ieo.csv", row.names =F)
#write.csv(filter(catchby,FishingArea=="VIIIcW"), "output/catch by metier_8cW_ieo.csv", row.names =F)
#write.csv(filter(catchby,FishingArea=="VIIIa"), "output/catch by metier_8a_ieo.csv", row.names =F)
#write.csv(filter(catchby,FishingArea=="VIIIb"), "output/catch by metier_8b_ieo.csv", row.names =F)
#write.csv(filter(catchby,FishingArea=="VIIId"), "output/catch by metier_8d_ieo.csv", row.names =F)



#write.csv(t(catage_tot_VIIIcE), "output/catage_tot_VIIIcE.csv", row.names =F)
#write.csv(t(lmeanage_tot_VIIIcE), "output/lmeanage_tot_VIIIcE.csv", row.names =F)
#write.csv(t(wmeanage_tot_VIIIcE), "output/wmeanage_tot_VIIIcE.csv", row.names =F)


#write.csv(t(catage_tot_VIIIcW), "output/catage_tot_VIIIcW.csv", row.names =F)
#write.csv(t(lmeanage_tot_VIIIcW), "output/lmeanage_tot_VIIIcW.csv", row.names =F)
#write.csv(t(wmeanage_tot_VIIIcW), "output/wmeanage_tot_VIIIcW.csv", row.names =F)

#write.csv(t(catage_tot_VIIIa), "output/catage_tot_VIIIa.csv", row.names =F)
#write.csv(t(lmeanage_tot_VIIIa), "output/lmeanage_tot_VIIIa.csv", row.names =F)
#write.csv(t(wmeanage_tot_VIIIa), "output/wmeanage_tot_VIIIa.csv", row.names =F) 
 
#write.csv(t(catage_tot_VIIIb), "output/catage_tot_VIIIb.csv", row.names =F)
#write.csv(t(lmeanage_tot_VIIIb), "output/lmeanage_tot_VIIIb.csv", row.names =F)
#write.csv(t(wmeanage_tot_VIIIb), "output/wmeanage_tot_VIIIb.csv", row.names =F )



#write.csv(totalcatch_8cE, "output/totalcatch_8cE_ieo.csv", row.names =F)
#write.csv(totalcatch_8cW, "output/totalcatch_8cW_ieo.csv", row.names =F)
#write.csv(totalcatch_8a, "output/totalcatch_8a_ieo.csv", row.names =F )


#write.csv(unallocatedcatch_8cE, "output/unallocatedcatch_8cE_ieo.csv", row.names =F)
#write.csv(unallocatedcatch_8cW, "output/unallocatedcatch_8cW_ieo.csv", row.names =F)
#write.csv(unallocatedcatch_8a, "output/unallocatedcatch_8a_ieo.csv", row.names =F )
#write.csv(unallocatedcatch_8b, "output/unallocatedcatch_8b_ieo.csv", row.names =F )
#write.csv(unallocatedcatch_8d, "output/unallocatedcatch_8d_ieo.csv", row.names =F )




### 06 Samplig intensity


###Number INDIVIDUALS and SAMPLES by quarter and subdivision

#NumSamplesLngt:number length samples  
#NumLngtMeas: number of length sampled individuals 
#Reimpottamos SD por problemas de datos en muestreos y samples

##AD HOC
IC_HOM_W_2016.SD_2<-read.table("SD_IC_2016_10414_T_Gerson_nmuestras.csv",header=F,sep=",",na.strings ="")

#ponemos nombres a las variable del archivo intercatch
colnames(IC_HOM_W_2016.SD_2)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")

IC_HOM_W_2016.SD_2[IC_HOM_W_2016.SD_2==-9]<-sqrt(-1) # replace -9 to NA
IC_HOM_W_2016.SD_2$AgeLength<- as.numeric(as.character(IC_HOM_W_2016.SD_2$AgeLength))# pasamos la variable talla a foprmato numerico. . Al importarlo automaticament le habÃ?a asignado formato factor


SAMPLING<-  IC_HOM_W_2016.SD_2%>%select(Fleet,FishingArea,Season,NumSamplesLngt,NumLngtMeas)%>%group_by(Fleet,FishingArea,  Season)%>%summarise_at(vars(NumSamplesLngt: NumLngtMeas),funs(mean(.,na.rm=T)))%>%ungroup()%>%select(FishingArea,  Season,NumSamplesLngt,NumLngtMeas )%>%group_by(FishingArea,  Season)%>%summarise_at(vars(NumSamplesLngt, NumLngtMeas),funs(sum(.,na.rm=T)) )%>%data.frame()


write.csv(SAMPLING, "sampling by subdiv_ieo_2016.csv", row.names =F)






#########################################################length distributions by gear



LD_gear<-LD%>%ungroup()%>%mutate(Flota= revalue( Fleet,c("GNS_DEF_60-79_0_0"="artisanal", "GNS_DEF_>=100_0_0"="artisanal","GNS_DEF_80-99_0_0"="artisanal", "GTR_DEF_60-79_0_0"="artisanal", "LHM_DEF_0_0_0" ="artisanal", "LHM_DWS_0_0_0" ="artisanal","LHM_SPF_0_0_0"="artisanal", "LLS_DEF_0_0_0"="artisanal", "MIS_MIS_0_0_0_HC"="artisanal","OTB_DEF_>=55_0_0"="arrastre",  "OTB_MCD_>=55_0_0"="arrastre",  "OTB_MPD_>=55_0_0" ="arrastre","OTB_DEF_>=70_0_0"="arrastre" ,"PS_SPF_0_0_0"="cerco","PTB_MPD_>=55_0_0"="arrastre" )))
table(LD_gear$FishingArea)  

LD_trawl_8c<-LD_gear%>%filter(Flota=="arrastre")%>%filter(FishingArea=="27.8.c.e"|FishingArea=="27.8.c.w")%>%select( Season,AgeLength, canum, Flota)%>%group_by( Season,AgeLength,  Flota)%>%summarise_all(funs(sum(., na.rm=T) ))%>%ungroup()%>%  spread(Season,canum)
LD_trawl_8c[is.na(LD_trawl_8c)]<-0#pasar na a 0 pro

LD_art_8c<-LD_gear%>%filter(Flota=="artisanal")%>%filter(FishingArea=="27.8.c.e"|FishingArea=="27.8.c.w")%>%select( Season,AgeLength, canum, Flota)%>%group_by( Season,AgeLength,  Flota)%>%summarise_all(funs(sum(., na.rm=T) ))%>%ungroup()  %>%  spread(Season,canum)
LD_art_8c[is.na(LD_art_8c)]<-0#pasar na a 0 pro    

LD_ps_8c<-LD_gear%>%filter(Flota=="cerco")%>%filter(FishingArea=="27.8.c.e"|FishingArea=="27.8.c.w")%>%select( Season,AgeLength, canum, Flota)%>%group_by( Season,AgeLength,  Flota)%>%summarise_all(funs(sum(., na.rm=T) ))%>%ungroup()%>%  spread(Season,canum)
LD_ps_8c[is.na(LD_ps_8c)]<-0#pasar na a 0 pro

write.csv(LD_trawl_8c, "HOM_LD_trawl_8c_2016_ieo.csv", row.names =F)
write.csv(LD_art_8c, "HOM_LD_art_2016_8c_ieo.csv.csv", row.names =F)
write.csv(LD_ps_8c, "HOM_LD_psn_2016_8c_ieo.csv.csv", row.names =F)


####catch by metier to official catch

#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH#############FORMAT INTERCATCH


#n_otolites_9an<-#1130

head(IC_HOM_W_2016.SD,3)








