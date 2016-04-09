storm<-read.csv("repdata_data_StormData.csv")
data<-storm[,c("EVTYPE","FATALITIES","INJURIES")]


data$EVTYPE<-gsub("TSTM","Thunderstorm",data$EVTYPE,fixed = FALSE,ignore.case = TRUE)
tndbool<-grepl("Tornado",data$EVTYPE,fixed = FALSE,ignore.case = TRUE)
marinehailbool<-grepl("marine hail",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
wfbool<-grepl("Wild Fire",data$EVTYPE,fixed = FALSE,ignore.case = TRUE)
marinehighwindbool<-grepl("marine high",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
marinetstmbool<-grepl("marine thunderstorm",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[marinetstmbool]<-"MarineThunderstorm"
tsmbool<-grepl("Thunderstorm",data$EVTYPE,fixed = FALSE,ignore.case = TRUE)&!marinetstmbool
hwbool<-grepl("High Wind",data$EVTYPE,fixed = FALSE,ignore.case = TRUE)&!marinehighwindbool
flashbool<-grepl("flash",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
lsfld2bool<-grepl("Lake flood",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
lsfld1bool<-grepl("Lakeshore",data$EVTYPE,ignore.case = TRUE, fixed = FALSE) 
lsfldbool<-lsfld2bool|lsfld1bool
cstlfldbool<-grepl("coastal flood",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
tdlfldbool<-grepl("tidal flood",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
coastalfldbool<-tdlfldbool|cstlfldbool
floodbool<-grepl("flood",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
floodbool<-floodbool&!flashbool&!coastalfldbool&!lsfldbool
hailbool<-grepl("hail",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)&!marinehailbool
frostbool<-grepl("frost",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
freezebool<-grepl("freeze",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
fzftzebool<-freezebool|frostbool
trndobool<-grepl("tornado",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
exheatbool<-grepl("excessive heat",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|
  grepl("extreme heat",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
heatbool<-grepl("heat",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
heatbool<-heatbool&!exheatbool
srgbool<-grepl("surge",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)

data$EVTYPE[marinehailbool]<-"MarineHail"
data$EVTYPE[trndobool]<-"Tornado"
data$EVTYPE[wfbool]<-"Wild Fire"
data$EVTYPE[tsmbool]<-"Thunderstorm"
data$EVTYPE[hwbool]<-"HighWind"
data$EVTYPE[flashbool]<-"FlashFlood"
data$EVTYPE[lsfldbool]<-"LakeshoreFlood"
data$EVTYPE[floodbool]<-"Flood"
data$EVTYPE[coastalfldbool]<-"CoastalFlood"
data$EVTYPE[hailbool]<-"Hail"
data$EVTYPE[fzftzebool]<-"Frost/Freeze"
data$EVTYPE[exheatbool]<-"ExcessiveHeat"
data$EVTYPE[srgbool]<-"StormSurge/Tide"
data$EVTYPE[heatbool]<-"Heat"
trpstormbool<-grepl("tropical storm",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[trpstormbool]<-"Tropicalstorm"
ashbool<-grepl("volcanic ash",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[ashbool]<-"VolcanicAsh"
data$EVTYPE[trpstormbool]<-"TropicalStorm"
waterbool1<-grepl("waterspout",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
waterbool2<-grepl("water spout",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
waterbool<-waterbool1|waterbool2
data$EVTYPE[waterbool]<-"Waterspout"
winterstormbool<-grepl("winter storm",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[winterstormbool]<-"WinterStorm"
winterwthrbool<-grepl("winter weather",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[winterwthrbool]<-"WinterWeather"
justwind<-data$EVTYPE%in%"WIND"
data$EVTYPE[justwind]<-"StrongWinds"
currentbool<-grepl("current",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[currentbool]<-"RipCurrent"
ltbool<-grepl("lightning",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[ltbool]<-"Lightning"
lake_effbool<-grepl("Lake Effect",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|grepl("Lake-Effect",data$EVTYPE,ignore.case = TRUE,fixed = "False")
data$EVTYPE[lake_effbool]<-"LakeEffectSnow"
icebool<-grepl("ice storm",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[icebool]<-"IceStorm"
mrstrongbool<-grepl("marine strong",data$EVTYPE,ignore.case = TRUE,fixed = FALSE)
strongwindbool<-grepl("strong wind",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|grepl("gusty wind",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|grepl("wind gusts",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)&!mrstrongbool
data$EVTYPE[strongwindbool]<-"StrongWinds"
avalanchebool<-grepl("avalan",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[avalanchebool]<-"Avalanche"
blizbool<-grepl("bliz",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[blizbool]<-"Blizzard"
excoldchillbool<-grepl("extreme",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[excoldchillbool]<-"ExtremeCold/WindChill"
data$EVTYPE[excoldchillbool]<-"ExtremeCold/WindChill"
coldbool<-grepl("chill",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)&!excoldchillbool
data$EVTYPE[coldbool]<-"Cold/WindChill"
dfogbool<-grepl("dense fog",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[dfogbool]<-"DenseFog"
fogbool<-data$EVTYPE%in%"FOG"
data$EVTYPE[fogbool]<-"Densefog"
devilbool<-grepl("devil",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[devilbool]<-"DustDevil"
dstormbool<-grepl("duststorm",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|grepl("dust storm",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[dstormbool]<-"DustStorm"
funnelbool<-grepl("funnel cloud",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[funnelbool]<-"FunnelCloud"
frfogbool<-grepl("freezing fog",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[frfogbool]<-"FreezingFog"
sltbool<-grepl("sleet",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[sltbool]<-"Sleet"
rainbool<-grepl("heavy rain",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|grepl("excessive rain",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[rainbool]<-"HeavyRain"
surfbool<-grepl("surf",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[surfbool]<-"HighSurf"
hrtfbool<-grepl("hurricane",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)|grepl("typhoon",data$EVTYPE,ignore.case = TRUE,fixed = "FALSE")
data$EVTYPE[hrtfbool]<-"Hurricane(Typhoon)"
snowbool<-grepl("heavy snow",data$EVTYPE,ignore.case = TRUE, fixed = FALSE)
data$EVTYPE[snowbool]<-"HeavySnow" 




dmg<-tapply(dmg$Exp,dmg$Exp,function(x){
  gsub("k",1000,dmg$Exp,ignore.case = TRUE,)
  gsub("b",10^9,dmg$Exp,ignore.case = TRUE,)
  gsub("m",10^6,dmg$Exp,ignore.case = TRUE,)})



for(1 in seq_along(dmg$Exp))
{
  
}  