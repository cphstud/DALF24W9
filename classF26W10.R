library(jsonlite)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(stringr)
source("util.R")


dfMatch=fromJSON("3893795.json", flatten = T)
dfShots=dfMatch %>% filter(type.name=="Shot")

dfShots=dfShots %>% select(dfShotV)

dkShots=dfShots %>% filter(str_detect(team.name,"Denmar"))
dkShots$endx=unlist(lapply(dkShots$shot.end_location, function(x) x[1]))
dkShots$endy=unlist(lapply(dkShots$shot.end_location, function(x) x[2]))
dkShots$shx=unlist(lapply(dkShots$location, function(x) x[1]))
dkShots$shy=unlist(lapply(dkShots$location, function(x) x[2]))

# plot one shot
onShot=dkShots[8,]
trishot=plotSingleShotTri(onShot[[1,'location']])

ggplot(onShot) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_polygon(data=trishot,aes(x=sx,y=sy),alpha=0.4)+
  geom_segment(aes(x = shx, 
                   y = shy, 
                   xend = endx,
                   yend=endy),
               colour = "yellow",
               size = 1) +
  theme_pitch() +
  coord_flip(xlim = c(49, 121)) +
  scale_y_reverse() +
  geom_text(aes(x=shx,y=shy,label=player.name), size=2.5,vjust=1)+
  geom_point(aes(x=shx,y=shy,color=team.name), size=2.5)


# freezeframe
phff=onShot$shot.freeze_frame[[1]]
onShot$teammate=T
onShotff=onShot %>% select(ffv)
#ffv=colnames(phff)
#ffv=ffv[-c(3,5)]
ffv=readRDS("freezevector.rds")
phff=phff %>% select(ffv)
phff=rbind(onShotff,phff)
phff$shooter=F
phff$opInTri=0
phff[1,'shooter']=T

colnames(onShot)

phff$ox=unlist(lapply(phff$location, function(x) x[1]))
phff$oy=unlist(lapply(phff$location, function(x) x[2]))

phff$shx=onShot$shx
phff$shy=onShot$shy
phff$shootername=onShot$player.name

# plot freezeframe
ggplot() +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_polygon(data=trishot,aes(x=sx,y=sy),alpha=0.4)+
  geom_point(data=phff,aes(x=ox,y=oy, color=teammate))+
  theme_pitch() +
  coord_flip(xlim = c(49, 121)) +
  scale_y_reverse() +
  geom_text(data=phff,aes(x=ox,y=oy,label=player.name), size=2.5,vjust=1)+
  geom_point(data=phff,aes(x=ox,y=oy,color=teammate), size=2.5)


# get shooters inTri
phff <- phff %>% rowwise() %>% mutate(isinT = is_opponent_inside_triangle(ox,oy,shx,shy)) %>% ungroup()
phff[1,'opInTri']=sum(phff$isinT)


# get teammates inTri
teammates=which(phff$teammate==T)
teammates=teammates[c(2,length(teammates))]
isEgo=F
for (i in teammates) {
  phff$shx=phff[i,'ox']
  phff$shy=phff[i,'oy']
  phff <- phff %>% rowwise() %>% mutate(isinT = is_opponent_inside_triangle(ox,oy,shx,shy)) %>% ungroup()
  phff[i,'opInTri']=sum(phff$isinT)
  isEgo=ifelse(phff[i,'opInTri']<phff[1,'opInTri'],T,F)
}
onShot$isEgo=isEgo

















