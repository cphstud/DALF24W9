library(jsonlite)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(stringr)
source("util.R")




em2023F=allMatches %>% filter(competition.competition_id==53)
em2024M=allMatches %>% filter(competition.competition_id==55) %>% sample_n(31)

em2023FMIDS=em2023F$match_id
em2023FEvents=getAllEventsMultipleMatches(em2023FMIDS)

# get all freezeframes
# get all shots
em2023FShots=em2023FEvents %>% filter(type.name=="Shot")
shotv=readRDS("shotvector.rds")
em2023FShotsSub=em2023FShots %>% select(shotv)
em2023FShotsSub$endx=unlist(lapply(em2023FShotsSub$shot.end_location, function(x) x[1]))
em2023FShotsSub$endy=unlist(lapply(em2023FShotsSub$shot.end_location, function(x) x[2]))
em2023FShotsSub$shx=unlist(lapply(em2023FShotsSub$location, function(x) x[1]))
em2023FShotsSub$shy=unlist(lapply(em2023FShotsSub$location, function(x) x[2]))

em2023FShotsSub = em2023FShotsSub %>% rowwise() %>% 
  mutate(isEgo=calcEgo(shot.freeze_frame,player.name,player.id,position.name,location))


calcEgo <- function(ff,pln,plid,posn,loc) {
  retval=T
  pln=em2023FShotsSub[[1,'player.name']]
  plid=em2023FShotsSub[[1,'player.id']]
  posn=em2023FShotsSub[[1,'position.name']]
  loc=(em2023FShotsSub[[1,'location']])
  
  ffv=readRDS("freezevector.rds")
  df=as.data.frame(em2023FShotsSub[1,])
  ff=flatten(em2023FShotsSub[[1,'shot.freeze_frame']])
  ff=ff %>% select(ffv)
  
  new_row <- data.frame(
    location = I(list(loc)),
    teammate = TRUE,
    player.name = pln,
    position.name = posn,
    player.id = plid
  )
  ff=rbind(new_row,ff)
  
  ff$shooter=F
  ff$opInTri=0
  ff[1,'shooter']=T
  ff$ox=unlist(lapply(ff$location, function(x) x[1]))
  ff$oy=unlist(lapply(ff$location, function(x) x[2]))
  
  # get val for shooter
  oplist=ff %>% filter(teammate==F)
  
  shx=ff[1,'ox']
  shy=ff[1,'oy']
  num=0
  for (i in (1:nrow(oplist))) {
    tmpnum=is_opponent_inside_triangle(oplist[i,'ox'],oplist[i,'oy'],shx,shy)
    num=num+tmpnum 
  }
  ff[1,'opInTri']=num
  # done shooter
  
  tlist=ff %>% filter(teammate==T)
  tlist=tlist[2:nrow(tlist),]
  
  for (t in (1:nrow(tlist))) {
    shx=ff[t,'ox']
    shy=ff[t,'oy']
    num=0
    for (i in (1:nrow(oplist))) {
      tmpnum=is_opponent_inside_triangle(oplist[i,'ox'],oplist[i,'oy'],shx,shy)
      num=num+tmpnum 
    }
    tlist[t,'opInTri']=num
    # done shooter
  }
  tlist=tlist %>% select(player.id,opInTri)
  # get value back to ff
  # set isEgo based on opInTri
  # return isEgo to shot-dataframe
  return(retval)
}



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
#phff$shootername=onShot$player.name

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

















