library(StatsBombR)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(StatsBombR)
source("utilClass.R")


Comp <- FreeCompetitions() 
FComp <- FreeCompetitions() %>%
  filter(competition_id %in% c(72) ) #
Matches <- FreeMatches(FComp) 

# Kvindernes VM 2023, competitionid=107
VMMatches=Matches %>% filter(season.season_id==107)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData = allclean(StatsBombData) #


# find en kamp
dkm <- StatsBombData %>% filter(team.id==853)
dkm$match_id %>% unique()
dkmatch <- dkm %>% filter(match_id==3893795)
dkmatch <- StatsBombData %>% filter(match_id==3893795)
dkmatch <- StatsBombData %>% filter(team.id==853)

dkmshot <- dkmatch %>% filter(type.name=="Shot")

# angle and dist to goal
#tl = unlist(dkmshot[1,'location'])
dkmshotsel <- dkmshot[,c(74,77,25,26,27,28,21,22,10,115,150,151,156,157)]
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(angle=mangle(unlist(location)))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(dist=disttogoal(unlist(location)))
dkpshotsel <- dkmshotsel %>% filter(team.id==853)
chpshotsel <- dkmshotsel %>% filter(team.id==1207)

# plot a shot

dftest=dkpshotsel %>% filter(player.name=="Josefine Hasbo")
ggplot(dftest) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_segment(aes(x = location.x, 
                   y = location.y, 
                   xend = shot.end_location.x,
                   yend=shot.end_location.y),
               colour = "yellow",
               size = 1) +
  theme_pitch() +
  coord_flip(xlim = c(49, 121)) +
  scale_y_reverse() +
  geom_text(aes(x=location.x,y=location.y,label=player.name), size=2.5,vjust=1)+
  geom_point(aes(x=location.x,y=location.y,color=team.name), size=2.5)


# another shot with tri
tridf <- plotSingleShotTri(dkpshotsel8$location[[1]])
ggplot(tridf, aes(x=sx,y=sy))+
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_polygon(alpha=0.4 )+
  theme_pitch() +
  ggtitle("Simple passmap aylor", 
          "ggsoccer example")+
coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
geom_text(data=dkpshotsel[8,],aes(x=location.x,y=location.y,label=player.name), size=2.5,vjust=1)+
geom_point(data=dkpshotsel[8,],aes(x=location.x,y=location.y,color=team.name), size=2.5)


# test frame
dkpshotsel8 = dkpshotsel[8,]
testff <- dkpshotsel8$shot.freeze_frame[[1]]
testff <- testff %>% rowwise() %>% mutate(x=location[1])
testff <- testff %>% rowwise() %>% mutate(y=location[2])

# plot tri and players colored by team
ggplot() +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_polygon(data=tridf,aes(x=sx,y=sy),alpha=0.4, fill = "blue" )+
  geom_point(data=testff,aes(x = x, y = y, color=teammate), size = 2) +
  geom_point(data=dkpshotsel8,aes(x = location.x, y = location.y, color="black"), size = 4) +
  theme_pitch() +
  direction_label() +
  ggtitle("Simple passmap Taylor", 
          "ggsoccer example")+
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
  geom_text(data=testff,aes(x=x,y=y,label=player.name), angle=90,size=2.5,vjust=-1)+
  geom_text(data=dkpshotsel8,aes(x=location.x,y=location.y,label=player.name), size=4.5,vjust=1)

# THE MATH
## number of opponent in the tri
### for each opponent test wether on not in tri
#### comput total area of tri including the x,y of opponent
##### if aO > a then outside

# shooters og modstandere.

nrow(dkpshotsel)
  
  #for (k in (1:nrow(dkpshotsel))) {
  for (k in (1:20)) {
    print(c("DO ",k))
    shooter <- dkpshotsel[k,]
    testff <- shooter$shot.freeze_frame[[1]]
    tryCatch({
      
    testff <- testff %>% rowwise() %>% mutate(x=location[1])
    testff <- testff %>% rowwise() %>% mutate(y=location[2])
    dft <- testff %>% filter(teammate==T)
    dfo <- testff %>% filter(teammate==F)
    numOfOpps = 0
    for (i in (1:nrow(dfo))) {
      numOfOpps[i]=is_opponent_inside_triangle(dfo[i,'x'],dfo[i,'y'],shooter$location.x,shooter$location.y)
    }
    shooterNumOfOpps=sum(numOfOpps)
    # shooters teammates og modstandere.
    # teammates og modstandere
    for (j in (1:nrow(dft))) {
      numOfOpps = 0
      for (i in (1:nrow(dfo))) {
        numOfOpps[i]=is_opponent_inside_triangle(dfo[i,'x'],dfo[i,'y'],dft[j,'x'],dft[j,'y'])
      }
      teammateNumOfOpps=sum(numOfOpps)
      dft[j,'numops']=teammateNumOfOpps
    }
    
    tjek = dft$numops < shooterNumOfOpps
    dkpshotsel[k,'ego'] = ifelse(sum(tjek) > 0,T,F)
    })
  }      
      