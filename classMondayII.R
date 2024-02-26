library(StatsBombR)
source("util.R")


FComp <- FreeCompetitions() %>%
  filter(competition_id %in% c(72) ) #
Matches <- FreeMatches(FComp) 

# Kvindernes VM 2023, competitionid=107
VMMatches=Matches %>% filter(season.season_id==107)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData = allclean(StatsBombData) #


# find en kamp
dkmatch <- StatsBombData %>% filter(match_id==3893795)
dkmshot <- dkmatch %>% filter(type.name=="Shot")

# angle and dist to goal
#tl = unlist(dkmshot[1,'location'])
dkmshotsel <- dkmshot[,c(74,77,25,26,21,22,10,150,151,156,157)]
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



# freeze frame

testshot <- dkpshotsel[4,]
testff <- testshot$shot.freeze_frame[[1]]
testff <- testff %>% rowwise() %>% mutate(x=location[1],
                                          y=location[2])
#testff <- cbind(testff,testshot[1,c(player.name,testshot$location.x,testshot$location.y)])

dftriforshot <- dfForSingleShotTri(unlist(testshot$location))
ggplot(testff) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_point(data=testff,aes(x = x, y = y, color=teammate), size = 2) +
  geom_point(data=testshot,aes(x = location.x, y = location.y), size = 4) +
  geom_segment(data=testshot,aes(x = location.x, 
                   y = location.y, 
                   xend = shot.end_location.x,
                   yend=shot.end_location.y),
               colour = "yellow",
               size = 1) +
  geom_polygon(data = dftriforshot,aes(x=sx,y=sy),alpha=0.5)+
  theme_pitch() +
  direction_label() +
  ggtitle("Simple passmap Taylor", 
          "ggsoccer example")+
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
  geom_text(data=testff,aes(x=x,y=y,label=player.name), size=3.5,vjust=1)+
  geom_text(data=testshot,aes(x=location.x,y=location.y,label=player.name), size=4.5,vjust=1)




dfForSingleShotTri <-  function(shot) {
  g1=c(120, 36)
  g2=c(120,44)
  x=c(shot[1],g1[1],g2[1],shot[1])
  y=c(shot[2],g1[2],g2[2],shot[2])
  resshot=data.frame(x,y)
  colnames(resshot)=c("sx","sy")
  return(resshot)
}
