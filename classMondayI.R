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
