#Adam Qu - module 2

library(tidyverse)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum= L+R)

view(pseed.wide)

#Compute mean maximum of all amp.sum values for each specific swimming speed for each fish

find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.max <- pseed.wide %>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) 
pseed.max$peak <- NULL

pseed.sum.max<- pseed.max %>%
  group_by(fish,speed) %>%
  summarize(amp.sum.mean=mean(amp.sum)) 

#plot of speed v amp.sum.mean
pseed.sum.max%>%
  ggplot(aes(x=speed,y=amp.sum.mean))+geom_point()+geom_smooth(method="lm")

#Use a custom function to compute standard error of the mean, add column in summary of pseed.sum.max called amp.sum.se
stdErr <- function(x){
  sd(x)/ sqrt(length(x))
}

pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = stdErr(amp.sum))

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("fish","speed")) %>%
  print()

#Plot amp.sum.mean vs. specific swimming speed. Add error bars that correspond to the SE of amp.sum
pseed.sum.max %>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.5, color="skyblue")+theme_classic()

#Read pseed.met.rate.csv as a tibble, merge it with the new pseed.sum.max tibble.
pseed.met.rate <- read_csv("pseed.met.rate.csv")

pseed.max <- pseed.max%>%
  merge(pseed.met.rate,by=c("fish","date","m.s","cm.s","bl.s"))

pseed.mean.rate <- pseed.max %>%
  group_by(fish, speed)%>%
  summarize(amp.met.rate=mean(met.rate))

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.mean.rate, by = c("speed","fish")) %>% 
  print()

#Plot the metabolic power output of each fish vs. mean maximum of amp.sum
pseed.sum.max %>%
  ggplot(aes(x=amp.met.rate,y=amp.sum.mean,col=fish))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.05, color="pink")+theme_classic()

