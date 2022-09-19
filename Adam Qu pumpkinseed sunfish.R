#Adam Qu - module 2

library(tidyverse)
library(features)

#Load csv files
pseed <- read_csv("pseed.fin.amps.csv") #amplitudes
pseed.bl <- read_csv("pseed.lengths.csv") 
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

pseed.bl%>%
  print()
pseed2%>%
  select(fish)%>%
  unique()
pseed2 <- pseed2%>% # #adding body length column
  left_join(pseed.bl,by="fish")%>%
  print()
pseed2 <- pseed2%>% #mutate to get specific speed in BLxs-1
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed2%>% #plot specific fin amplitude vs specific swimming speed
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
pseed2%>% #shows oscillation of fin
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

exp1 <- pseed2%>% #
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
fget(f1) #gives critical points

pseed2%>% #plot with lines through critical points
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100) #find peaks
fget(f2)

f.tib <- fget(f2)[2:3]%>% #extract peaks (negative critical pts)
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

pseed2%>% #
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

#function for finding max oscillation amplitude for a fin
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we want to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}
pseed2%>% #applying function to dataset's first 3 experiments
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

pseed.max <- pseed2%>% #applying function to dataset
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

pseed.max%>% #linear regression of specific amplitude vs specific swimming speed
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov) #statistically significant, amplitude decreases with speed

pseed.max %>% #means for each speed and fish
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

pseed2 #want to compute sum of amplitudes for each frame
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#How does the sum of amplitude of both fins vary over our range of speeds?
#1. Compute mean maximum of amp.sum values
pseed.max.sum <- pseed.wide %>% #means for each speed and fish
  group_by(fish, amp.sum) %>% 
  summarize(amp.sum.mean = mean(amp.sum))

stdEM <- function(x){
  sd(x)/sqrt(length(x))
}

pseed.sum.se <- pseed.max.sum%>%
  group_by(fish, amp.sum,amp.sum.mean)%>%
  summarize(amp.sum.se = stdEM(amp.sum.mean))

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = "amp.sum") %>%
  print()



