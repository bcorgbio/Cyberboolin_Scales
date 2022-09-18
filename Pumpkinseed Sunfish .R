#Rachel Zhu - module 2

library(tidyverse)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#fish: The fish number in the experiment.
#speed: The voltage sent to the motor driving the prop that is moving the water. Higher voltages result in a faster motor speed.
#frame: The frame in the experiment video from which the data are taken.
#date: A date in the format “year-month-day-hourminutesecond” that identifies when the experiment started. This is unique for each experiment.
#amp: Fin amplitude in pixels.
#fin: From which fin, left or right, the amplitude was recorded.
#amp.bl: The specific fin amplitude as a proportion of body length (BL−1).

#joined 2 tables --> new tibble
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

#pseed bodylength
pseed.bl%>%
  print()

pseed2%>%
  select(fish)%>%
  unique()

#joining psee.bl to pseed2
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

#adding a new column
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#plotting fin amplitude(y) against swimming speed(x)
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()

#transparancy
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)

#left pelvic fin for ONE experiment
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

#new package
library(features)

#breaking up pseed2 table
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1

#extracting important data from the results
fget(f1)

#plotting vertical lines that correspond to critical points over OG plot
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)

#defines the amplitudes
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

#pulling out the peaks
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

#new column, peaks
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

#number of experiments?
pseed2%>%
  summarize(n=length(unique(date)))

#define find.peaks
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

#Filter to limit operations
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

#tibble w/ 3300 amplitudes
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

#linear regression model
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

#P value
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

#means v speeds linear regression for each fish
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#How to compute the sum of the amplitude for each frame?

#Add column (amp.sum) to the tibble
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
#violates tidy principle

#deletes 1 row for each frame in each experiment
pseed2 %>%
  filter(fin=="R")
#lost half our data, amp.sum only applies to the right fin

#Pivots data (makes it wider), left and right fin have their own amplitude column, we can then sum these values to get the sum of amplitude
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#How does the sum of amplitude of both fins vary over our range of speeds?

