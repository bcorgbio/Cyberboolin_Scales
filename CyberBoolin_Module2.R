#Rachel Zhu - module 2

#In a script named “groupname_module2.R”, combine the code above so that you can establish the pseed.wide data tibble.

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
  mutate(amp.sum=L+R)%>%
  print(amp.sum)


#Compute the mean maximum* of all the amp.sums for each specific swimming speed for each fish just like we did for mean maximum amplitude of each fin (i.e., the mean of all the max amplitudes across each experiment for each fish). Make sure this is contained in a new tibble named pseed.sum.max. Call this column amp.sum.mean.

find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.max <- pseed2%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) 

pseed.sum.max<- pseed.max %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum)) 

#plot of speed v amp.sum.mean
pseed.sum.max%>%
  ggplot(aes(x=speed,y=amp.sum.mean))+geom_point()+geom_smooth(method="lm")

#Create a custom function that computes the standard error of the mean (SE). [see below] and add a column in your summary table pseed.sum.max for SE and call it amp.sum.se.
#Using ggplot, plot the amp.sum.mean vs. specific swimming speed and add error bars that correspond to the SE of amp.sum. Be sure to color your points and error bars by specimen (fish).

StanErr <- function(x){
  sd(x)/ sqrt(length(x))
}

pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = StanErr(amp.sum))

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("speed","fish")) %>%
  print()

pseed.sum.max%>%
  ggplot(aes(x=amp.sum.mean,y=speed))+geom_point()+geom_smooth(method="lm")


#Download this file, read it as a tibble and merge it with the your new pseed.sum.max tibble. [see below].
#Use ggplot to plot the metabolic power output of each fish vs. mean maximum of amp.sum.
#Include the code for all the tasks listed above in the “groupname_module2.R” script and upload it to this link.
