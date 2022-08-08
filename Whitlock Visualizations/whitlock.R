apr_pitch <- read.csv("whitlock_april.csv")
may_pitch <- read.csv("whitlock_may.csv")

library(tidyverse)

head(apr_pitch)
apr_pitch <- apr_pitch %>%
  janitor::clean_names() %>%
  mutate(pitch = i_pitch_type, break_x = pfx_x, break_z = pfx_z) %>%
  select(pitch, break_x, break_z, plate_x, plate_z)

may_pitch <- may_pitch %>%
  janitor::clean_names() %>%
  mutate(pitch = i_pitch_type, break_x = pfx_x, break_z = pfx_z) %>%
  select(pitch, break_x, break_z, plate_x, plate_z)



#' 
#' Recreating Pitch Charts
#' 

#creating strike zone rectangle
x <- c(-.95, .95, .95, -.95, -.95)
z <- c(1.6, 1.6, 3.5, 3.5, 1.6)
k_zone <- data.frame(x,z)

#plotting it all
ggplot(apr_pitch, aes(plate_x, plate_z)) +
  geom_point(aes(fill = pitch), col = "black", size = 3, pch = 21) +
  geom_path(k_zone, mapping = aes(x = x, y = z)) +
  theme(aspect.ratio = 1.5) +
  labs(col = "Pitch", title = "Garrett Whitlock", subtitle = "April 8 - May 4, 2022") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title = element_blank())

ggplot(may_pitch, aes(plate_x, plate_z)) +
  geom_point(aes(fill = pitch), col = "black", size = 3, pch = 21) +
  geom_path(k_zone, mapping = aes(x = x, y = z)) +
  theme(aspect.ratio = 2.5) +
  labs(col = "Pitch", title = "Garrett Whitlock", subtitle = "May 10 - June 1, 2022") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title = element_blank())



#' 
#' Heat Maps by pitch of frequency thrown and location in the zone for
#' entire first two months of the season
#' 

all_pitch <- rbind(apr_pitch, may_pitch)

#creating strike zone rectangle
x <- c(-.95, .95, .95, -.95, -.95)
z <- c(1.6, 1.6, 3.5, 3.5, 1.6)
k_zone <- data.frame(x,z)

ggplot(all_pitch, aes(plate_x, plate_z)) +
  facet_wrap(~pitch, nrow = 1) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)), contour_var = "ndensity", bins = 9) +
  scale_fill_distiller(palette = "RdBu") +
  geom_point(alpha = .05) +
  geom_path(k_zone, mapping = aes(x, z)) +
  theme(aspect.ratio = 2) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank())

#geom_density_2d charts by frequency of pitch
#geom_contour allows the contour to be created according to a third variable
#this could be useful for determining batter's hot and cold spots

#Ultimately, heat maps are most useful for batters, but this allows us
#to have another (possibly easier) way to visualize where Whitlock 
#tends to throw each of his pitches the most



#' 
#' Finally, using this data, I will create the break charts, which is
#' what I originally split the data for. His strikeouts and velocity 
#' were noticeably and consistently higher in April compared to May.
#' I wonder if the break on his pitches have changed much, signifying
#' a change in how he is throwing his pitches.
#' 
#' Break Charts
#' 

ggplot(apr_pitch, aes(break_x, break_z, col = pitch)) +
  geom_point() +
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("Whitlock Break (Apr 8 - May 4)") +
  theme(aspect.ratio = 1.3)


ggplot(may_pitch, aes(break_x, break_z, col = pitch)) +
  geom_point() +
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("Whitlock Break (May 10 - June 1)") +
  theme(aspect.ratio = 1.3)

#There is notably less break in the May chart for all the non-Slider pitches
#For the slider more pitches have positive break, which means there is some
#backspin being put on the sliders. This usually creates an inefficient SL, 
#so may be worth looking at. The main positive sign is that mostly all his sliders
#still have slight positive horizontal break, different from the rest of his
#pitches. This is also more effective since he does not have a curve.

#Next, I am going to move on to another data set that I can use for more modeling
#of data rather than just visualizing.