# library
library(parallel) # for parallel processing
library(msm) # for Delta method
library(MuMIn) # dredge
library(ggplot2)
library(tidyr)
library(dplyr)


# read file of read estimates
Phi.s_a04_r.p.sa02_sry.df = read.csv("Phi.s_a04_r.p.sa02_sry.real.csv")

Phi.s_a04_r.p.sa02_sry.df = Phi.s_a04_r.p.sa02_sry.df[,1:5]


# Split X column into five
Phi.s_a04_r.p.sa02_sry.df[c('par', 'group', 'cohort', 'age', 'time')] <- stringr::str_split_fixed(Phi.s_a04_r.p.sa02_sry.df$X, ' ', 5)




#######################
# read file of read estimates
all.avg = read.csv("all.avg.0201.csv")

# add age class column

all.avg.age <- all.avg %>%
  mutate(a02 = case_when(
    agegroup == "0-1y" ~ "young(0-2)",
    agegroup == "1-2y" ~ "young(0-2)",
    agegroup == "2-3y" ~ "old(2+)",
    agegroup == "3-4y" ~ "old(2+)",
    agegroup == "4y+" ~ "old(2+)",
    TRUE ~ "Other" # Default case
  ))

# create Phi dataframe
Phi.est = all.avg.age[all.avg.age$parameter=="Phi",] 
Phi.est.sar = Phi.est
Phi.est.sar$sar = paste(Phi.est.sar$season, Phi.est.sar$a02, Phi.est.sar$region, sep = "-")
# Phi plotting
Phi <- ggplot(
  Phi.est.sar, 
  aes(x = sar, y = estimate, ymin = lcl, ymax = ucl)
)
# Vertical line with point in the middle
Phi + geom_pointrange()

# create p dataframe
p.est = all.avg.age[all.avg.age$parameter=="p",] 
p.est.sar = p.est
p.est.sar$sar = paste(p.est.sar$season, p.est.sar$a02, p.est.sar$region, sep = "-")

# p plotting
P <- ggplot(
  p.est.sar, 
  aes(x = sar, y = estimate, ymin = lcl, ymax = ucl)
)

# Vertical line with point in the middle
P + geom_pointrange()


# get geometric means of whole time survivals (time effects not important)



Phi.plotting.2 = 
  Phi.plotting %>%
  group_by(region, season, agegroup) %>%
  mutate(
    gm_estimate = exp(mean(log(estimate))),  # Calculate geometric mean
    gm_se = exp(mean(log(replace(se, se ==0, 0.00001)))),       # How to calculate SE ???
    gm_ucl = exp(mean(log(replace(ucl, ucl ==0, 0.00001)))),       # How to calculate???
    gm_lcl = exp(mean(log(replace(lcl, lcl ==0, 0.00001))))       # How to calculate???
  ) %>%
  distinct(region, season, agegroup, .keep_all = TRUE)


plot(Phi.plotting$season, Phi.plotting$estimate)
plot(Phi.plotting$region, Phi.plotting$estimate)
# plotting test (Y0-2:r*s)


library(ggplot2)

Phi.plotting.2$season_low =  as.numeric(as.factor(Phi.plotting.2$season)) - 0.1 
Phi.plotting.2$season_high =  as.numeric(as.factor(Phi.plotting.2$season)) + 0.1 

RXA.young = 
  ggplot(data = Phi.plotting.2[Phi.plotting.2$agegroup=="0-1y"|Phi.plotting.2$agegroup=="1-2y",], 
         aes(x = region, y = gm_estimate, color=`season`))+
  geom_pointrange(aes(ymin = gm_estimate-gm_se, ymax = gm_estimate+gm_se)) + 
  scale_color_manual(values = c("W-S" = "deepskyblue2", "S-W" = "lightsalmon")) +
  geom_segment(aes(y = gm_lcl, yend = gm_lcl
                   #, x = region_low, xend = region_high
  ), linetype="solid", alpha=0.2, linewidth = 1) + 
  geom_segment(aes(y = gm_ucl, yend = gm_ucl
                   #, x = region_low, xend = region_high
  ), linetype="solid", alpha=0.2, linewidth = 1) +
  geom_pointrange(aes(ymin = gm_estimate-gm_se, ymax = gm_estimate+gm_se),position = position_dodge(width = 0)) + 
  labs(title="BFS Seasonal Survival in Different Age Classes",x="Wintering Regions", y = "Survival Estimates (Φ)")+
  theme_classic()+
  theme(
    legend.position=c(0.9, 0.2),
    plot.title = element_text(hjust = 0.5, size = 16),
  ) 



ggplot(data = Phi.plotting.2[!Phi.plotting.2$region=="K" & Phi.plotting.2$agegroup=="3-4y"|Phi.plotting.2$agegroup=="4y+",], 
       aes(x = region, y = gm_estimate, color=`season`))+
  geom_pointrange(aes(ymin = gm_estimate-gm_se, ymax = gm_estimate+gm_se)) + 
  scale_color_manual(values = c("W-S" = "deepskyblue2", "S-W" = "lightsalmon")) +
  #  geom_segment(aes(y = gm_lcl, yend = gm_lcl
  #                   , x = season_low, xend = season_high
  #  ), linetype="solid", alpha=0.2, linewidth = 1) + 
  # geom_segment(aes(y = gm_ucl, yend = gm_ucl
  #                  , x = season_low, xend = season_high
  # ), linetype="solid", alpha=0.2, linewidth = 1) +
  geom_pointrange(aes(ymin = gm_estimate-gm_se, ymax = gm_estimate+gm_se),position = position_dodge(width = 0.1)) + 
  labs(title="BFS Seasonal Survival in Different Age Classes",x="Wintering Regions", y = "Survival Estimates (Φ)")+
  theme_classic()+
  theme(
    legend.position=c(0.9, 0.2),
    plot.title = element_text(hjust = 0.5, size = 16),
  ) +
  ylim(0.7, 1)











############
### Plotting ####






# real estimates of the current best model
real.estimates = read.csv("Phi.WSxrxa04_TWxyxs.p.Wxr_a04.real.csv")

# [Phi] Prepare data
# Create new columns for the extracted information
real.estimates$parameter = str_extract(real.estimates[,1], "Phi|p")  # Add other parameters as needed
real.estimates$region = str_extract(real.estimates[,1], "J|K|S|T")
real.estimates$year = as.integer(str_extract(real.estimates[,1], "(?<=t)\\d+(\\.\\d+)?"))
real.estimates$age.class = ifelse(as.numeric(str_extract(real.estimates[,1], "(?<=a)(\\d+\\.?\\d*)")) < 4, "young (0–4)", "old (4+)")
real.estimates$season = ifelse(str_detect(real.estimates[,1], "t\\d+\\.5"), "W-S", "S-W")

# change p season
real.estimates$season[real.estimates$parameter == "p" & real.estimates$season == "W-S"] = "W"
real.estimates$season[real.estimates$parameter == "p" & real.estimates$season == "S-W"] = "S"


# create Phi ~ region * age dataframe
Phi.rxa.plotting = real.estimates[real.estimates$parameter=="Phi",] # some parameters are estimated 1 and I don't know how to explain them

# get geometric means of TW seasonal survivals

Phi.rxa.plotting.2 = 
  Phi.rxa.plotting %>%
  group_by(region, season, age.class) %>%
  mutate(
    gm_estimate = exp(mean(log(estimate))),  # Calculate geometric mean
    gm_se = exp(mean(log(replace(se, se ==0, 0.00001)))),       # How to calculate SE ???
    gm_ucl = exp(mean(log(replace(ucl, ucl ==0, 0.00001)))),       # How to calculate???
    gm_lcl = exp(mean(log(replace(lcl, lcl ==0, 0.00001))))       # How to calculate???
  ) %>%
  distinct(region, season, age.class, .keep_all = TRUE)

# create Phi ~ TW:y*s dataframe
Phi.TW.plotting = real.estimates[real.estimates$parameter=="Phi"&real.estimates$region=="T",] 

Phi.TW.plotting2 = Phi.TW.plotting
Phi.TW.plotting2$sxa = paste(Phi.TW.plotting$season, Phi.TW.plotting$age.class)

# [p]
# create p ~ region + age dataframe
p.plotting = real.estimates[real.estimates$parameter=="p",] # some parameters are estimated 1 and I don't know how to explain them


# [Phi] ~W-S:r*a
# replace NaNs as 0 otherwise they can't be plotted
Phi.rxa.plotting.3 = Phi.rxa.plotting.2 
Phi.rxa.plotting.3$gm_lcl[is.na(Phi.rxa.plotting.3$gm_lcl)] = 0

# for plotting lcl & ucl
Phi.rxa.plotting.3$region_low =  as.numeric(as.factor(Phi.rxa.plotting.3$region)) - 0.1 
Phi.rxa.plotting.3$region_high =  as.numeric(as.factor(Phi.rxa.plotting.3$region)) + 0.1 

# changing label & legend title names
Phi.rxa.plotting.3$region[Phi.rxa.plotting.3$region=="J"] = "Japan"
Phi.rxa.plotting.3$region[Phi.rxa.plotting.3$region=="K"] = "South Korea"
Phi.rxa.plotting.3$region[Phi.rxa.plotting.3$region=="S"] = "Southern China"
Phi.rxa.plotting.3$region[Phi.rxa.plotting.3$region=="T"] = "Taiwan"

names(Phi.rxa.plotting.3)[names(Phi.rxa.plotting.3) == 'age.class'] <- 'Age classes'

# ggpplot2
RXA = 
  ggplot(data = Phi.rxa.plotting.3[Phi.rxa.plotting.3$season=="W-S",], 
         aes(x = region, y = gm_estimate, color=`Age classes`))+
  geom_pointrange(aes(ymin = gm_estimate-gm_se, ymax = gm_estimate+gm_se)) + 
  scale_color_manual(values = c("young (0–4)" = "deepskyblue2", "old (4+)" = "deepskyblue4")) +
  geom_segment(aes(y = gm_lcl, yend = gm_lcl, x = region_low, xend = region_high), linetype="solid", alpha=0.2, linewidth = 1) + 
  geom_segment(aes(y = gm_ucl, yend = gm_ucl, x = region_low, xend = region_high), linetype="solid", alpha=0.2, linewidth = 1) +
  geom_pointrange(aes(ymin = gm_estimate-gm_se, ymax = gm_estimate+gm_se),position = position_dodge(width = 0)) + 
  labs(title="BFS Winter to Summer Survival in Different Age Classes",x="Wintering Regions", y = "Survival Estimates (Φ)")+
  theme_classic()+
  theme(
    legend.position=c(0.9, 0.2),
    plot.title = element_text(hjust = 0.5, size = 16),
  ) 


# [Phi] ~TW:y*s
# changing label names
Phi.TW.plotting2$season[Phi.TW.plotting2$season=="W-S"] = "Winter to Summer"
Phi.TW.plotting2$season[Phi.TW.plotting2$season=="S-W"] = "Summer to Winter"

# ggpplot2
TW = 
  ggplot(data = Phi.TW.plotting2, 
         aes(x = year, y = estimate, color=sxa))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill = sxa), alpha=0.1, colour = NA) +
  scale_fill_manual(values = c("salmon3","lightsalmon", "deepskyblue4","deepskyblue2"))+
  geom_pointrange(aes(ymin = estimate-se, ymax = estimate+se), alpha=0.8) + 
  scale_colour_manual(values = c("salmon3","lightsalmon", "deepskyblue4","deepskyblue2"))+
  geom_line(linetype = "solid") +
  labs(title="Seasonal Survival of BFS Wintering in Taiwan",x="Year", y = "Survival Estimates (Φ)")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(2006, 2020, by = 2)) +
  facet_grid(~ season, scales = "free", space = "free")+
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    legend.title=element_blank(),
    legend.position=c(0.9, 0.2),
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) 



# [p] ~W:r+a
# for plotting lcl & ucl
p.plotting$region_low =  as.numeric(as.factor(p.plotting$region)) - 0.1 
p.plotting$region_high =  as.numeric(as.factor(p.plotting$region)) + 0.1 

# changing label & legend title names
p.plotting$region[p.plotting$region=="J"] = "Japan"
p.plotting$region[p.plotting$region=="K"] = "South Korea"
p.plotting$region[p.plotting$region=="S"] = "Southern China"
p.plotting$region[p.plotting$region=="T"] = "Taiwan"

names(p.plotting)[names(p.plotting) == 'age.class'] <- 'Age classes'

# ggplot2
Detection =   
  ggplot(data = p.plotting[p.plotting$season=="W",], 
         aes(x = region, y = estimate, color=`Age classes`))+
  geom_pointrange(aes(ymin = estimate-se, ymax = estimate+se), alpha=0.8)+ 
  scale_color_manual(values = c("young (0–4)" = "deepskyblue2", "old (4+)" = "deepskyblue4")) +
  geom_segment(aes(y = lcl, yend = lcl, x = region_low, xend = region_high), linetype="solid", alpha=0.2, linewidth = 1) + 
  geom_segment(aes(y = ucl, yend = ucl, x = region_low, xend = region_high), linetype="solid", alpha=0.2, linewidth = 1) +
  geom_pointrange(aes(ymin = estimate-se, ymax = estimate+se), alpha=0.8)+ 
  labs(title="BFS Detection Probabilities in Winter",x="Wintering Regions", y = "Detection Probabilities (p)")+
  theme_classic()+
  theme(
    legend.position=c(0.9, 0.2),
    plot.title = element_text(hjust = 0.5, size = 16),
  )  