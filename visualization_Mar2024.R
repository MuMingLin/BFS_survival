### Library ####
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

### Clean up input files ####
## read file of real estimates of the top model Phi.s.p.sa_sry
Phi.s_r.p.sa_sry.df = read.csv("Phi.s_r.p.sa_sry.real.csv")

Phi.s_r.p.sa_sry.df = Phi.s_r.p.sa_sry.df[,1:5]

## split X column into five
Phi.s_r.p.sa_sry.df[c('par', 'group', 'cohort', 'age', 'time')] <- stringr::str_split_fixed(Phi.s_r.p.sa_sry.df$X, ' ', 5)

## extract information needed
real.df <- Phi.s_r.p.sa_sry.df %>%
  # Extract year from time and convert to numeric for rounding
  mutate(Year = as.numeric(str_extract(time, "\\d+")),
         # Determine season based on whether time has ".5"
         Season = ifelse(str_detect(time, "\\.5"), "Winter", "Summer"),
         # Adjust Season based on 'par' value
         Season = case_when(
           par == "Phi" & Season == "Summer" ~ "Summer–Winter",
           par == "Phi" & Season == "Winter" ~ "Winter–Summer",
           TRUE ~ Season
         ),
         # Determine Age category based on 'age' value
         Age = case_when(
           age %in% c("a1", "a1.5") ~ "0–2yo",
           TRUE ~ "2yo+"
         ),
         # Determine Region based on 'group' value
         Region = case_when(
           str_detect(group, "J") ~ "Japan",
           str_detect(group, "S") ~ "South China",
           str_detect(group, "T") ~ "Taiwan",
           TRUE ~ NA_character_
         ))

# Check the structure of the modified dataframe
str(real.df)


## Subsetting

# Phi
Phi_1.df = real.df[real.df$par == "Phi",]

# p
p_1.df = real.df[real.df$par == "p",]

### Plotting ####

## Phi

Phi_1 <- ggplot(Phi_1.df, aes(x = Season, y = estimate, ymin=lcl, ymax=ucl, group = Region)) +
  geom_errorbar(size = 1, width = 0, position = position_dodge(width = 0.3)) +
  geom_point(aes(color = Region),size = 4, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Japan" = "#942492", "Taiwan" = "#28AAA0", "South China" = "#FAAE40")) +
  theme_bw() +
  labs(x = "Season", 
       y = "Survival Probability", 
       title = "") +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11),  # Increase axis text size
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.6, 1))

  
# save as svg
ggsave("Phi_1.svg", plot = Phi_1, width = 20, height = 15, units = "cm")


## p

p_1 <- ggplot(p_1.df, aes(x = Year, y = estimate, color = Region, group = interaction(Age, Season, Region))) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Region), alpha = 0.1, color = NA, show.legend = FALSE) +  # Add shaded area for confidence interval
  geom_line() +
  geom_point(aes(shape = Age), size = 1.5) +  # Adjust the size to your preference
  scale_shape_manual(values = c("0–2yo" = 21, "2yo+" = 16)) +  # Set shapes for hollow (21) and solid (16) points
  scale_color_manual(values = c("Japan" = "#942492", "Taiwan" = "#28AAA0", "South China" = "#FAAE40")) +  # Custom colors for lines and points
  scale_fill_manual(values = c("Japan" = "#942492", "Taiwan" = "#28AAA0", "South China" = "#FAAE40")) +  # Custom colors for ribbon fills
  facet_wrap(~Season, ncol = 1, scales = "free_y", strip.position = "top") +  # Separated facets for each season
  theme_bw() +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    panel.spacing = unit(0.5, "lines"),  # Increase space between facets
    legend.position = c(0.5, 0.92),
    legend.direction = "horizontal",
    legend.box = "horizontal", # This ensures the legend keys are in a single row
    legend.box.spacing = unit(1.5, "cm"),  # Adjust the space between legends
    legend.background = element_blank(),
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11),  # Increase axis text size
    legend.title = element_text(size = 12),  # Increase legend title text size
    legend.text = element_text(size = 11, color = "grey20"),
    strip.text = element_text(size = 14), # Increase legend text size
  ) +
  labs(x = "Year", 
       y = "Resighting Probability", 
       color = "Region",
       title = "") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2))  # Add more x-axis labels


# Save the plot
ggsave("p_1.svg", plot = p_1, width = 20, height = 15, units = "cm")



### Model averaging estimates ####

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

