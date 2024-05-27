### Library ####
library(RMark)

### Load modelling results ####

## Read the marklist object with the results of 6889 models
model.results.all=readRDS("model.results.all.rds")

### Averaging models with weight > 0.01 ####
# extract model table
model.table.all = model.results.all$model.table
# Filter models with weight > 0.01
model.table.001 = model.table.all[model.table.all$weight > 0.01, ]
# Get the indices of the models that meet the weight criteria
model_indices = as.numeric(rownames(model.table.001))
# Get the indices of the models that need to be removed
indices_to_remove = setdiff(1:length(model.results.all), model_indices)
# Do not include the last element - the model table
indices_to_remove = head(indices_to_remove, -1)
# Get the subset marklist to include only the models with a weight > 0.01
marklist.001 = remove.mark(model.results.all, indices_to_remove)
# Perform model averaging on the subsetted models
est.mod.avg.001 = model.average(marklist.001, vcv=T)
# Save the model averaging results
saveRDS(est.mod.avg.001, file = "est.mod.avg.001.rds") 

### Organize the structure of the estimates table
# add a column with Phi or p so they can be plotted separately
BFS.ddl$Phi$parameter = "Phi"
BFS.ddl$p$parameter = "p"
# structure and rownames of the two ddl's are exactly the same, so they can be combined with rbind:
BFS.ddl.comb <- rbind(BFS.ddl$Phi, BFS.ddl$p)
# only keep relevant columns of BFS.ddl.comb:
BFS.ddl.comb = BFS.ddl.comb[,c(2,3,9,11,13,16,23)]
# create ageclass column based on Age:
BFS.ddl.comb$a02 = BFS.ddl.comb$Age
BFS.ddl.comb$a02[BFS.ddl.comb$a02>=2]='2+'
BFS.ddl.comb$a02[BFS.ddl.comb$a02<2]='0–2'
BFS.ddl.comb$a02 <- as.factor(BFS.ddl.comb$a02)

BFS.ddl.comb$a04 = BFS.ddl.comb$Age
BFS.ddl.comb$a04[BFS.ddl.comb$a04>=4]='4+'
BFS.ddl.comb$a04[BFS.ddl.comb$a04<4]='0–4'
BFS.ddl.comb$a04 <- as.factor(BFS.ddl.comb$a04)

# combine with model-averaged estimates:
est.mod.avg.001.comb <- cbind(est.mod.avg.001$estimates, BFS.ddl.comb)
# now only keep the unique estimates, after removing the par.index and model.index column (that I kept in for checking that the cbind combined the correct rows of both files), and the Age column:
Phi.001.uni <- unique(est.mod.avg.001.comb[est.mod.avg.001.comb$parameter=="Phi",c(2:5,7,9:11,14)])
p.001.uni <- unique(est.mod.avg.001.comb[est.mod.avg.001.comb$parameter=="p",c(2:5,7,9:13)])

### Plotting ####

# Define custom labels for the 'region' variable
region_labels = c("J" = "Japan", "S" = "South China", "T" = "Taiwan")
season_labels = c("W-S" = "Winter-Summer", "S-W" = "Summer-Winter", "S" = "Summer", "W" = "Winter")


## plot the Phi estimates
Phi.001.avg = ggplot(Phi.001.uni) +
  
  # the 0-4 age group 
  geom_line(data = subset(Phi.001.uni, a04=='0–4'), aes(x = Year - 0.3, y = estimate, color = a04)) +
  geom_ribbon(data = subset(Phi.001.uni, a04=='0–4'), aes(x = Year - 0.3, y = estimate, ymin = lcl, ymax = ucl, fill = a04), alpha=0.25) +
  geom_point(data = subset(Phi.001.uni, a04=='0–4'), aes(x = Year - 0.3, y = estimate, color = a04), size=1) +
  
  # the 4+ age group
  geom_line(data = subset(Phi.001.uni, a04=='4+'), aes(x = Year, y = estimate, color = a04)) +
  geom_ribbon(data = subset(Phi.001.uni, a04=='4+'), aes(x = Year, y = estimate, ymin = lcl, ymax = ucl, fill = a04), alpha=0.25) +
  geom_point(data = subset(Phi.001.uni, a04=='4+'), aes(x = Year, y = estimate, color = a04), size=1) +
  
  # Facet by region and season
  facet_grid(rows = vars(season), cols = vars(region), labeller = labeller(region = region_labels, season = season_labels)) +
  
  # Add themes and labels
  theme_bw() +
  labs(x = "Year", 
       y = "Survival Probability",
       color = "Age",
       title = "") +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11)  # Increase axis text size
  ) +
  scale_color_manual(values = c("0–4" = "#2FCCF5", "4+" = "#F58E2F")) +
  scale_fill_manual(values = c("0–4" = "#2FCCF5", "4+" = "#F58E2F")) +
  guides(fill = "none")  # Remove the fill legend

# save as svg
ggsave("Phi.001.avg.svg", plot = Phi.001.avg, width = 20, height = 15, units = "cm")


## plot the p estimates
p.001.avg = ggplot(p.001.uni) +
  
  # the 0-2 age group 
  geom_line(data = subset(p.001.uni, a02=='0–2'), aes(x = Year - 0.3, y = estimate, color = a02)) +
  geom_ribbon(data = subset(p.001.uni, a02=='0–2'), aes(x = Year - 0.3, y = estimate, ymin = lcl, ymax = ucl, fill = a02), alpha=0.25) +
  geom_point(data = subset(p.001.uni, a02=='0–2'), aes(x = Year - 0.3, y = estimate, color = a02), size=1) +
  
  # the 2+ age group
  geom_line(data = subset(p.001.uni, a02=='2+'), aes(x = Year, y = estimate, color = a02)) +
  geom_ribbon(data = subset(p.001.uni, a02=='2+'), aes(x = Year, y = estimate, ymin = lcl, ymax = ucl, fill = a02), alpha=0.25) +
  geom_point(data = subset(p.001.uni, a02=='2+'), aes(x = Year, y = estimate, color = a02), size=1) +
  
  # Facet by region and season
  facet_grid(rows = vars(season), cols = vars(region), labeller = labeller(region = region_labels, season = season_labels)) +
  
  # Add themes and labels
  theme_bw() +
  labs(x = "Year", 
       y = "Resighting Probability",
       color = "Age",
       title = "") +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11)  # Increase axis text size
  ) +
  scale_color_manual(values = c("0–2" = "#67F5DD", "2+" = "#F58F68")) +
  scale_fill_manual(values = c("0–2" = "#67F5DD", "2+" = "#F58F68")) +
  guides(fill = "none")  # Remove the fill legend


# save as svg
ggsave("p.001.avg.svg", plot = p.001.avg, width = 20, height = 15, units = "cm")

### Model averaging from a marklist ####

est.mod.avg.all = model.average(model.results.all, se=T) 

# add a column with Phi or p so they can be plotted separately
BFS.ddl$Phi$parameter = "Phi"
BFS.ddl$p$parameter = "p"

# structure and rownames of the two ddl's are exactly the same, so they can be combined with rbind:
BFS.ddl.comb <- rbind(BFS.ddl$Phi, BFS.ddl$p)
# only keep relevant columns of BFS.ddl.comb:
BFS.ddl.comb = BFS.ddl.comb[,c(2,3,9,11,13,16,23)]
# create ageclass column based on Age:
BFS.ddl.comb$a02 = BFS.ddl.comb$Age
BFS.ddl.comb$a02[BFS.ddl.comb$a02>=2]='2+'
BFS.ddl.comb$a02[BFS.ddl.comb$a02<2]='0–2'
BFS.ddl.comb$a02 <- as.factor(BFS.ddl.comb$a02)

BFS.ddl.comb$a04 = BFS.ddl.comb$Age
BFS.ddl.comb$a04[BFS.ddl.comb$a04>=4]='4+'
BFS.ddl.comb$a04[BFS.ddl.comb$a04<4]='0–4'
BFS.ddl.comb$a04 <- as.factor(BFS.ddl.comb$a04)


# combine with model-averaged estimates:
est.mod.avg.all.comb <- cbind(est.mod.avg.all, BFS.ddl.comb)
# now only keep the unique estimates, after removing the par.index and model.index column (that I kept in for checking that the cbind combined the correct rows of both files), and the Age column:
est.mod.avg.Phi.uni <- unique(est.mod.avg.all.comb[est.mod.avg.all.comb$parameter=="Phi",c(2:3,5,7:10,12)])
est.mod.avg.p.uni <- unique(est.mod.avg.all.comb[est.mod.avg.all.comb$parameter=="p",c(2:3,5,7:11)])

### Plotting ####

# Define custom labels for the 'region' variable
region_labels = c("J" = "Japan", "S" = "South China", "T" = "Taiwan")
season_labels = c("W-S" = "Winter-Summer", "S-W" = "Summer-Winter", "S" = "Summer", "W" = "Winter")


## plot the Phi estimates
Phi.avg = ggplot(est.mod.avg.Phi.uni) +
  
  # the 0-4 age group 
  geom_line(data = subset(est.mod.avg.Phi.uni, a04=='0–4'), aes(x = Year - 0.3, y = estimate, color = a04)) +
  geom_errorbar(data = subset(est.mod.avg.Phi.uni, a04=='0–4'), aes(x = Year - 0.3, y = estimate, ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se, color = a04), width = 0) +
  geom_point(data = subset(est.mod.avg.Phi.uni, a04=='0–4'), aes(x = Year - 0.3, y = estimate, color = a04)) +
  
  # the 4+ age group
  geom_line(data = subset(est.mod.avg.Phi.uni, a04=='4+'), aes(x = Year, y = estimate, color = a04)) +
  geom_errorbar(data = subset(est.mod.avg.Phi.uni, a04=='4+'), aes(x = Year, y = estimate, ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se, color = a04), width = 0) +
  geom_point(data = subset(est.mod.avg.Phi.uni, a04=='4+'), aes(x = Year, y = estimate, color = a04)) +
  
  # Facet by region and season
  facet_grid(rows = vars(season), cols = vars(region), labeller = labeller(region = region_labels, season = season_labels)) +
  
  # Add themes and labels
  theme_bw() +
  labs(x = "Year", 
       y = "Survival Probability",
       color = "Age",
       title = "") +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11)  # Increase axis text size
  ) +
  scale_color_manual(values = c("0–4" = "#56B4E9", "4+" = "#0072B2")) 

# save as svg
ggsave("Phi.avg.svg", plot = Phi.avg, width = 20, height = 15, units = "cm")


## plot the p estimates
p.avg = ggplot(est.mod.avg.p.uni) +
  
  # the 0-2 age group 
  geom_line(data = subset(est.mod.avg.p.uni, a02=='0–2'), aes(x = Year - 0.3, y = estimate, color = a02)) +
  geom_errorbar(data = subset(est.mod.avg.p.uni, a02=='0–2'), aes(x = Year - 0.3, y = estimate, ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se, color = a02), width = 0) +
  geom_point(data = subset(est.mod.avg.p.uni, a02=='0–2'), aes(x = Year - 0.3, y = estimate, color = a02)) +
  
  # the 2+ age group
  geom_line(data = subset(est.mod.avg.p.uni, a02=='2+'), aes(x = Year, y = estimate, color = a02)) +
  geom_errorbar(data = subset(est.mod.avg.p.uni, a02=='2+'), aes(x = Year, y = estimate, ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se, color = a02), width = 0) +
  geom_point(data = subset(est.mod.avg.p.uni, a02=='2+'), aes(x = Year, y = estimate, color = a02)) +
  
  # Facet by region and season
  facet_grid(rows = vars(season), cols = vars(region), labeller = labeller(region = region_labels, season = season_labels)) +
  
  # Add themes and labels
  theme_bw() +
  labs(x = "Year", 
       y = "Resighting Probability",
       color = "Age",
       title = "") +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11)  # Increase axis text size
  ) +
  scale_color_manual(values = c("0–2" = "#B2ABD2", "2+" = "#5E3C99"))
  
# save as svg
ggsave("p.avg.svg", plot = p.avg, width = 20, height = 15, units = "cm")



### Model averaging from a list structure for estimates ####
# Extract indices for first year survival(?) of birds wintering in J (group 1), S (group 2) and T (group 3)
Phi.indices=extract.indices(model.results.all[[5378]], #$Phi.r.p.dot
                            "Phi",df=data.frame(group=c(1,2,3),row=c(1,1,1),col=c(1,1,1)))


# Create a matrix for estimates
estimate=matrix(0,ncol=length(Phi.indices),
                nrow=nrow(model.results.all$model.table))

# Extract weights for models
weight=model.results.all$model.table$weight


# Create an empty list for vcv matrices
vcv=vector("list",length=nrow(model.results.all$model.table))

# Loop over each model in model.table for model.results.all
for (i in 1:nrow(model.results.all$model.table))
{
  # The actual model number is the row number for the model.table
  model.numbers= as.numeric(row.names(model.results.all$model.table))
  # For each model extract those real parameter values and their
  # vcv matrix and store them
  x=covariate.predictions(model.results.all[[model.numbers[i]]],
                          data=data.frame(index=Phi.indices))
  estimate[i,]=x$estimates$estimate
  vcv[[i]]=x$vcv
}

# Call model.average using the list structure which includes estimate, weight and vcv list in this case
mavg = model.average(list(estimate=estimate,weight=weight,vcv=vcv))
df = data.frame(region=c("Japan","South China","Taiwan"),estimate=mavg$estimate,se=mavg$se)


### Plotting ####
ggplot(df, aes(x = region, y = estimate, ymin=estimate-1.95*se, ymax=estimate+1.95*se)) +
  geom_errorbar(linewidth = 1, width = 0) +
  geom_point(size = 4) +
  theme_bw() +
  labs(x = "Region", 
       y = "Survival Probability", 
       title = "") +
  theme(
    panel.grid = element_blank(),  # Remove all gridlines
    strip.background = element_rect(colour = "black", fill = NA),  # Black lines for facet wrap
    text = element_text(size = 12),  # Base text size for the entire plot
    axis.title = element_text(size = 14),  # Increase axis title text size
    axis.text = element_text(size = 11),  # Increase axis text size
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.5, 1))
