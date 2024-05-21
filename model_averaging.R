### Library ####
library(RMark)

### Load modelling results ####

## Read the marklist object with the results of 6889 models
model.results.all=readRDS("model.results.all.rds")


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
