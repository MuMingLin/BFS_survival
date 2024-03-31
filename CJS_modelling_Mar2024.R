### Library ####
library(RMark)
library(dplyr)
library(tidyverse)
library(stringr)
library(R2ucare)
library(parallel) # for parallel processing
library(MuMIn) # dredge
library(msm) # for Delta method

### Data Import/Transform ####

# import BFS data frame
BFS = read.csv("merged5.C_28Mar.csv", colClasses = c("character", "character", "factor", "numeric"))

colnames(BFS) = c("birdID", "ch", "region", "age")

str(BFS)

# age group
BFS$agegroup = cut(BFS$age, breaks = c(0, 1, 2, 3, 4, Inf),
                   labels = c("0-1", "1-2", "2-3", "3-4", "4+"))


### Process data ####
# Nov 2006–Aug 2023 (34 sampling occasions, 2 sampling occasions per year)
BFS.process = process.data(BFS,
                           model="CJS",
                           begin.time=2006.5, # starts in 2006 winter
                           time.intervals=rep(0.5,33), # 34 sampling occasions, 33 intervals
                           groups=c("region", "agegroup"),
                           age.var = 2,
                           initial.ages = c(0.5, 1.5, 2.5, 3.5, 4.5)) # starts in winter

names(BFS.process)


### Design data list #### 
# create design data frame
BFS.ddl=make.design.data(BFS.process)

# check data structure
str(BFS.ddl$Phi)
str(BFS.ddl$p)


## ddl - Year 
# add Year column (time column is not integer, as time.intervals=0.5)
BFS.ddl$Phi$Year = as.integer(as.numeric(as.character(BFS.ddl$Phi$time)))
BFS.ddl$p$Year = as.integer(as.numeric(as.character(BFS.ddl$p$time)))

## ddl - year
# add year column (factor)
BFS.ddl$Phi$year = as.factor(BFS.ddl$Phi$Year)
BFS.ddl$p$year = as.factor(BFS.ddl$p$Year)

## ddl - season 
# add season column
View(BFS.ddl$Phi)
View(BFS.ddl$p)
# when 'time' is interger (time - year = 0), specify 'season' as "S-W" (summer to winter)
BFS.ddl$Phi$time.minus.year = as.numeric(as.character(BFS.ddl$Phi$time)) - 
  as.numeric(as.character(BFS.ddl$Phi$year))

BFS.ddl$Phi <- BFS.ddl$Phi %>% 
  mutate(season = case_when(time.minus.year == 0.0 ~ "S-W",
                            time.minus.year == 0.5 ~ "W-S"))

# when 'time' is interger (time - year = 0), specify 'season' as "S" (summer)
BFS.ddl$p$time.minus.year = as.numeric(as.character(BFS.ddl$p$time)) - 
  as.numeric(as.character(BFS.ddl$p$year))

BFS.ddl$p <- BFS.ddl$p %>% 
  mutate(season = case_when(time.minus.year == 0.0 ~ "S", 
                            time.minus.year == 0.5 ~ "W")) 

BFS.ddl$Phi$season = as.factor(BFS.ddl$Phi$season)
BFS.ddl$p$season = as.factor(BFS.ddl$p$season)


## ddl - age classes
# Phi
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="Phi", type="age", bins=c(0,0.9,30),name="ageclass01",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="Phi", type="age", bins=c(0,1.9,30),name="ageclass02",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="Phi", type="age", bins=c(0,2.9,30),name="ageclass03",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="Phi", type="age", bins=c(0,3.9,30),name="ageclass04",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="Phi", type="age", bins=c(0,0.9,3.9,30),name="ageclass014",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="Phi", type="age", bins=c(0,0.9,1.9,2.9,3.9,30),name="ageclass01234",replace=TRUE)

# p
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="p", type="age", bins=c(0,0.9,30),name="ageclass01",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="p", type="age", bins=c(0,1.9,30),name="ageclass02",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="p", type="age", bins=c(0,2.9,30),name="ageclass03",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="p", type="age", bins=c(0,3.9,30),name="ageclass04",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="p", type="age", bins=c(0,0.9,3.9,30),name="ageclass014",replace=TRUE)
BFS.ddl=add.design.data(BFS.process, BFS.ddl,
                        parameter="p", type="age", bins=c(0,0.9,1.9,2.9,3.9,30),name="ageclass01234",replace=TRUE)

## ddl - dummy columns [run if needed]

# add dummy columns of seasons
BFS.ddl$Phi$S_W <- 0
BFS.ddl$Phi$S_W[BFS.ddl$Phi$season=='S-W'] <- 1 
BFS.ddl$Phi$W_S <- 1 - BFS.ddl$Phi$S_W

BFS.ddl$p$S <- 0
BFS.ddl$p$S[BFS.ddl$p$season=='S'] <- 1 
BFS.ddl$p$W <- 1 - BFS.ddl$p$S


# add dummy column "TW" & "O"(others)
BFS.ddl$Phi$TW <- 0
BFS.ddl$Phi$TW[BFS.ddl$Phi$region=='T'] <- 1
BFS.ddl$Phi$O <- 1 - BFS.ddl$Phi$TW


BFS.ddl$p$TW <- 0
BFS.ddl$p$TW[BFS.ddl$p$region=='T'] <- 1
BFS.ddl$p$O <- 1 - BFS.ddl$p$TW

# add dummy column of age classes
BFS.ddl$Phi$Y04 <- 0
BFS.ddl$Phi$Y04[BFS.ddl$Phi$ageclass04=='[0,3.9]'] <- 1
BFS.ddl$Phi$A04 <- 1 - BFS.ddl$Phi$Y04


BFS.ddl$p$TW <- 0
BFS.ddl$p$TW[BFS.ddl$p$region=='T'] <- 1
BFS.ddl$p$O <- 1 - BFS.ddl$p$TW

### GoF test ####
## Custom GoF Function
# Runs RELEASE for goodness of fit test
release.gof(BFS.process) 

# change ch into cr:
cr <- matrix(ncol=nchar(BFS$ch[1]), nrow=dim(BFS)[1])
for (i in 1:dim(BFS)[1]) cr[i,] <- as.numeric(strsplit(BFS$ch[i], "")[[1]])
region = BFS$region

# conduct TEST 3.SR separately to 3 wintering regions (there's no 'K' birds anymore)
test3sr_T <- test3sr(cr[region=="T",], freq=rep(1,dim(cr[region=="T",])[1]))
test3sr_J <- test3sr(cr[region=="J",], freq=rep(1,dim(cr[region=="J",])[1]))
test3sr_S <- test3sr(cr[region=="S",], freq=rep(1,dim(cr[region=="S",])[1]))

test3sr.comb <- rbind(test3sr_T$test3sr, test3sr_J$test3sr, test3sr_S$test3sr)
rownames(test3sr.comb) = c('T','J','S')
test3sr.sum <- colSums(test3sr.comb[,1:2])
cbind(X2=test3sr.sum[1], df=test3sr.sum[2], chat=test3sr.sum[1]/test3sr.sum[2]) # c-hat=0.829

test3sm_T <- test3sm(cr[region=="T",], freq=rep(1,dim(cr[region=="T",])[1])) # p<0.05
test3sm_J <- test3sm(cr[region=="J",], freq=rep(1,dim(cr[region=="J",])[1]))
test3sm_S <- test3sm(cr[region=="S",], freq=rep(1,dim(cr[region=="S",])[1])) # p<0.05

test3sm.comb <- rbind(test3sm_T$test3sm, test3sm_J$test3sm, test3sm_S$test3sm)
rownames(test3sm.comb) = c('T','J','S')
test3sm.sum <- colSums(test3sm.comb[,1:2])
cbind(X2=test3sm.sum[1], df=test3sm.sum[2], chat=test3sm.sum[1]/test3sm.sum[2]) # c-hat=2.431595

test2ct_T <- test2ct(cr[region=="T",], freq=rep(1,dim(cr[region=="T",])[1])) 
test2ct_J <- test2ct(cr[region=="J",], freq=rep(1,dim(cr[region=="J",])[1])) 
test2ct_S <- test2ct(cr[region=="S",], freq=rep(1,dim(cr[region=="S",])[1])) 

test2ct.comb <- rbind(test2ct_T$test2ct, test2ct_J$test2ct, test2ct_S$test2ct)
rownames(test2ct.comb) = c('T','J','S')
test2ct.sum <- colSums(test2ct.comb[,1:2])
cbind(X2=test2ct.sum[1], df=test2ct.sum[2], chat=test2ct.sum[1]/test2ct.sum[2]) # chat=0.947378

test2cl_T <- test2cl(cr[region=="T",], freq=rep(1,dim(cr[region=="T",])[1])) # p<0.05
test2cl_J <- test2cl(cr[region=="J",], freq=rep(1,dim(cr[region=="J",])[1]))
test2cl_S <- test2cl(cr[region=="S",], freq=rep(1,dim(cr[region=="S",])[1]))

test2cl.comb <- rbind(test2cl_T$test2cl, test2cl_J$test2cl, test2cl_S$test2cl)
rownames(test2cl.comb) = c('T','J','S')
test2cl.sum <- colSums(test2cl.comb[,1:2])
cbind(X2=test2cl.sum[1], df=test2cl.sum[2], chat=test2cl.sum[1]/test2cl.sum[2]) # chat=2.229238

# all tests combined:
tests.combined <- rbind(test3sr.sum, test3sm.sum, test2ct.sum, test2cl.sum)
tests.combined.sum = colSums(tests.combined)
cbind(X2=tests.combined.sum[1], df=tests.combined.sum[2], chat=tests.combined.sum[1]/tests.combined.sum[2]) # c-hat=1.540507


chat=tests.combined.sum[1]/tests.combined.sum[2] # can be used in model selection and variance calculations


# Filter objects that start with "Phi." or "p."
test_objects <- ls()[grep("^test.", ls())]


# Remove the filtered objects from the workspace
rm(list = test_objects)


# Run the GOF model
GOF.model <- mark(data=BFS.process, ddl=BFS.ddl, model.parameters=list(Phi=list(formula=~season*time*region), p=list(formula=~season*time*region)), 
                  invisible=F, delete=T, adjust=T)


### Specify formulas ####
## Step 1: Determine age structure - different age structure for Phi and p ####

## Specify formulas: Full parameterization

# Full parameterization: Phi(~s*a*r+s*r*t)p(~s*a*r+s*r*t)
# [Phi] ~ . with different age classes

Phi.sxa01xrxt = list(formula = ~ season*ageclass01*region+season*region*Year) 
Phi.sxa02xrxt = list(formula = ~ season*ageclass02*region+season*region*Year) 
Phi.sxa03xrxt = list(formula = ~ season*ageclass03*region+season*region*Year) 
Phi.sxa04xrxt = list(formula = ~ season*ageclass04*region+season*region*Year) 
Phi.sxa014xrxt = list(formula = ~ season*ageclass014*region+season*region*Year) 
Phi.sxa01234xrxt = list(formula = ~ season*ageclass01234*region+season*region*Year) 


# [p] ~ . with different age classes
p.sxa01xrxt = list(formula = ~ season*ageclass01*region+season*region*Year) 
p.sxa02xrxt = list(formula = ~ season*ageclass02*region+season*region*Year) 
p.sxa03xrxt = list(formula = ~ season*ageclass03*region+season*region*Year) 
p.sxa04xrxt = list(formula = ~ season*ageclass04*region+season*region*Year) 
p.sxa014xrxt = list(formula = ~ season*ageclass014*region+season*region*Year) 
p.sxa01234xrxt = list(formula = ~ season*ageclass01234*region+season*region*Year) 



## Run models 
# TL: you could use the following function to get all the combinations of Phi and p parameterizations (as you may not necessarily expect the same age structure for Phi and p):
age.cml = create.model.list("CJS")
age.model.results = mark.wrapper.parallel(model.list=age.cml,data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,parallel = TRUE,cpus = 14)

# if invisible = TRUE, window for running MARK is hidden
# threads: number of cpus to use with mark.exe
# if delete = TRUE, the output ﬁles are deleted after the results are extracted

# check the results, while adding the GOF model add adjusting the AIC for overdispersion (now called QAICc instead of AICc)
age.model.results.adj = adjust.chat(chat, merge.mark(age.model.results, GOF.model))
# to also have the SE and CI adjusted for overdispersion, chat should be included in the mark.wrapper function, which I added below in the model selection of the 'interesting' models.


# export model selection table as csv file
write.csv(age.model.results.adj[["model.table"]], "age.model.results.adj.7Feb24.csv", row.names = FALSE)

### Remove all parameterizations of Phi and p with the different age structures, and continue with a04 for Phi and a02 for p. 

# Filter objects that start with "Phi." or "p."
Phi_objects <- ls()[grep("^Phi.", ls())]
p_objects <- ls()[grep("^p.", ls())]

# Remove the filtered objects from the workspace
rm(list = c(Phi_objects, p_objects))


## Step 2: Specify Phi & p formulas  ####

# Create formula list

# Full parameterization: Phi(~s*a04*r+s*r*t)p(~s*a02*r+s*r*t)

# use dredge() to create all possible conbinations of terms
fm = lm(O ~ season*ageclass04*region+season*region*time, BFS.ddl$Phi, na.action = na.fail) #create a fake full model #use categorical 'time' here because it's easier to convert, remember to change it back to 'Year' later
dd = dredge(fm)
df = dd[,2:12]


# Function to convert a row to formula
row_to_formula <- function(row) {
  variables <- names(row[!is.na(row) & row == '+'])
  if (length(variables) > 0) {
    formula_str <- paste(variables, collapse = '+')
    formula <- as.formula(paste("~", formula_str))
    return(list(formula = formula))
  } else {
    return(NULL)
  }
}

# Apply the function to each row
fm.list = apply(df, 1, row_to_formula)

fm.list_filtered = fm.list[sapply(fm.list, function(x) !is.null(x))]

fm.df = data.frame(matrix(unlist(fm.list_filtered), nrow=length(fm.list_filtered), byrow=TRUE),stringsAsFactors=FALSE)

colnames(fm.df)='formula'

# remember to change 'time' back to 'Year'
fm.df$formula = gsub("time", "Year", fm.df$formula)


# formatting
fm.df$formula.2 = fm.df$formula
fm.df$formula.2 = gsub('season','s',fm.df$formula.2)
fm.df$formula.2 = gsub('ageclass04','a',fm.df$formula.2)
fm.df$formula.2 = gsub('region','r',fm.df$formula.2)
fm.df$formula.2 = gsub('Year','Y',fm.df$formula.2)
fm.df$formula.2 = gsub('\\:','',fm.df$formula.2)
fm.df$formula.2 = gsub('\\~','',fm.df$formula.2)
fm.df$formula.2 = gsub(' \\+ ','_',fm.df$formula.2)

fm.df$Phi = paste("Phi.", fm.df$formula.2, " = list(formula = ", fm.df$formula, ")", sep="")
fm.df$p = paste("p.", fm.df$formula.2, " = list(formula = ", fm.df$formula, ")", sep="")
# a02 is best supoorted for p
fm.df$p = gsub("ageclass04", "ageclass02", fm.df$p)

## Specify Phi formulas
# print the formulas
cat(fm.df$Phi, sep = "\n")
# copy the result and paste here
Phi.a_r_Y_rY = list(formula = ~ageclass04 + region + Year + region:Year)
Phi.a_r_s_Y_rs_rY = list(formula = ~ageclass04 + region + season + Year + region:season + region:Year)
Phi.a_r_s_Y_rs_rY_sY = list(formula = ~ageclass04 + region + season + Year + region:season + region:Year + season:Year)
Phi.a_r_s_Y_rs_rY_sY_rsY = list(formula = ~ageclass04 + region + season + Year + region:season + region:Year + season:Year + region:season:Year)
Phi.a_r_s_Y_rY = list(formula = ~ageclass04 + region + season + Year + region:Year)
Phi.a_r_s_Y_rY_sY = list(formula = ~ageclass04 + region + season + Year + region:Year + season:Year)
Phi.a_r_s_Y_as_rs_rY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:season + region:Year)
Phi.a_r_s_Y_as_rs_rY_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:season + region:Year + season:Year)
Phi.a_r_s_Y_as_rs_rY_sY_rsY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:season + region:Year + season:Year + region:season:Year)
Phi.a_r_s_Y_as_rY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:Year)
Phi.a_r_s_Y_as_rY_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:Year + season:Year)
Phi.a_r_Y_ar_rY = list(formula = ~ageclass04 + region + Year + ageclass04:region + region:Year)
Phi.a_r_s_Y_ar_rs_rY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:season + region:Year)
Phi.a_r_s_Y_ar_rs_rY_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:season + region:Year + season:Year)
Phi.a_r_s_Y_ar_rs_rY_sY_rsY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:season + region:Year + season:Year + region:season:Year)
Phi.a_r_s_Y_ar_rY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:Year)
Phi.a_r_s_Y_ar_rY_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:Year + season:Year)
Phi.a_r_s_Y_ar_as_rs_rY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + region:Year)
Phi.a_r_s_Y_ar_as_rs_rY_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + region:Year + season:Year)
Phi.a_r_s_Y_ar_as_rs_rY_sY_rsY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + region:Year + season:Year + region:season:Year)
Phi.a_r_s_Y_ar_as_rY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:Year)
Phi.a_r_s_Y_ar_as_rY_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:Year + season:Year)
Phi.a_r_s_Y_ar_as_rs_rY_ars = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + region:Year + ageclass04:region:season)
Phi.a_r_s_Y_ar_as_rs_rY_sY_ars = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + region:Year + season:Year + ageclass04:region:season)
Phi.a_r_s_Y_ar_as_rs_rY_sY_ars_rsY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + region:Year + season:Year + ageclass04:region:season + region:season:Year)
Phi.a_r_s_Y_ar_as_rs_ars = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + ageclass04:region:season)
Phi.a_r_s_Y_ar_as_rs_sY_ars = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + season:Year + ageclass04:region:season)
Phi.a_r_Y_ar = list(formula = ~ageclass04 + region + Year + ageclass04:region)
Phi.a_r_s_Y_ar = list(formula = ~ageclass04 + region + season + Year + ageclass04:region)
Phi.a_r_s_Y_ar_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + season:Year)
Phi.a_r_s_Y_ar_as = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season)
Phi.a_r_s_Y_ar_as_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + season:Year)
Phi.a_r_s_Y_ar_rs = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:season)
Phi.a_r_s_Y_ar_rs_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + region:season + season:Year)
Phi.a_r_s_Y_ar_as_rs = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season)
Phi.a_r_s_Y_ar_as_rs_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:region + ageclass04:season + region:season + season:Year)
Phi.a_r_Y = list(formula = ~ageclass04 + region + Year)
Phi.a_r_s_Y = list(formula = ~ageclass04 + region + season + Year)
Phi.a_r_s_Y_sY = list(formula = ~ageclass04 + region + season + Year + season:Year)
Phi.a_r_s_Y_as = list(formula = ~ageclass04 + region + season + Year + ageclass04:season)
Phi.a_r_s_Y_as_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + season:Year)
Phi.a_r_s_Y_rs = list(formula = ~ageclass04 + region + season + Year + region:season)
Phi.a_r_s_Y_rs_sY = list(formula = ~ageclass04 + region + season + Year + region:season + season:Year)
Phi.a_r_s_Y_as_rs = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:season)
Phi.a_r_s_Y_as_rs_sY = list(formula = ~ageclass04 + region + season + Year + ageclass04:season + region:season + season:Year)
Phi.a_r_s_ar_as_rs_ars = list(formula = ~ageclass04 + region + season + ageclass04:region + ageclass04:season + region:season + ageclass04:region:season)
Phi.a_r_s_ar_as = list(formula = ~ageclass04 + region + season + ageclass04:region + ageclass04:season)
Phi.a_r_s_ar_as_rs = list(formula = ~ageclass04 + region + season + ageclass04:region + ageclass04:season + region:season)
Phi.a_r_ar = list(formula = ~ageclass04 + region + ageclass04:region)
Phi.a_r_s_ar = list(formula = ~ageclass04 + region + season + ageclass04:region)
Phi.a_r_s_ar_rs = list(formula = ~ageclass04 + region + season + ageclass04:region + region:season)
Phi.a_r_s_as = list(formula = ~ageclass04 + region + season + ageclass04:season)
Phi.a_r_s_as_rs = list(formula = ~ageclass04 + region + season + ageclass04:season + region:season)
Phi.a_r = list(formula = ~ageclass04 + region)
Phi.a_r_s = list(formula = ~ageclass04 + region + season)
Phi.a_r_s_rs = list(formula = ~ageclass04 + region + season + region:season)
Phi.r_s_Y_rs_rY = list(formula = ~region + season + Year + region:season + region:Year)
Phi.r_s_Y_rs_rY_sY = list(formula = ~region + season + Year + region:season + region:Year + season:Year)
Phi.r_s_Y_rs_rY_sY_rsY = list(formula = ~region + season + Year + region:season + region:Year + season:Year + region:season:Year)
Phi.r_Y_rY = list(formula = ~region + Year + region:Year)
Phi.r_s_Y_rY = list(formula = ~region + season + Year + region:Year)
Phi.r_s_Y_rY_sY = list(formula = ~region + season + Year + region:Year + season:Year)
Phi.r_s_Y_rs = list(formula = ~region + season + Year + region:season)
Phi.r_s_Y_rs_sY = list(formula = ~region + season + Year + region:season + season:Year)
Phi.r_s_Y = list(formula = ~region + season + Year)
Phi.r_s_Y_sY = list(formula = ~region + season + Year + season:Year)
Phi.r_Y = list(formula = ~region + Year)
Phi.r_s_rs = list(formula = ~region + season + region:season)
Phi.r_s = list(formula = ~region + season)
Phi.r = list(formula = ~region)
Phi.a = list(formula = ~ageclass04)
Phi.a_s = list(formula = ~ageclass04 + season)
Phi.a_s_as = list(formula = ~ageclass04 + season + ageclass04:season)
Phi.s = list(formula = ~season)
Phi.a_Y = list(formula = ~ageclass04 + Year)
Phi.a_s_Y = list(formula = ~ageclass04 + season + Year)
Phi.a_s_Y_sY = list(formula = ~ageclass04 + season + Year + season:Year)
Phi.a_s_Y_as = list(formula = ~ageclass04 + season + Year + ageclass04:season)
Phi.a_s_Y_as_sY = list(formula = ~ageclass04 + season + Year + ageclass04:season + season:Year)
Phi.s_Y = list(formula = ~season + Year)
Phi.s_Y_sY = list(formula = ~season + Year + season:Year)
Phi.Y = list(formula = ~Year)
Phi.dot = list(formula=~1) # remember to add Phi.dot


## Specify p formulas
# print the formulas
cat(fm.df$p, sep = "\n")
# copy the result and paste here
p.a_r_Y_rY = list(formula = ~ageclass02 + region + Year + region:Year)
p.a_r_s_Y_rs_rY = list(formula = ~ageclass02 + region + season + Year + region:season + region:Year)
p.a_r_s_Y_rs_rY_sY = list(formula = ~ageclass02 + region + season + Year + region:season + region:Year + season:Year)
p.a_r_s_Y_rs_rY_sY_rsY = list(formula = ~ageclass02 + region + season + Year + region:season + region:Year + season:Year + region:season:Year)
p.a_r_s_Y_rY = list(formula = ~ageclass02 + region + season + Year + region:Year)
p.a_r_s_Y_rY_sY = list(formula = ~ageclass02 + region + season + Year + region:Year + season:Year)
p.a_r_s_Y_as_rs_rY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:season + region:Year)
p.a_r_s_Y_as_rs_rY_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:season + region:Year + season:Year)
p.a_r_s_Y_as_rs_rY_sY_rsY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:season + region:Year + season:Year + region:season:Year)
p.a_r_s_Y_as_rY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:Year)
p.a_r_s_Y_as_rY_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:Year + season:Year)
p.a_r_Y_ar_rY = list(formula = ~ageclass02 + region + Year + ageclass02:region + region:Year)
p.a_r_s_Y_ar_rs_rY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:season + region:Year)
p.a_r_s_Y_ar_rs_rY_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:season + region:Year + season:Year)
p.a_r_s_Y_ar_rs_rY_sY_rsY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:season + region:Year + season:Year + region:season:Year)
p.a_r_s_Y_ar_rY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:Year)
p.a_r_s_Y_ar_rY_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:Year + season:Year)
p.a_r_s_Y_ar_as_rs_rY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + region:Year)
p.a_r_s_Y_ar_as_rs_rY_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + region:Year + season:Year)
p.a_r_s_Y_ar_as_rs_rY_sY_rsY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + region:Year + season:Year + region:season:Year)
p.a_r_s_Y_ar_as_rY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:Year)
p.a_r_s_Y_ar_as_rY_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:Year + season:Year)
p.a_r_s_Y_ar_as_rs_rY_ars = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + region:Year + ageclass02:region:season)
p.a_r_s_Y_ar_as_rs_rY_sY_ars = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + region:Year + season:Year + ageclass02:region:season)
p.a_r_s_Y_ar_as_rs_rY_sY_ars_rsY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + region:Year + season:Year + ageclass02:region:season + region:season:Year)
p.a_r_s_Y_ar_as_rs_ars = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + ageclass02:region:season)
p.a_r_s_Y_ar_as_rs_sY_ars = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + season:Year + ageclass02:region:season)
p.a_r_Y_ar = list(formula = ~ageclass02 + region + Year + ageclass02:region)
p.a_r_s_Y_ar = list(formula = ~ageclass02 + region + season + Year + ageclass02:region)
p.a_r_s_Y_ar_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + season:Year)
p.a_r_s_Y_ar_as = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season)
p.a_r_s_Y_ar_as_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + season:Year)
p.a_r_s_Y_ar_rs = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:season)
p.a_r_s_Y_ar_rs_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + region:season + season:Year)
p.a_r_s_Y_ar_as_rs = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season)
p.a_r_s_Y_ar_as_rs_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:region + ageclass02:season + region:season + season:Year)
p.a_r_Y = list(formula = ~ageclass02 + region + Year)
p.a_r_s_Y = list(formula = ~ageclass02 + region + season + Year)
p.a_r_s_Y_sY = list(formula = ~ageclass02 + region + season + Year + season:Year)
p.a_r_s_Y_as = list(formula = ~ageclass02 + region + season + Year + ageclass02:season)
p.a_r_s_Y_as_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + season:Year)
p.a_r_s_Y_rs = list(formula = ~ageclass02 + region + season + Year + region:season)
p.a_r_s_Y_rs_sY = list(formula = ~ageclass02 + region + season + Year + region:season + season:Year)
p.a_r_s_Y_as_rs = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:season)
p.a_r_s_Y_as_rs_sY = list(formula = ~ageclass02 + region + season + Year + ageclass02:season + region:season + season:Year)
p.a_r_s_ar_as_rs_ars = list(formula = ~ageclass02 + region + season + ageclass02:region + ageclass02:season + region:season + ageclass02:region:season)
p.a_r_s_ar_as = list(formula = ~ageclass02 + region + season + ageclass02:region + ageclass02:season)
p.a_r_s_ar_as_rs = list(formula = ~ageclass02 + region + season + ageclass02:region + ageclass02:season + region:season)
p.a_r_ar = list(formula = ~ageclass02 + region + ageclass02:region)
p.a_r_s_ar = list(formula = ~ageclass02 + region + season + ageclass02:region)
p.a_r_s_ar_rs = list(formula = ~ageclass02 + region + season + ageclass02:region + region:season)
p.a_r_s_as = list(formula = ~ageclass02 + region + season + ageclass02:season)
p.a_r_s_as_rs = list(formula = ~ageclass02 + region + season + ageclass02:season + region:season)
p.a_r = list(formula = ~ageclass02 + region)
p.a_r_s = list(formula = ~ageclass02 + region + season)
p.a_r_s_rs = list(formula = ~ageclass02 + region + season + region:season)
p.r_s_Y_rs_rY = list(formula = ~region + season + Year + region:season + region:Year)
p.r_s_Y_rs_rY_sY = list(formula = ~region + season + Year + region:season + region:Year + season:Year)
p.r_s_Y_rs_rY_sY_rsY = list(formula = ~region + season + Year + region:season + region:Year + season:Year + region:season:Year)
p.r_Y_rY = list(formula = ~region + Year + region:Year)
p.r_s_Y_rY = list(formula = ~region + season + Year + region:Year)
p.r_s_Y_rY_sY = list(formula = ~region + season + Year + region:Year + season:Year)
p.r_s_Y_rs = list(formula = ~region + season + Year + region:season)
p.r_s_Y_rs_sY = list(formula = ~region + season + Year + region:season + season:Year)
p.r_s_Y = list(formula = ~region + season + Year)
p.r_s_Y_sY = list(formula = ~region + season + Year + season:Year)
p.r_Y = list(formula = ~region + Year)
p.r_s_rs = list(formula = ~region + season + region:season)
p.r_s = list(formula = ~region + season)
p.r = list(formula = ~region)
p.a = list(formula = ~ageclass02)
p.a_s = list(formula = ~ageclass02 + season)
p.a_s_as = list(formula = ~ageclass02 + season + ageclass02:season)
p.s = list(formula = ~season)
p.a_Y = list(formula = ~ageclass02 + Year)
p.a_s_Y = list(formula = ~ageclass02 + season + Year)
p.a_s_Y_sY = list(formula = ~ageclass02 + season + Year + season:Year)
p.a_s_Y_as = list(formula = ~ageclass02 + season + Year + ageclass02:season)
p.a_s_Y_as_sY = list(formula = ~ageclass02 + season + Year + ageclass02:season + season:Year)
p.s_Y = list(formula = ~season + Year)
p.s_Y_sY = list(formula = ~season + Year + season:Year)
p.Y = list(formula = ~Year)
p.dot = list(formula=~1) # remember to add p.dot


# create model list
all.cml = create.model.list(c("CJS")) 


# free unused memory
gc()

# paste 83 lines of mark.wrapper.parallel codes
for (i in seq(1, 6807, 83)) {
  j = i + 82
  cat(
    paste("model.results.", i, "_", j, " = mark.wrapper(model.list=all.cml[", i, ":", j, ",],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)", sep="")
    , sep = "\n")
}

### Run models ####
# Running 6889 models easily causes my computer to crash, so I tried several methods to segment and save progress.

## Plan A: Run 83 models at a time and save workspace fequently ####
model.results.1_83 = mark.wrapper(model.list=all.cml[1:83,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.84_166 = mark.wrapper(model.list=all.cml[84:166,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.167_249 = mark.wrapper(model.list=all.cml[167:249,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.250_332 = mark.wrapper(model.list=all.cml[250:332,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.333_415 = mark.wrapper(model.list=all.cml[333:415,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.416_498 = mark.wrapper(model.list=all.cml[416:498,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.499_581 = mark.wrapper(model.list=all.cml[499:581,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.582_664 = mark.wrapper(model.list=all.cml[582:664,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.665_747 = mark.wrapper(model.list=all.cml[665:747,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.748_830 = mark.wrapper(model.list=all.cml[748:830,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.1.RData")
gc()

model.results.831_913 = mark.wrapper(model.list=all.cml[831:913,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.914_996 = mark.wrapper(model.list=all.cml[914:996,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.997_1079 = mark.wrapper(model.list=all.cml[997:1079,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1080_1162 = mark.wrapper(model.list=all.cml[1080:1162,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1163_1245 = mark.wrapper(model.list=all.cml[1163:1245,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1246_1328 = mark.wrapper(model.list=all.cml[1246:1328,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1329_1411 = mark.wrapper(model.list=all.cml[1329:1411,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1412_1494 = mark.wrapper(model.list=all.cml[1412:1494,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1495_1577 = mark.wrapper(model.list=all.cml[1495:1577,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1578_1660 = mark.wrapper(model.list=all.cml[1578:1660,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.2.RData")
gc()

model.results.1661_1743 = mark.wrapper(model.list=all.cml[1661:1743,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1744_1826 = mark.wrapper(model.list=all.cml[1744:1826,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1827_1909 = mark.wrapper(model.list=all.cml[1827:1909,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1910_1992 = mark.wrapper(model.list=all.cml[1910:1992,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.1993_2075 = mark.wrapper(model.list=all.cml[1993:2075,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2076_2158 = mark.wrapper(model.list=all.cml[2076:2158,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2159_2241 = mark.wrapper(model.list=all.cml[2159:2241,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2242_2324 = mark.wrapper(model.list=all.cml[2242:2324,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2325_2407 = mark.wrapper(model.list=all.cml[2325:2407,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2408_2490 = mark.wrapper(model.list=all.cml[2408:2490,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.3.RData")
gc()

model.results.2491_2573 = mark.wrapper(model.list=all.cml[2491:2573,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2574_2656 = mark.wrapper(model.list=all.cml[2574:2656,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2657_2739 = mark.wrapper(model.list=all.cml[2657:2739,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2740_2822 = mark.wrapper(model.list=all.cml[2740:2822,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2823_2905 = mark.wrapper(model.list=all.cml[2823:2905,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2906_2988 = mark.wrapper(model.list=all.cml[2906:2988,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.2989_3071 = mark.wrapper(model.list=all.cml[2989:3071,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3072_3154 = mark.wrapper(model.list=all.cml[3072:3154,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3155_3237 = mark.wrapper(model.list=all.cml[3155:3237,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3238_3320 = mark.wrapper(model.list=all.cml[3238:3320,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.4.RData")
gc()

model.results.3321_3403 = mark.wrapper(model.list=all.cml[3321:3403,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3404_3486 = mark.wrapper(model.list=all.cml[3404:3486,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3487_3569 = mark.wrapper(model.list=all.cml[3487:3569,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3570_3652 = mark.wrapper(model.list=all.cml[3570:3652,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3653_3735 = mark.wrapper(model.list=all.cml[3653:3735,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3736_3818 = mark.wrapper(model.list=all.cml[3736:3818,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3819_3901 = mark.wrapper(model.list=all.cml[3819:3901,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3902_3984 = mark.wrapper(model.list=all.cml[3902:3984,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.3985_4067 = mark.wrapper(model.list=all.cml[3985:4067,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4068_4150 = mark.wrapper(model.list=all.cml[4068:4150,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.5.RData")
gc()

model.results.4151_4233 = mark.wrapper(model.list=all.cml[4151:4233,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4234_4316 = mark.wrapper(model.list=all.cml[4234:4316,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4317_4399 = mark.wrapper(model.list=all.cml[4317:4399,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4400_4482 = mark.wrapper(model.list=all.cml[4400:4482,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4483_4565 = mark.wrapper(model.list=all.cml[4483:4565,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4566_4648 = mark.wrapper(model.list=all.cml[4566:4648,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4649_4731 = mark.wrapper(model.list=all.cml[4649:4731,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4732_4814 = mark.wrapper(model.list=all.cml[4732:4814,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4815_4897 = mark.wrapper(model.list=all.cml[4815:4897,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.4898_4980 = mark.wrapper(model.list=all.cml[4898:4980,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.6.RData")
gc()

model.results.4981_5063 = mark.wrapper(model.list=all.cml[4981:5063,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5064_5146 = mark.wrapper(model.list=all.cml[5064:5146,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5147_5229 = mark.wrapper(model.list=all.cml[5147:5229,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5230_5312 = mark.wrapper(model.list=all.cml[5230:5312,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5313_5395 = mark.wrapper(model.list=all.cml[5313:5395,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5396_5478 = mark.wrapper(model.list=all.cml[5396:5478,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5479_5561 = mark.wrapper(model.list=all.cml[5479:5561,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5562_5644 = mark.wrapper(model.list=all.cml[5562:5644,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5645_5727 = mark.wrapper(model.list=all.cml[5645:5727,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5728_5810 = mark.wrapper(model.list=all.cml[5728:5810,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.7.RData")
gc()

model.results.5811_5893 = mark.wrapper(model.list=all.cml[5811:5893,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5894_5976 = mark.wrapper(model.list=all.cml[5894:5976,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.5977_6059 = mark.wrapper(model.list=all.cml[5977:6059,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6060_6142 = mark.wrapper(model.list=all.cml[6060:6142,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6143_6225 = mark.wrapper(model.list=all.cml[6143:6225,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6226_6308 = mark.wrapper(model.list=all.cml[6226:6308,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6309_6391 = mark.wrapper(model.list=all.cml[6309:6391,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6392_6474 = mark.wrapper(model.list=all.cml[6392:6474,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6475_6557 = mark.wrapper(model.list=all.cml[6475:6557,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6558_6640 = mark.wrapper(model.list=all.cml[6558:6640,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.8.RData")
gc()

model.results.6641_6723 = mark.wrapper(model.list=all.cml[6641:6723,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6724_6806 = mark.wrapper(model.list=all.cml[6724:6806,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
model.results.6807_6889 = mark.wrapper(model.list=all.cml[6807:6889,],data=BFS.process,ddl=BFS.ddl,invisible=FALSE,delete=TRUE,adjust=T,chat=chat, threads=14)
save.image("7Feb24.9.RData")
gc()





## Plan B: Use a loop to wrap 83 models at a time, save the object and immediately remove it to free up the CPUs ####

# Loop to wrap model lists, save and remove the object each time

for (i in seq(1, 6807, 83)) {
  j = i + 82
  # Construct and evaluate the code
  eval(parse(text = paste0(
    "model.results.", i, "_", j, " <- mark.wrapper(",
    "model.list = all.cml[", i, ":", j, ",],",
    "data = BFS.process,",
    "ddl = BFS.ddl,",
    "invisible = FALSE,",
    "delete = TRUE,",
    "adjust = TRUE,",
    "chat = chat,",
    "threads = 14",
    ")"
  )))
  
  # Save the results
  eval(parse(text = paste0(
    "saveRDS(model.results.", i, "_", j, ", file = \"model.results.", i, "_", j, ".rds\")"
  )))
  
  # Remove the results from the environment
  eval(parse(text = paste0(
    "rm(model.results.", i, "_", j, ")"
  )))
}


# Restore the object (model.results.i_i+82 = readRDS(file = "model.results.i_i+82.rds")

for (i in seq(1, 6807, 83)) {
  j = i + 82
  # Construct and evaluate the code
  eval(parse(text = paste0(
    "model.results.", i, "_", j, " <- readRDS(file = \"model.results.", i, "_", j, ".rds\")"
  )))
}




### Model selection ####

## [DO NOT RUN] Plan A: Merging all at once can cause the computer to crash ####
# model.results.all = merge.mark(
#  model.results.1_83, model.results.84_166, model.results.167_249, model.results.250_332, model.results.333_415, model.results.416_498,
#  model.results.499_581, model.results.582_664, model.results.665_747, model.results.748_830, model.results.831_913, model.results.914_996,
#  model.results.997_1079, model.results.1080_1162, model.results.1163_1245, model.results.1246_1328, model.results.1329_1411, model.results.1412_1494,
#  model.results.1495_1577, model.results.1578_1660, model.results.1661_1743, model.results.1744_1826, model.results.1827_1909, model.results.1910_1992,
#  model.results.1993_2075, model.results.2076_2158, model.results.2159_2241, model.results.2242_2324, model.results.2325_2407, model.results.2408_2490,
#  model.results.2491_2573, model.results.2574_2656, model.results.2657_2739, model.results.2740_2822, model.results.2823_2905, model.results.2906_2988,
#  model.results.2989_3071, model.results.3072_3154, model.results.3155_3237, model.results.3238_3320, model.results.3321_3403, model.results.3404_3486,
#  model.results.3487_3569, model.results.3570_3652, model.results.3653_3735, model.results.3736_3818, model.results.3819_3901, model.results.3902_3984,
#  model.results.3985_4067, model.results.4068_4150, model.results.4151_4233, model.results.4234_4316, model.results.4317_4399, model.results.4400_4482,
#  model.results.4483_4565, model.results.4566_4648, model.results.4649_4731, model.results.4732_4814, model.results.4815_4897, model.results.4898_4980,
#  model.results.4981_5063, model.results.5064_5146, model.results.5147_5229, model.results.5230_5312, model.results.5313_5395, model.results.5396_5478,
#  model.results.5479_5561, model.results.5562_5644, model.results.5645_5727, model.results.5728_5810, model.results.5811_5893, model.results.5894_5976,
#  model.results.5977_6059, model.results.6060_6142, model.results.6143_6225, model.results.6226_6308, model.results.6309_6391, model.results.6392_6474,
#  model.results.6475_6557, model.results.6558_6640, model.results.6641_6723, model.results.6724_6806, model.results.6807_6889
# )


## Plan B: Merge model lists gradually ####

## [Optional] paste all the model lists' names
for (i in seq(1, 6807, 83)) {
  j = i + 82
  cat(
    paste("model.results.", i, "_", j, ", ", sep='')
  )
}

for (i in seq(1, 6807, 83)) {
  j = i + 82
  cat(
    paste("\"model.results.", i, "_", j, "\", ", sep='')
  )
}

# merge into medium lists then combine them

model.results.all.1 = merge.mark(
  model.results.1_83, model.results.84_166, model.results.167_249, model.results.250_332, model.results.333_415, model.results.416_498, 
  model.results.499_581, model.results.582_664, model.results.665_747, model.results.748_830, model.results.831_913, model.results.914_996
)
saveRDS(model.results.all.1, file = "model.results.all.1.rds")

model.results.all.2 = merge.mark(
  model.results.997_1079, model.results.1080_1162, model.results.1163_1245, model.results.1246_1328, model.results.1329_1411, model.results.1412_1494, 
  model.results.1495_1577, model.results.1578_1660, model.results.1661_1743, model.results.1744_1826, model.results.1827_1909, model.results.1910_1992
)
saveRDS(model.results.all.2, file = "model.results.all.2.rds")

model.results.all.3 = merge.mark(
  model.results.1993_2075, model.results.2076_2158, model.results.2159_2241, model.results.2242_2324, model.results.2325_2407, model.results.2408_2490, 
  model.results.2491_2573, model.results.2574_2656, model.results.2657_2739, model.results.2740_2822, model.results.2823_2905, model.results.2906_2988
)
saveRDS(model.results.all.3, file = "model.results.all.3.rds")

model.results.all.4 = merge.mark(
  model.results.2989_3071, model.results.3072_3154, model.results.3155_3237, model.results.3238_3320, model.results.3321_3403, model.results.3404_3486, 
  model.results.3487_3569, model.results.3570_3652, model.results.3653_3735, model.results.3736_3818, model.results.3819_3901, model.results.3902_3984 
)
saveRDS(model.results.all.4, file = "model.results.all.4.rds")

model.results.all.5 = merge.mark(
  model.results.3985_4067, model.results.4068_4150, model.results.4151_4233, model.results.4234_4316, model.results.4317_4399, model.results.4400_4482, 
  model.results.4483_4565, model.results.4566_4648, model.results.4649_4731, model.results.4732_4814, model.results.4815_4897, model.results.4898_4980 
)
saveRDS(model.results.all.5, file = "model.results.all.5.rds")

model.results.all.6 = merge.mark(
  model.results.4981_5063, model.results.5064_5146, model.results.5147_5229, model.results.5230_5312, model.results.5313_5395, model.results.5396_5478, 
  model.results.5479_5561, model.results.5562_5644, model.results.5645_5727, model.results.5728_5810, model.results.5811_5893, model.results.5894_5976
)
saveRDS(model.results.all.6, file = "model.results.all.6.rds")

model.results.all.7 = merge.mark(
  model.results.5977_6059, model.results.6060_6142, model.results.6143_6225, model.results.6226_6308, model.results.6309_6391, model.results.6392_6474, 
  model.results.6475_6557, model.results.6558_6640, model.results.6641_6723, model.results.6724_6806, model.results.6807_6889
)
saveRDS(model.results.all.7, file = "model.results.all.7.rds")

# remove all model lists to free up CPU usage
rm("model.results.1_83", "model.results.84_166", "model.results.167_249", "model.results.250_332", "model.results.333_415", "model.results.416_498", 
   "model.results.499_581", "model.results.582_664", "model.results.665_747", "model.results.748_830", "model.results.831_913", "model.results.914_996", 
   "model.results.997_1079", "model.results.1080_1162", "model.results.1163_1245", "model.results.1246_1328", "model.results.1329_1411", "model.results.1412_1494", 
   "model.results.1495_1577", "model.results.1578_1660", "model.results.1661_1743", "model.results.1744_1826", "model.results.1827_1909", "model.results.1910_1992", 
   "model.results.1993_2075", "model.results.2076_2158", "model.results.2159_2241", "model.results.2242_2324", "model.results.2325_2407", "model.results.2408_2490", 
   "model.results.2491_2573", "model.results.2574_2656", "model.results.2657_2739", "model.results.2740_2822", "model.results.2823_2905", "model.results.2906_2988", 
   "model.results.2989_3071", "model.results.3072_3154", "model.results.3155_3237", "model.results.3238_3320", "model.results.3321_3403", "model.results.3404_3486", 
   "model.results.3487_3569", "model.results.3570_3652", "model.results.3653_3735", "model.results.3736_3818", "model.results.3819_3901", "model.results.3902_3984", 
   "model.results.3985_4067", "model.results.4068_4150", "model.results.4151_4233", "model.results.4234_4316", "model.results.4317_4399", "model.results.4400_4482", 
   "model.results.4483_4565", "model.results.4566_4648", "model.results.4649_4731", "model.results.4732_4814", "model.results.4815_4897", "model.results.4898_4980", 
   "model.results.4981_5063", "model.results.5064_5146", "model.results.5147_5229", "model.results.5230_5312", "model.results.5313_5395", "model.results.5396_5478", 
   "model.results.5479_5561", "model.results.5562_5644", "model.results.5645_5727", "model.results.5728_5810", "model.results.5811_5893", "model.results.5894_5976", 
   "model.results.5977_6059", "model.results.6060_6142", "model.results.6143_6225", "model.results.6226_6308", "model.results.6309_6391", "model.results.6392_6474", 
   "model.results.6475_6557", "model.results.6558_6640", "model.results.6641_6723", "model.results.6724_6806", "model.results.6807_6889")

# merge all the medium lists into the final big list
model.results.all = merge.mark(model.results.all.1, model.results.all.2, model.results.all.3, model.results.all.4, model.results.all.5, model.results.all.6, model.results.all.7)
saveRDS(model.results.all, file = "model.results.all.rds")

# save model selection result
write.csv(model.results.all[["model.table"]], "model.results.all.Feb2024.csv", row.names = FALSE)

## Remove all parameterizations of Phi and p
# Filter objects that start with "Phi." or "p."
Phi_objects <- ls()[grep("^Phi.", ls())]
p_objects <- ls()[grep("^p.", ls())]

# Remove the filtered objects from the workspace
rm(list = c(Phi_objects, p_objects))

### Model averaging #########
# Phi
all.avg.Phi=model.average(model.results.all,"Phi",vcv=TRUE)
saveRDS(all.avg.Phi, file = "all.avg.Phi.rds")
write.csv(all.avg.Phi[["estimates"]], "all.avg.Phi.Feb2024.csv", row.names = FALSE)
# p
all.avg.p=model.average(model.results.all,"p",vcv=TRUE)
saveRDS(all.avg.p, file = "all.avg.p.rds")
write.csv(all.avg.p[["estimates"]], "all.avg.p.Feb2024.csv", row.names = FALSE)


## [Optional] Step 3: Test specific models #####

# Phi(~season)p(~ageclass02 * season + region * season * Year)
Phi.s.p.sa_sry <- mark(data=BFS.process, ddl=BFS.ddl, model.parameters=list(Phi=list(formula=~season), p=list(formula=~season*ageclass02 + season*region*Year)), 
                       invisible=F, delete=T, adjust=T)

Phi.s.p.sa_sry$results$real
Phi.s.p.sa_sry$results$beta
write.csv(Phi.s.p.sa_sry$results$real, "Phi.s.p.sa_sry.real.csv")
write.csv(Phi.s.p.sa_sry$results$beta, "Phi.s.p.sa_sry.beta.csv")

# # Phi(~season + ageclass04 + region)p(~ageclass02 * season + region * season * Year)
Phi.s_a_r.p.sa_sry <- mark(data=BFS.process, ddl=BFS.ddl, model.parameters=list(Phi=list(formula=~season+ageclass04+region), p=list(formula=~season*ageclass02 + season*region*Year)), 
                invisible=F, delete=T, adjust=T)


summary(Phi.s_a_r.p.sa_sry, se=T)
Phi.s_a_r.p.sa_sry$results$real
write.csv(Phi.s_a_r.p.sa_sry$results$real, "Phi.s_a04_r.p.sa02_sry.real.csv")
write.csv(Phi.s_a_r.p.sa_sry$results$beta, "Phi.s_a04_r.p.sa02_sry.beta.csv")

collect.models()



## Export results 
# beta estimates
write.csv(stepwise.model.results[["Phi.s_a_r.p.sa_sry"]][["results"]][["beta"]], "Phi.s_a04_r.p.sa02_sry.beta.csv")
# real estimates
write.csv(stepwise.model.results[["Phi.s_a_r.p.sa_sry"]][["results"]][["real"]], "Phi.s_a04_r.p.sa02_sry.real.csv")

# get estimates of the best model
options(max.print=7000)

sink("summary.Phi.WSxrxa04_TWxyxs.p.Wxr_a04.txt")
summary(
  stepwise.model.results[["Phi.WSxrxa04_TWxyxs.p.Wxr_a04"]], se=T
)
sink()

# get PIMS
PIMS(stepwise.model.results[["Phi.WSxrxa04_TWxyxs.p.Wxr_a04"]],"Phi", simplified = TRUE)

### [Optional] Delta Method ####
betas=summary(stepwise.model.results[["Phi.WSxrxa04_TWxyxs.p.Wxr_a04"]])$beta
beta.lcl=betas$estimate-1.96*betas$se
beta.ucl=betas$estimate+1.96*betas$se
# CI for the real parameters
exp(beta.lcl)/(1+exp(beta.lcl))
exp(beta.ucl)/(1+exp(beta.ucl))

deltamethod(~exp(x1)/(1+exp(x1)),mean=betas$estimate[1],cov=betas$se[1]^2)
deltamethod(~exp(x1)/(1+exp(x1)),mean=betas$estimate[2],cov=betas$se[2]^2)


deltamethod(list(~exp(x1)/(1+exp(x1)),~exp(x2)/(1+exp(x2))),
            mean=betas$estimate,stepwise.model.results[["Phi.WSxrxa04_TWxyxs.p.Wxr_a04"]]$results$beta.vcv)

deltamethod(list(~exp(x1)/(1+exp(x1)),~exp(x2)/(1+exp(x2))),
            mean=betas$estimate,stepwise.model.results[["Phi.WSxrxa04_TWxyxs.p.Wxr_a04"]]$results$beta.vcv,ses=FALSE)






### Remove objects & linked files ####
rm(myexample2)
list.files()
cleanup(ask=FALSE)
list.files()
