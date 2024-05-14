library(R2ucare)

# do GOF test for summer and winter resightings separately: 
merged5
cr <- matrix(ncol=nchar(merged5$ch[1]), nrow=dim(merged5)[1])
rownames(cr) <- rownames(merged5)
for (i in 1:dim(merged5)[1]) cr[i,] <- as.numeric(strsplit(merged5$ch[i], "")[[1]])
region = merged5$most_visited

cr.winter <- cr[,seq(1,34,2)]
cr.summer <- cr[,seq(2,34,2)]
head(cr.winter)
head(cr.summer)

# check that this went right:
ch.winter <- BFS_06_23_count.3[,c(1,seq(2,35,2))]
ch.winter[ch.winter$band.of.ind.=="02K",]
ch.summer <- BFS_06_23_count.3[,c(1,seq(3,35,2))]
ch.summer[ch.summer$band.of.ind.=="02K",]
# checked!

# checking the raw data of Japanese winterers:
cr.J = cr[region=="J",]
cr.J.first <- apply(cr.J, 1, function(x) min(which(x==1)))
table(cr.J.first) # the number of newly entering Japanese birds in the dataset has been relatively stable over time
colSums(cr.J)[seq(1,23,2)] # number of Japanese wintering birds seen in winter are quite high in recent years!
# I don't understand then why the resighting rate is estimated to have strongly decreased.

# GOF test on winter resightings only:

# conduct TEST 3.SR separately to 3 wintering regions (there's no 'K' birds anymore)
test3sr_T <- test3sr(cr.winter[region=="T",], freq=rep(1,dim(cr.winter[region=="T",])[1]))
test3sr_J <- test3sr(cr.winter[region=="J",], freq=rep(1,dim(cr.winter[region=="J",])[1]))
test3sr_S <- test3sr(cr.winter[region=="S",], freq=rep(1,dim(cr.winter[region=="S",])[1]))

test3sr.comb <- rbind(test3sr_T$test3sr, test3sr_J$test3sr, test3sr_S$test3sr)
rownames(test3sr.comb) = c('T','J','S')
test3sr.sum <- colSums(test3sr.comb[,1:2])
cbind(X2=test3sr.sum[1], df=test3sr.sum[2], chat=test3sr.sum[1]/test3sr.sum[2]) # c-hat=1.11

test3sm_T <- test3sm(cr.winter[region=="T",], freq=rep(1,dim(cr.winter[region=="T",])[1]))
test3sm_J <- test3sm(cr.winter[region=="J",], freq=rep(1,dim(cr.winter[region=="J",])[1]))
test3sm_S <- test3sm(cr.winter[region=="S",], freq=rep(1,dim(cr.winter[region=="S",])[1]))

test3sm.comb <- rbind(test3sm_T$test3sm, test3sm_J$test3sm, test3sm_S$test3sm)
rownames(test3sm.comb) = c('T','J','S')
test3sm.sum <- colSums(test3sm.comb[,1:2])
cbind(X2=test3sm.sum[1], df=test3sm.sum[2], chat=test3sm.sum[1]/test3sm.sum[2]) # c-hat=0.47

test2ct_T <- test2ct(cr.winter[region=="T",], freq=rep(1,dim(cr.winter[region=="T",])[1])) # p<0.0001
test2ct_J <- test2ct(cr.winter[region=="J",], freq=rep(1,dim(cr.winter[region=="J",])[1])) 
test2ct_S <- test2ct(cr.winter[region=="S",], freq=rep(1,dim(cr.winter[region=="S",])[1])) # p=0.005

test2ct.comb <- rbind(test2ct_T$test2ct, test2ct_J$test2ct, test2ct_S$test2ct)
rownames(test2ct.comb) = c('T','J','S')
test2ct.sum <- colSums(test2ct.comb[,1:2])
cbind(X2=test2ct.sum[1], df=test2ct.sum[2], chat=test2ct.sum[1]/test2ct.sum[2]) # chat=3.39

test2cl_T <- test2cl(cr.winter[region=="T",], freq=rep(1,dim(cr.winter[region=="T",])[1]))
test2cl_J <- test2cl(cr.winter[region=="J",], freq=rep(1,dim(cr.winter[region=="J",])[1]))
test2cl_S <- test2cl(cr.winter[region=="S",], freq=rep(1,dim(cr.winter[region=="S",])[1]))

test2cl.comb <- rbind(test2cl_T$test2cl, test2cl_J$test2cl, test2cl_S$test2cl)
rownames(test2cl.comb) = c('T','J','S')
test2cl.sum <- colSums(test2cl.comb[,1:2])
cbind(X2=test2cl.sum[1], df=test2cl.sum[2], chat=test2cl.sum[1]/test2cl.sum[2]) # chat=0.88

# all tests combined:
tests.combined <- rbind(test3sr.sum, test3sm.sum, test2ct.sum, test2cl.sum)
tests.combined.sum = colSums(tests.combined)
cbind(X2=tests.combined.sum[1], df=tests.combined.sum[2], chat=tests.combined.sum[1]/tests.combined.sum[2]) # c-hat=1.52

# GOF test on summer data only (pretending that the first winter resighting was done in the summer of that year)

# retrieve first winter resighting for each bird:
first.winter.resightings = apply(cr.winter, 1, function(x) min(which(x==1)))
# place a 1 in cr.summer in the column(=year) of the first winter resighting
cr.summer.adj = cr.summer
for (i in 1:dim(cr.summer.adj)[1]) cr.summer.adj[i,first.winter.resightings[i]]=1

# Do GOF tests: if many wintering birds are never observed in summer, TEST3.SR will become very significant. 
test3sr_T <- test3sr(cr.summer.adj[region=="T",], freq=rep(1,dim(cr.summer.adj[region=="T",])[1])) # p<0.0001
test3sr_J <- test3sr(cr.summer.adj[region=="J",], freq=rep(1,dim(cr.summer.adj[region=="J",])[1]))  
test3sr_S <- test3sr(cr.summer.adj[region=="S",], freq=rep(1,dim(cr.summer.adj[region=="S",])[1])) # p=0.055

test3sr_overall <- test3sr(cr.summer.adj, freq=rep(1,dim(cr.summer.adj)[1])) # p<0.001

test3sr.comb <- rbind(test3sr_T$test3sr, test3sr_J$test3sr, test3sr_S$test3sr)
rownames(test3sr.comb) = c('T','J','S')
test3sr.sum <- colSums(test3sr.comb[,1:2])
cbind(X2=test3sr.sum[1], df=test3sr.sum[2], chat=test3sr.sum[1]/test3sr.sum[2]) # c-hat=2.85

test3sm_T <- test3sm(cr.summer.adj[region=="T",], freq=rep(1,dim(cr.summer.adj[region=="T",])[1]))
test3sm_J <- test3sm(cr.summer.adj[region=="J",], freq=rep(1,dim(cr.summer.adj[region=="J",])[1]))
test3sm_S <- test3sm(cr.summer.adj[region=="S",], freq=rep(1,dim(cr.summer.adj[region=="S",])[1]))

test3sm.comb <- rbind(test3sm_T$test3sm, test3sm_J$test3sm, test3sm_S$test3sm)
rownames(test3sm.comb) = c('T','J','S')
test3sm.sum <- colSums(test3sm.comb[,1:2])
cbind(X2=test3sm.sum[1], df=test3sm.sum[2], chat=test3sm.sum[1]/test3sm.sum[2]) # c-hat=0.72

test2ct_T <- test2ct(cr.summer.adj[region=="T",], freq=rep(1,dim(cr.summer.adj[region=="T",])[1])) # p<0.001
test2ct_J <- test2ct(cr.summer.adj[region=="J",], freq=rep(1,dim(cr.summer.adj[region=="J",])[1])) 
test2ct_S <- test2ct(cr.summer.adj[region=="S",], freq=rep(1,dim(cr.summer.adj[region=="S",])[1])) # p=0.005

test2ct.comb <- rbind(test2ct_T$test2ct, test2ct_J$test2ct, test2ct_S$test2ct)
rownames(test2ct.comb) = c('T','J','S')
test2ct.sum <- colSums(test2ct.comb[,1:2])
cbind(X2=test2ct.sum[1], df=test2ct.sum[2], chat=test2ct.sum[1]/test2ct.sum[2]) # chat=0.77

test2cl_T <- test2cl(cr.summer.adj[region=="T",], freq=rep(1,dim(cr.summer.adj[region=="T",])[1]))
test2cl_J <- test2cl(cr.summer.adj[region=="J",], freq=rep(1,dim(cr.summer.adj[region=="J",])[1]))
test2cl_S <- test2cl(cr.summer.adj[region=="S",], freq=rep(1,dim(cr.summer.adj[region=="S",])[1]))

test2cl.comb <- rbind(test2cl_T$test2cl, test2cl_J$test2cl, test2cl_S$test2cl)
rownames(test2cl.comb) = c('T','J','S')
test2cl.sum <- colSums(test2cl.comb[,1:2])
cbind(X2=test2cl.sum[1], df=test2cl.sum[2], chat=test2cl.sum[1]/test2cl.sum[2]) # chat=0.76

# all tests combined:
tests.combined <- rbind(test3sr.sum, test3sm.sum, test2ct.sum, test2cl.sum)
tests.combined.sum = colSums(tests.combined)
cbind(X2=tests.combined.sum[1], df=tests.combined.sum[2], chat=tests.combined.sum[1]/tests.combined.sum[2]) # c-hat=1.35


