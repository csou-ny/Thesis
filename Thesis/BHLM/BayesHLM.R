#Using the same data frame from the Frequentist HLM model: AuctionDataFiltered2
install.packages("MCMCglmm")
library(MCMCglmm)
model1 <- MCMCglmm(SalesUSD ~ LEstimateUSD + Length + Width + Signed, 
                   random= ~SiteID, data= AuctionDataFiltered2) #around 25 minutes to render
plot(model1)
summary(model1) #Note: VCV means variance components
#check ACF of time series plots
autocorr(model1$VCV) #for random effects- possible to plot the ACF? yes, see below
autocorr(model1$Sol) #autocorrelation for fixed effects
plot(acf(model1$VC)) #based on acf plots-might have to adjust the burn in and thining
#Now play around with user defined priors
#Books's example
#vari <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.1), nrow=5, ncol=5)
#prior.model2 <- list(G=list(mu=c(0,0.15), V= vari)) #typo in book! B should'nt be there

#Based on Wild Animal Models tutorial---15 minutes to render
p.var <-var(AuctionDataFiltered2$SalesUSD, na.rm = TRUE)
prior.model2 <- list(G= list(G1= list(V= matrix(p.var *0.05), nu=1)), 
                     R= list(V= matrix(p.var*0.95), nu=1))
model2 <- MCMCglmm(SalesUSD ~ LEstimateUSD + Length + Width + Signed, 
                   random= ~SiteID, data= AuctionDataFiltered2, prior= prior.model2)
plot(model2) #very similar to the plots of model1
autocorr(model2$VCV)
autocorr(model2$Sol)
plot(acf(model2$VC))
summary(model2) 
HPDinterval(model1$VCV) #from coda package
#Both have same DIC(compared with model1? yes!)
posterior.mode(model1$VCV)
posterior.mode(model2$VCV) #difference in priors does have an effect on results- more so on 
# SiteID variance and less on the residual(units) variance
HPDinterval(model2$VCV) #model1's HPD is slightly better, still both give a huge HPD that's not useful

#Also consider that the sample size is so large that priors don't have much effect on results
#Now edit the prior again for something more informative and compare models

#Model 3


#------------- Using R Stan
#in code.stan file need to add a blank line at the end of text file due to software peculiarity
install.packages("rstan", repos= "https://cloud.r-project.org/", dependencies = TRUE)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores=parallel::detectCores())
unique(AuctionDataFiltered2$SiteID) #22 levels 242339 obs
#Bayesuan Multilevel Model
lmod <- lm( SalesUSD ~ LEstimateUSD + Length+ Width + Signed, AuctionDataFiltered2)
sdscal <- sd(residuals(lmod))
Xmatrix <- model.matrix(~LEstimateUSD + Length+ Width + Signed, AuctionDataFiltered2)
AuctionDataFiltered2$SiteID <- factor(AuctionDataFiltered2$SiteID)

stan_dat <- list(Nobs=nrow(AuctionDataFiltered2),
                 Npreds= ncol(Xmatrix),
                 Nlevel1= length(unique(AuctionDataFiltered2$SiteID)),
                 response= AuctionDataFiltered2$SalesUSD,
                 X= Xmatrix,
                 levind1= as.numeric(AuctionDataFiltered2$SiteID),
                 sdscal= sdscal) 
#run the model
rt <- stanc("code.stan")
sm <- stan_model(stanc_ret = rt, verbose = FALSE)
set.seed(1)
fit <- sampling(sm, data= stan_dat)

#Do this in Python
write.csv(AuctionDataFiltered2, file="/Users/Sou/Desktop/Thesis/PyStan/Auction.csv")
