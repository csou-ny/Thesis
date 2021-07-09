#subset only results that are sold and fall into the multi-site houses
library(stringr)
DFSold <- subset(DF, is.na(DF$LEstimateUSD)== FALSE & DF$SalesUSD >0)
#Painting Level Variables
DFSold <- subset(DFSold, str_detect(DFSold$House, regex("sotheby|christie|bonham|poly|guard|herit| artcu|doyl|taja|swann|lyon
                                                        |griesb|zurich|doroth|cornette|agutte", ignore_case = TRUE)))
#returns 295060 obs
DFSold2 <- DFSold[, c("ArtistName", "Title", "LEstimateUSD", "Length", "Width", "Signed", "SalesUSD", "House" )]
#give dframe a column ID number for each painting
DFSold2$ID <- seq.int(nrow(DFSold2))

#Site Level Variables--------------------------------
library(plyr)
DFSold2$SiteID <- mapvalues(DFSold2$House, from = c("Christie's Amsterdam","Christie's Hong Kong","Christie's Paris",
                                                    "Sotheby's Amsterdam","Sotheby's Hong Kong","Sotheby's Paris",     
                                                    "Bonhams London","Bonhams New York","Bonhams SF", "China Guardian",
                                                    "Poly International", "Heritage Auctions", "Artcurial", "Doyle NY",
                                                    "Tajan Paris", "Swann Galeries NYC", "Lyon & Turnbull", "Griesbach",
                                                    "Galerie Koller Zurich", "Dorotheum Vienna","Cornette De Saint Cry Paris",
                                                    "Claude Aguttes Paris")
                            ,to = c("C-AM", "C-HK", "C-PR","S-AM", "S-HK", "S-PR", "B-LN", "B-NY", "B-SF", "CH-BJ", "PO-BJ",
                                    "HER", "AR-PR", "DY-NY", "TJ-PR", "SW-NY", "LY-PR", "GR-BR", "GK-ZH", "DO-VN", "CSC-PR"
                                    ,"CA-PR"))
DFSold2$HouseID <- mapvalues(DFSold2$House, from = c("Christie's Amsterdam","Christie's Hong Kong","Christie's Paris",
                                                    "Sotheby's Amsterdam","Sotheby's Hong Kong","Sotheby's Paris",     
                                                    "Bonhams London","Bonhams New York","Bonhams SF")
                            ,to = c("CHRISTIES", "CHRISTIES", "CHRISTIES","SOTHEBYS", "SOTHEBYS", "SOTHEBYS", "BONHAMS", "BONHAMS", "BONHAMS"))

AuctionData <- 
  within(DFSold2,
         {
           HouseID <- factor(HouseID)
           SiteID <- factor(SiteID)
           PaintingID <- factor(ID)
           Signed <- factor(Signed, labels=c("Signed:F", "Signed:T"))
         })
AuctionData$ID <- NULL #drop a column
AuctionData$House <- NULL

#Data Hierarchy---R2.12 (Galecki)
dtID <- subset(AuctionData, select= c(HouseID, SiteID, PaintingID))
names(dtID)
any(duplicated(dtID)) #FALSE- there arent any duplicate ids
library(nlme)
names(gsummary(dtID, form= ~PaintingID, inv=TRUE)) #to show nested property
names(gsummary(dtID, form= ~SiteID, inv=TRUE))
names(gsummary(dtID, form= ~HouseID, inv=TRUE))

#R2.13 (Galecki)
(nms1 <- names(gsummary(AuctionData, form=~HouseID, inv=TRUE))) #House specific
nms2a <- names(gsummary(AuctionData, form=~ SiteID, inv=TRUE)) #Site and paintingID specifics
idx1 <- match(nms1, nms2a)
(nms2 <- nms2a[-idx1])
nms3a <- names(gsummary(AuctionData, form= ~PaintingID, inv=TRUE))
idx2 <- match(c(nms1, nms2), nms3a)
nms3a[-idx2]

#R3.4 (Galecki) Exploration
sapply(AuctionData, FUN= function(x) any(is.na(x))) #it seems that Length and Width vars have missing values
sum(as.numeric(is.na(AuctionData$Length))) #5470 missing values for this var
range(AuctionData$Length, na.rm = TRUE) #0.0 638570.5 range of length--- need to clean up

sum(as.numeric(is.na(AuctionData$Width))) #8818 missing values for this var
range(AuctionData$Width, na.rm = TRUE) #0.38 31496.06 range of width--- need to clean up

#remove these missing values
AuctionDataFiltered <- AuctionData[!as.numeric(is.na(AuctionData$Width)),] #109366 obs
#remove artistname and title
AuctionDataFiltered$ArtistName <- NULL
AuctionDataFiltered$Title <- NULL
#Painting Level Data Scatterplot
library(lattice) 
dotplot(SalesUSD ~ HouseID,AuctionDataFiltered) 
xyplot(SalesUSD~ SiteID, AuctionDataFiltered,type = c("p", "smooth"))

#Need to redo this for more insights when additional vars are added

#First Attempt at Model
form1 <- formula(SalesUSD ~ LEstimateUSD + Length + Width + Signed)
fm18.1 <- lme(form1, random= ~1|HouseID/SiteID , data=AuctionDataFiltered, method="ML")
#R18.6
rsd1 <- resid(fm18.1, level=0) #marginal residuals
range(rsd1)
myPanel <- function(x,y, subscripts, ... ){
  + panel.xyplot(x,y, ...)
  + y1 <- y
  + x1 <- x
  + ltex(x1, y1, names(y1), pos=3)}
xyplot(rsd1 ~ SiteID|SalesUSD, AuctionDataFiltered, type= c("p", "smooth"), panel = myPanel)#not working as hoped

#qqplot
qqnorm(fm18.1) #not even closed to normal

#-----------------------------Second attempt
form2 <- formula(log(SalesUSD) ~ log(LEstimateUSD) + log(Length) + log(Width) + Signed + Length:Width)
fm18.2 <- lme(form2,  random= ~1|HouseID/SiteID , data=AuctionDataFiltered,control = lmeControl(opt = "optim"), method = "REML") 
#lmeControl needs to be specified, otherwise it might not converge
plot(fm18.2, which=1) #looks good
qqnorm(fm18.2) #better but still not great

#-----------------------------Third attempt
form3 <- formula(sqrt(SalesUSD) ~ sqrt(LEstimateUSD) + sqrt(Length) + sqrt(Width) + Signed + LEstimateUSD:Length:Width)
fm18.3 <- lme(form3,  random= ~1|HouseID/SiteID , data=AuctionDataFiltered,control = lmeControl(opt = "optim")) 
#lmeControl needs to be specified, otherwise it might not converge
plot(fm18.3, which=1) #looks good
qqnorm(fm18.3) #better but still not great

#-----------------------------Fourth attempt
form4 <- formula(log(SalesUSD) ~ log(LEstimateUSD) + Length + Width + Signed)
fm18.4 <- lme(form4, random= ~1|HouseID/SiteID, data= AuctionDataFiltered, control = lmeControl(opt = "optim")) 
#lmeControl needs to be specified, otherwise it might not converge
plot(fm18.4, which=1) #looks good; 
qqnorm(residuals(fm18.4)) #better but still not great-obviously not normal but best one so far
qqnorm(fm18.4) #how come this looks worse? ----have to use this!
qqnorm(fm18.4, ~ranef(., level=2)) # also try level 1; should be linear/normal
anova(fm18.4)
qqnorm(fm18.4, form=~resid(., type="p")|SiteID ) #look somewhat normal
qqnorm(fm18.4, form=~resid(., type="p")|HouseID )
#confidence interval---success
intervals(fm18.4)
#predicted values-----unsuccessful-----------------------------------------
auxl <- list(Signed= factor(c("Signed=No", "Signed=Yes")), Length=0,Width=0, LEstimateUSD = 0,
                                         SiteID=factor(c("1A", "1B", "1C","2A", "2B", "2C", "3A", "3B", "3C")),
                                         HouseID= factor(c("CHRISTIES", "CHRISTIES", "CHRISTIES","SOTHEBYS", "SOTHEBYS", "SOTHEBYS", "BONHAMS", "BONHAMS", "BONHAMS")),
                                          SalesUSD= 0)
auxD <- data.frame(expand.grid(auxl))
prd <- predict(fm18.4, auxD, level = 0) #predicted values
#-------------------------------------------------------------------

#How about a two level model?
DFSoldRestricted <- subset(DF, is.na(DF$LEstimateUSD)== FALSE & DF$Sold=="Sold" & DF$SalesUSD >0)#403526 obs
HouseIndex <- c("Christie's Amsterdam","Christie's Hong Kong","Christie's Paris",
                "Sotheby's Amsterdam","Sotheby's Hong Kong","Sotheby's Paris",     
                "Bonhams London","Bonhams New York","Bonhams SF", "China Guardian",
                "Poly International", "Heritage Auctions", "Artcurial", "Doyle NY",
                "Tajan Paris", "Swann Galleries NYC", "Lyon & Turnbull", "Griesbach",
                "Galerie Koller Zurich", "Dorotheum Vienna","Cornette De Saint Cyr Paris",
                "Claude Aguttes Paris")
DFSoldRestricted <- subset(DFSoldRestricted,DFSoldRestricted$House  %in% HouseIndex)
#returns 325982 entries
DFSoldRestricted <- DFSoldRestricted[, c("ArtistName", "Title", "LEstimateUSD", "Length", "Width", "Signed", "SalesUSD", "House" )]
#give dframe a column ID number for each painting
DFSoldRestricted$ID <- seq.int(nrow(DFSoldRestricted))
library(plyr)
DFSoldRestricted$SiteID <- mapvalues(DFSoldRestricted$House, from = HouseIndex
                            ,to = c("C-AM", "C-HK", "C-PR","S-AM", "S-HK", "S-PR", "B-LN", "B-NY", "B-SF", "CH-BJ", "PO-BJ",
                                    "HER", "AR-PR", "DY-NY", "TJ-PR", "SW-NY", "LY-PR", "GR-BR", "GK-ZH", "DO-VN", "CSC-PR"
                                    ,"CA-PR"))
#tier related values
#DFSoldRestricted$SiteIDTier <- mapvalues(DFSoldRestricted$SiteID, from = 
#                                           c("C-AM", "C-HK", "C-PR","S-AM", "S-HK", "S-PR", "B-LN", "B-NY", "B-SF", "CH-BJ", "PO-BJ",
#                                             "HER", "AR-PR", "DY-NY", "TJ-PR", "SW-NY", "LY-PR", "GR-BR", "GK-ZH", "DO-VN", "CSC-PR"
#                                             ,"CA-PR")
#                                     ,to = c(rep("First-Lower",9), rep("Second-Upper",13) ))
AuctionData2 <- 
  within(DFSoldRestricted,
         {
           SiteID <- factor(SiteID)
           PaintingID <- factor(ID)
           Signed <- factor(Signed, labels=c("Signed:F", "Signed:T"))
         })
AuctionData2$ID <- NULL #drop a column; 
AuctionData2$House <- NULL; AuctionData2$HouseID <- NULL; AuctionData2$ArtistName <- NULL; AuctionData2$Title <- NULL
sapply(AuctionData2, FUN= function(x) any(is.na(x))) #it seems that Length and Width vars have missing values
sum(as.numeric(is.na(AuctionData2$Length))) #12530 missing values for this var
range(AuctionData2$Length, na.rm = TRUE) #0.0 638570.5 range of length--- need to clean up

sum(as.numeric(is.na(AuctionData2$Width))) #21322 missing values for this var
range(AuctionData2$Width, na.rm = TRUE) #0.38 47244.09 range of width--- need to clean up
#remove these missing values & extreme values
AuctionDataFiltered2 <- AuctionData2[!as.numeric(is.na(AuctionData2$Width)) 
                                    &!as.numeric(is.na(AuctionData2$Length)) &AuctionData2$Width <200 & AuctionData2$Length <200 &
                                    AuctionData2$Width >10 & AuctionData2$Length >10 & AuctionData2$SalesUSD < 1500000
                                    ,] # 242339 obs
library(lattice)  #explorartory graph
xyplot(SalesUSD~ SiteID, AuctionDataFiltered2,type = c("p", "smooth"))

#boxplots for each auction house:
#boxplot(SalesUSD~ SiteID, AuctionDataFiltered2, horizontal=TRUE)
library(ggplot2)
install.packages("reshape")
library(reshape)

#Change the sitenames
meltData <- melt(AuctionDataFiltered2)
p <- ggplot(AuctionDataFiltered2, aes(factor(SiteID), SalesUSD, fill=Signed) )
p + geom_boxplot(width=.6, position= "dodge") +
  facet_wrap(~SiteID, scale="free")+ xlab("Auction House Sites") +ylab("Prices Sold in USD") 

form5 <- formula(log(SalesUSD) ~ log(LEstimateUSD) + Length+ Width + Signed)
fm18.5 <- lme(form5, random= ~1|SiteID, data= AuctionDataFiltered2, control = lmeControl(opt = "optim")) 
#lmeControl needs to be specified, otherwise it might not converge
plot(fm18.5, which=1) # symmetric around the zero line
qqnorm(residuals(fm18.5)) #curvilinear suggests skewness
qqline(residuals(fm18.5)) #this is the one used in faraway's book
Resid <- data.frame(residuals(fm18.5))
#Attempt at residual examination--------
names(Resid)[1] <- "ResIndex"
Resid$ResNum <-seq.int(nrow(Resid))
which.max(Resid$ResIndex) #208273
extrrows <- head(sort(Resid$ResIndex, decreasing = TRUE), 20)
#Note: PaintingID has been casted as a factor
AuctionDataFiltered2$PaintingID <-as.numeric(AuctionDataFiltered2$PaintingID)
Resid[Resid$Resindex %in% extrrows,]
#-----------------------------------------------------


#normality of random effects looks good
qqnorm(ranef(fm18.5)[,1]/sd(ranef(fm18.5)[,1]) )
abline(a=0,b=1, col="red", lty=2)

#Visualizing the fixed and random effects


qqnorm(fm18.5, ~resid(.)| SiteID)
#confidence interval---success
intervals(fm18.5)
#EBLUPs of the random effects
ref5 <- ranef(fm18.5, level=1)
plot(ref5, xlab=" Random Effects Associated with Each Auction Site", ylab="Auction House Site")
qqnorm(ref5$`(Intercept)`)

#Variance Components
VarCorr(fm18.5)

#fixed effects analysis



#predictions-------unsuccessful
auxl <- list(Signed= factor(c("Signed=No", "Signed=Yes")), Length=seq(12,300, by =50)
               ,Width= seq(12,300, by=50), LEstimateUSD = seq(20, 15380400, by= 1000),
             SiteID=factor(c("1A", "1B", "1C","2A", "2B", "2C", "3A", "3B", "3C")) )
auxD <- data.frame(expand.grid(auxl))
prd <- predict(fm18.5, auxD)

#fit a non randomeffect model and compare it to the mixed effect model using anova()




#####Might have to use GLMM for this######
#Using the LME4.0 package
library(lme4)