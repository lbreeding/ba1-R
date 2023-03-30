# lagsVsNot01.ba1: How important are lags to accuracy and skill of SSR models

# Remove all variables except functions
rm(list = setdiff(ls(), lsf.str()))

# HARD CODE

library(colorspace)
library(pracma)
library(R.matlab)
library("rjson") # for reading json input files



# JSON INPUT. Input data as list from json; in for loop, store the list elements

naJSON <- -99999 # regard -99999 as NA, in cas there is a -99999 in the json file
myData <- fromJSON(file="initDM001.json")
X<- myData
for (j in 1:length(X)){
  a <- names(X)[j]
  b <- paste(a,'<-X$',a,sep='')
  s <- paste(a,'<-',b)
  eval(parse(text=s))
}
rm(a,b,s)
jfig1 <- as.numeric(jfig1) # first figure will have "F1" in name
# 
# # For json
# code_dir<-'/home/dave/Data/RlibraryMeko/'
# outputDir <- '/home/dave/AAATreeRO/test_out/'
# flow_Dir <- '/home/dave/Projects/ba1/Data_ba1/Flows_ba1/NaturalCNRFC/'
# jfig1 <- as.numeric(jfig1) # first figure will have "F1" in name

filesIn1=c(treeFile1,treeFile2) # Files with SSR statistics (as treeFile1, treeFile2 in json)
# 1 is results from model allowing lags. 2 is results from constrained model (lag 0 forced)

source(paste(code_dir,"TranFlow.R",sep="")) # optional transformation of flows

#  Read data for reconstruction that allowed lags in SSRs
df1 <- read.table(file=filesIn1[1], header = TRUE,,skip=2, stringsAsFactors = FALSE )
#  Read data for reconstruction that  did not allowlags in SSRs
df2 <- read.table(file=filesIn1[2], header = TRUE,,skip=2, stringsAsFactors = FALSE )

# Medians of te 37 R2adj and RE
medR2 <- median(df1$R2a,na.rm=TRUE)
medRE <- median(df1$REcv,na.rm=TRUE)

jfig <- jfig1  #   initialized figure numbering



### FIGURE  DOT CHART OF ADJUSTED R-SQUARED AND RECV FOR ALL 37 SSRS

fileoutBase <- 'lagsVsNot01_ba1_F'
jfigS <- as.character(jfig)
fileOut <- paste(outputDir,fileoutBase,jfigS,'.png',sep='')
png(filename=fileOut, width = 400, height = 600)
par(mar=c(5,6,5,2),cex.main=1.4,cex.axis=1.5, cex.lab=1.3, cex.main=1.5)


# Build factor variable that will effectivey code whether from RLP or RNP
n <- dim(df1)[1]
j1 <- (rep(1,n)); j2 <- (rep(2,n)); 
xcode <- c(j1,j2)
xx <- c(df1$R2a,df1$REcv)
ylims = c(-0.10,0.5)
xlims = c(0,2.9)

j <- rep(1,37)
plot(j,df1$R2a,xlim=xlims,ylim=ylims,
     xaxt='n',yaxt='n',cex=1.3,
     ylab='Accuracy or Skill',xlab='Metric',
     main='SSR Models, 37 Chronologies')
j <- rep(2,37)
lines(j,df1$REcv,type='p',col='red',cex=1.3)
lines(xlims,c(0,0),col='gray')
lines(c(0.85,1.15),c(medR2,medR2),col='black')
lines(c(1.85,2.15),c(medRE,medRE),col='red')
axis(1, at = c(1,2),labels=c('Adj R\U00B2','RE'))
axis(2, at = c(0,0.2,0.4))

dev.off()



#  PULL SUBSET OF SSRS FOR WHICH LAGS ENTERED THE STEPWISE MODELING

# Pointer to those chrons for which lags entered mode, and cull rows of df1,df2
L1 <- df1$Model != 100
df1a <-df1[L1,]; df2a <- df2[L1,] # metadata for those
n1 <- sum(L1) # number of SSR models with lags

# Models with lags AND not rejected
L2 <- L1 & !df1$Reject
n2 <- sum(L2) # modes with lags and not rejected
df1b <-df1[L2,]; df2b <- df2[L2,] # metadata for those



#-- PIE CHART SUBSETS
#
# This analysis from run df1, which allowed lags to enter into model if warranted
# A1= include lags, pass screening
# A2= include lags, fail ...
# B1 = no lags, pass
# B2 = no lags, fail
#
# recall
# L1 selected model has lag
# L2 ... and pass

LA1 <- L1 & !df1$Reject
nA1 <- sum(L2); # has lag and pass
LA2 <- L1 & df1$Reject
nA2 <- sum(LA2)  # has lag and fail
LB1 <- !L1 & !df1$Reject
nB1 <- sum(LB1) # no lag and pass
LB2 = !L1 & df1$Reject
nB2 <- sum(LB2)


### FIGURE Simple Pie Chart

jfig <- jfig+1
jfigS <- as.character(jfig)
fileOut <- paste(outputDir,fileoutBase,jfigS,'.png',sep='')
png(filename=fileOut, width = 550, height = 480)
par(mar=c(3,1.5,3,3),cex.main=1.4,cex.axis=1.5, cex.lab=1.3, cex.main=1.5)

slices <- c(nB1, nA1,nA2,nB2)
lbl1 <- paste('No lags, # pass =',as.character(nB1))
lbl2 <- paste('Lags, # pass =',as.character(nA1))
lbl3 <- paste('Lags, # fail =',as.character(nA2))
lbl4 <- paste('No lags, # fail =',as.character(nB2))
pct <- round(slices/sum(slices)*100)
lbls <- c(lbl1, lbl2, lbl3, lbl4)
pie(slices, labels = lbls, cex=1.3,
    col=c('springgreen2','steelblue2','hotpink','orange2'),
    main="37 Single-site-reconstruction (SSR) models")
dev.off()



### FIGURE Heatmap the lag models 

# Initialize a data frame

LA3 <- LA1 | LA2 # from df1, models with lags, pass or fail
nLA3 = sum(LA3)
Mods1 <- df1$Model[LA3]

# Initialized dfM to "lag not in model"
dfM <- data.frame(matrix(1,ncol = 5, nrow = nLA3))
rownames(dfM) <- df1$Site[LA3]
colnames(dfM) <- c('-2','-1','0','+1','+2')

# Fill in 5-col df dfM with following 
# 0 = this lag did not enter, will map as white
# 1 = this lag entered first, darkest color
# 2 = this lag entered second, second darkest color 
#.... etc to as high as 5

# Convert model to character, restoring initial zeros so 5 chars
M <- df1$Model[LA3]
G <-  vector(mode='character', length=nLA3)
for (n in 1:nLA3){
  gthis  <- as.character(M[n])
  ng = nchar(gthis)
  need1 <- 5-ng
  if (need1 ==1){
    add1 <- '0';
  } else if (need1==2) {
    add1 <- '00'
  } else if (need1==3) {
    add1 <-  '000'
  } else if (need1==4) {
    add1 <-  '0000'
  } else {
  }
  if (need1 == 0){
    G[n] <- gthis
  } else {
    G[n] <- paste (add1,gthis,sep='')
  }
}

#  Build dataframe of 1, 2, 3, or 4
#
# dfM, filled in will be a data 25 x 5 data frame with cols 1-2 
#   corresponding to lags -2 to +2 years. Entry of "0" means lag not
#   in model. Otherwise, will have entry representing order of entry in the 
#   stepwise

for (j in 1:nLA3){
  g <- G[j]
  for (m in 1:5){
    gthis <- substr(g,m,m)
    if (gthis == '0'){
      dfM[j,m] <- 0;
    } else {
      dfM[j,m] <-as.numeric(gthis)
    }
    }
}

#  On to plotting figure
jfig <- jfig+1
jfigS <- as.character(jfig)
fileOut <- paste(outputDir,fileoutBase,jfigS,'.png',sep='')
png(filename=fileOut, width = 500, height = 480) #960 x 480
layout( matrix(c(1,2), ncol=2) )

#  Left plot
par(mar = c(5.1, 6.5, 5.1, 0.5),cex.axis=1.1, cex.lab=1.5, cex.main=1.3)

# These should match 0,  1, 2, 3, 4 and 5
# salmon, dark ,lighter, even lighter, green, yellow
colorsMy<- c('#FFC8B0FF','#0000FF','#0080FF','#00DDFF','#55FF00','#FFFF00')
z <- as.matrix(dfM) # image requires matrix
z <- t(z)
z <- flipdim(z,2) 
#z <-flipdim(t(z),1) # ids up left; lags across L to R
image((as.matrix(z)), zlim=c(0,5),
      col = colorsMy,
      xaxt="n", yaxt="n")
# image((as.matrix(z)), zlim=c(0,5),
#       col = hcl.colors(6,"YlOrRd",rev = TRUE),
#       xaxt="n", yaxt="n")
mz <- dim(z)[2];  nz <- dim(z)[1] # rows and cols in z
L3  <- !df1a$Reject # non-reject members of set of models that ended up including lags

# lines horiz
inc <-  0.5/(mz-1)
for (n in 0:mz){
  lines(c(-0.2,1.5),c(n/(mz-1)+inc,n/(mz-1)+inc),col='#808080')
}

# lines vert
inc <-  0.5/(nz-1)
for (n in 0:nz){
  lines(c(n/(nz-1)+inc,n/(nz-1)+inc),c(-0.2,1.5),col='#808080')
}

# Axes
axis(2, at=seq(0-0.5/24,1-0.5/24,length.out=mz), labels=rev(df1a$Site), las=2,padj=0)
axis(1, at=seq(0,1,length.out=nz), labels=(rownames(z)))
title(xlab='Lag (yr)',line=2.3)

# Add y-axis labe
mtext('Tree-Ring Chronology', side = 2, line = 0, outer = TRUE, at = NA,
      adj = NA, padj = 2, cex = 1.3, col = NA, font = NA)

# Right Plot
par(mar = c(4.2, 0.5, 4.27, 7.1),cex.axis=1.1, cex.lab=1.3, cex.main=1.3)
#par(mar = c(5.1, 6.5, 5.1, 0.5),cex.axis=1.1, cex.lab=1.5, cex.main=1.3)

# Build mtx with adj R2 of model allowing lags in pool as col1 and for same
# chrons with constrained lag-0 model as col 2
R2 <- rev(df1a$R2a) # adj R-squared for models allowing lags in pool
R2nL <- rev(df2a$R2a) # ... for models constrained as lag-o
R2x <- cbind(R2nL,R2) 

# Convert negative adjusted R2 to 0, to avoid confusion in stacked bar. This
# because some models have very small negative (-0.02 is largest abs magnitude)
# adj R2.  
Lneg = R2x<0
R2x[Lneg] <- 0

# But want bar-length to match R2 adj of the model allowing lags in pool, rather
# than total of that and R2 adj of a no-lag model
R2x[,2]<- R2x[,2]-R2x[,1]


#xlims <- c(-0.30 1.30)
bp<-barplot(t(R2x), horiz=TRUE,beside=FALSE,
            legend.text=c('No lags','Lags'),args.legend=list(x='topright',inset=c(0,0)))

# Red asterisk at top of bar for "non-rejected" models
L3 <- rev(L3)
for (n in 1:mz){
  Lthis  <- L3[n]
  if (Lthis){
    #x <-5; y<-0.40
    y <- R2[n]+0.02; x <- bp[n]
    lines(y,x,type='p',pch=20,col='red',cex=2)
  }
}


axis(side=1,c(-.35,1.25))
title(xlab='adj R\u00B2',line=2.3)
dev.off()


### FIGURE -- Time plot of natural flow

pfin <- paste(flow_Dir,fileNF,sep='') # file with natural flows

jfig <- jfig+1
jfigS <- as.character(jfig)
fileOut <- paste(outputDir,fileoutBase,jfigS,'.png',sep='')

X<- readMat(pfin) # gives a nested list, X. Can see in R that X has
# a field "D", which is the matlab stucture D. Thus have X$D in R space.

# Must unlist that nested list using function lapply, which applies 
# operations to list elements.
U = lapply(X$D, unlist, use.names=FALSE) # because X$D is nested list

# The 2-river NF sum and corresponding year are in U[11] and U[12]
# Last year (2023) is fake (projected), so cut to end in 2022
q <- as.numeric(unlist(U[11])); yrq <- as.numeric(unlist(U[12]))
L <- yrq <2023;
q <- q[L]; yrq <-yrq[L]
mq <- length(q)

# Compute desired plot limits
xlims <- c(yrq[1]-1,yrq[mq]+1) # limits for x axis

yinc <- 0.02*(max(q)-min(q));
ylims <- c(min(q)-yinc, max(q)+yinc)

qmean <- mean(q) # mean flow

# Build figure
png(filename=fileOut, width = 960, height = 480)
par(mar=c(5,5,5,1),cex.main=1.4,cex.lab=1.3,cex.axis=1.3)
plot(yrq,q,xlim=xlims,ylim=ylims,type="b",pch=1,col="blue",
     xlab='Year',ylab='Flow (kaf)',
     cex=1.2,
     main=paste('Two-River Sum of Observed Water-Year Natural Flow',
                '\n(black line at observed mean)'))
#lines(yrv1,Fits,type="b",pch=2,col="red")
#lines(yrv1,ResMLR1a$CrossValidStorage$CVpredictions,type="b",pch=17,col="#990099")
lines(xlims,c(qmean,qmean))
# h<-par("usr"); yoffset<- (h[4]-h[3])/100; ytop <- h[4]-yoffset
# legend(yrgo1+1,ytop,legend=c("Obs", "Recon","CVpred"),
#        col=c("blue", "red","#990099"),pch=c(1,2,17),lty=1, cex=1.2)
# Shade the calib period for SSR models
xP <- c(1945-0.5,1945-0.5,1999+0.5, 1999+0.5,1945-0.5)
yinc1 <- 1.8*yinc
yP <- c(ylims[1]-yinc1,ylims[2]+yinc1,ylims[2]+yinc1,ylims[1]-yinc1,ylims[1]-yinc1)

polygon(xP,yP, col=rgb(1.00,0,0,0.1),border=NA) # mustard
text (1950,2000, labels = 'Calibration period =1945-1999', adj = NULL,
      pos = 4, offset = 0.0, vfont = NULL,
      cex = 1.3, col = NULL, font = NULL)

dev.off()


### FIGURE -- Time plot of two reconstructions
#
# Plot to include: 1) RLP reconstruction, 1876-1999, 2) its 50% CI, and
# 3) RNP reconstructions


# Read tab-sep files with the reconstructed time series and 50% CI
