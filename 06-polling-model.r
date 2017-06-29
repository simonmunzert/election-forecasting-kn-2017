# FORECASTING THE 2013 BUNDESTAG ELECTION USING DATA FROM VARIOUS POLLS

# remove everything in workspace
rm(list=ls(all=TRUE))

library(foreign)
library(lme4)
library(boot)
library(Hmisc)

# load data
load("data/polls_btw.RData")

parties <- levels(wahlrecht_df$party)
institutes <- levels(wahlrecht_df$institute)
elections <- levels(as.factor(wahlrecht_df$election))

names(wahlrecht_df)




### Figure 2: Lead time selection

# compute days and months to election
wahlrecht_df$months_to_election = round(wahlrecht_df$days_to_election/28)
table(wahlrecht_df$months_to_election)

# compute absolute forecasting error
wahlrecht_df$afr = abs(wahlrecht_df$poll-surveys.df$vote)
summary(surveys.df$afr)

for (i in unique(surveys.df$mte)) {
surveys.df$mafr[surveys.df$mte==i] <- mean(surveys.df$afr[surveys.df$mte==i], na.rm=T)
}

mafr.mte <- vector()
for (i in 1:53) {
mafr.mte[i] <- mean(surveys.df$mafr[surveys.df$mte==(i-1)])
}

pdf(file="fig_lead_R.pdf", height=6, width=9, family="URWTimes")
par(oma=c(5,2,1,1))
par (mar=c(0,3,1,1))
par(yaxs = "i") 
par(xaxs = "i") 
plot(surveys.df$mte, surveys.df$afr, pch=1, col="white", ylim=c(0,.2), xlim=c(-1,53), xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F)
polygon(x=c(7.5,7.5,10.5,10.5), y=c(0,.2,.2,0), col=rgb(220,220,220,200, max=255), border=NA)
points(surveys.df$mte, surveys.df$afr, pch=1, col=rgb(170,170,170, max=255))
lines(0:52, mafr.mte, lwd=2)
#abline(h=mean(surveys.df$afr, na.rm=T), lty=2)
axis(1, at=seq(-10,60,10), labels=seq(-10,60,10))
axis(2, at=seq(0,.2,.05), labels=seq(0,.2,.05))
mtext("Absolute prediction error", side=2, at=.5, line = 0, outer=T, cex=1.5)
mtext("Months to election", side=1, at=.5, line = 3, outer=T,cex=1.5)
dev.off()



### Estimate model: party REs, partyXinstitute REs

# generate partyXinstitute interaction
surveys.df$partyXinstitute <- interaction(surveys.df$party,surveys.df$institute)

# model selection: specify different windows of time
elections <- c(2002,2005,2009)

range.months <- 0:23
afe.df <- array(NA, c(length(elections), length(parties), length(range.months)))
afe.means <- vector()
afe.elections.means <- matrix(NA, length(elections), length(range.months))
for (i in range.months) {
# compute model
model <- lmer(vote ~ poll + (1|party) + (1|partyXinstitute), data=surveys.df[surveys.df$mte>=i & surveys.df$mte<= i+3,])
# retrieve fitted values
surveys.df$fit <- NA
surveys.df$fit[surveys.df$mte>=i & surveys.df$mte<= i+3 & surveys.df$election!=2013] <- fitted(model)
# save fitted values
for (j in 1:length(elections)) {
for (k in 1:length(parties)) {
afe.df[j,k,i+1] <- mean(abs(surveys.df$fit[surveys.df$election==elections[j] & surveys.df$party==parties[k]] - surveys.df$vote[surveys.df$election==elections[j] & surveys.df$party==parties[k]]), na.rm=T)
}
afe.elections.means[j,i+1] <- mean(afe.df[j,,i+1], na.rm=T) # mean afe's over pooled over elections and parties, by window of time
}
afe.means[i+1] <- mean(afe.df[,,i+1], na.rm=T) # mean afe's over pooled over elections and parties, by window of time
}

# build plot
pdf(file="fig_modelspec_R_4months.pdf", height=8, width=10, family="URWTimes")
par(oma=c(5,2,1,1)+1.5)
par (mar=c(0,3,1,1))
par(yaxs = "i") 
par(xaxs = "i") 
plot(range.months, seq(min(afe.df, na.rm=T), max(afe.df, na.rm=T), length.out=length(range.months)), col="white", ylim=c(0,.09), xlim=c(-1,25), xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F)
abline(h=seq(0,.09,.01), lty=2, col="darkgrey")
for (i in range.months) {
points(rep(i, length(afe.df[,1,i+1])), as.vector(afe.df[,1,i+1]), col=rgb(60,60,60, max=255), pch=20)
points(rep(i, length(afe.df[,2,i+1])), as.vector(afe.df[,2,i+1]), col=rgb(222,45,38, max=255), pch=20)
points(rep(i, length(afe.df[,3,i+1])), as.vector(afe.df[,3,i+1]), col=rgb(49,163,84, max=255), pch=20)
points(rep(i, length(afe.df[,4,i+1])), as.vector(afe.df[,4,i+1]), col=rgb(136,86,167, max=255), pch=20)
points(rep(i, length(afe.df[,5,i+1])), as.vector(afe.df[,5,i+1]), col=rgb(254,178,76, max=255), pch=20)
points(rep(i, length(afe.df[,6,i+1])), as.vector(afe.df[,6,i+1]), col=rgb(200,200,200, max=255), pch=20)
lines(range.months, afe.elections.means[1,], lwd=1, lty=2)
lines(range.months, afe.elections.means[2,], lwd=1, lty=2)
lines(range.months, afe.elections.means[3,], lwd=1, lty=2)
lines(range.months, afe.means, lwd=2)
}
axis(1, at=seq(-10,60,5), labels=seq(-10,60,5))
axis(2, at=seq(0,.09,.01), labels=seq(0,.09,.01))
text(23, afe.elections.means[1,24], "2002", pos=4, cex=.8)
text(23, afe.elections.means[2,24], "2005", pos=4, cex=.8)
text(23, afe.elections.means[3,24], "2009", pos=4, cex=.8)
text(23, afe.means[24], "average", pos=4, cex=.8, font=2)
mtext("Absolute forecasting error", side=2, at=.5, line = 0, outer=T, cex=1.5)
mtext("Months to election", side=1, at=.5, line = 3, outer=T,cex=1.5)
dev.off()





# compute final model
#surveys.df <- surveys.df[!is.na(surveys.df$N),] # select polls with valid N
surveys.df$partyXinstitute <- interaction(surveys.df$party,surveys.df$institute)
model <- lmer(vote ~ poll + (1|party) + (1|partyXinstitute), data=surveys.df[surveys.df$mte>=8 & surveys.df$mte<= 10,])
summary(model)

# retrieve REs
ranef(model)$party
ranef(model)$partyXinstitute

# retrieve fitted values
surveys.df$fit <- NA
surveys.df$fit[surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$election!=2013] <- fitted(model)

# how many surveys for forecasting?
length(surveys.df$poll[surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$election==2013 & !is.na(surveys.df$N)])/6
length(surveys.df$poll[surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$election!=2013])/6


# for documentation: retrieve model estimates
model.table <-  as.data.frame(matrix(NA,nrow=6,ncol=2))
colnames(model.table) <- c("estimate", "std. error")
rownames(model.table) <- c("Intercept", "Poll result", "Party-level variance", "Party-institute-level variance", "Residual variance", "N")
model.table[1,1] <-  round(fixef(model)[1], 4)
model.table[2,1] <-  round(fixef(model)[2], 4)
model.table[1,2] <-  round(sqrt(diag(vcov(model)))[1], 4)
model.table[2,2] <-  round(sqrt(diag(vcov(model)))[2], 4)
model.table[3,1] <-  round(as.numeric(summary(model)@REmat[2,3]), 4)
model.table[4,1] <-  round(as.numeric(summary(model)@REmat[1,3]), 4)
model.table[5,1] <-  round(as.numeric(summary(model)@REmat[3,3]), 4)
model.table[6,1] <-  nrow(model@X)
model.table

ranefs.party <- round(ranef(model)$party[[1]],4)
ranefs.partyXinstitutions <- matrix(round(ranef(model)$partyXinstitute[[1]],5), nrow=6, ncol=6, byrow= TRUE)
ranefs.party
ranefs.partyXinstitutions 


# predictions for btw 2013

# retrieve REs
names.re.party <- rownames(ranef(model)$party)
re.party <- vector()
surveys.df$re.party <- ranef(model)$party[[1]][as.numeric(surveys.df$party)]
names.re.partyXinstitute <- rownames(ranef(model)$partyXinstitute)
re.partyXinstitute <- vector()
surveys.df$re.partyXinstitute <- ranef(model)$partyXinstitute[[1]][as.numeric(surveys.df$partyXinstitute)]

# build predictions
surveys.df$preds <- NA
surveys.df$preds <- (fixef(model)[1] + fixef(model)[2]*surveys.df$poll +
+ surveys.df$re.party
+ surveys.df$re.partyXinstitute
)

### raw polling data
# CDU/CSU
mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU"])
# SPD
mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="SPD"])
# B'90/Die Gr?nen
mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="B'90/Die Gr?nen"])
# Die Linke
mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Die Linke"])
# FDP
mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="FDP"])
# Others
mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Others"])

polling.mean <- vector()
for (j in 1:length(parties)){
polling.mean[j] <- round(mean(surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party==parties[j]]),3)
}

### empirical Bayes prediction
# CDU/CSU
mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU"])
# SPD
mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="SPD"])
# B'90/Die Gr?nen
mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="B'90/Die Gr?nen"])
# Die Linke
mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Die Linke"])
# FDP
mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="FDP"])
# Others
mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Others"])

preds.mean <- vector()
for (j in 1:length(parties)){
preds.mean[j] <- round(mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party==parties[j]]),3)
}

### graph: raw polls + empirical Bayes prediction
pdf(file="fig_estimates_R.pdf", height=4, width=9, family="URWTimes")
par(oma=c(5,1,1,1)+1.5)
par(mar=c(0,1,4,1))
par(pty="s")
par(yaxs = "i") 
par(xaxs = "i") 
par(mfcol=c(2,6))
for (j in parties) {
# set xlims
if (j=="CDU/CSU") { xlim <- c(.30,.50) }
if (j=="SPD") { xlim <- c(.20,.40) }
if (j=="B'90/Die Grünen") { xlim <- c(.05,.20) }
if (j=="Die Linke") { xlim <- c(.00,.15) }
if (j=="FDP") { xlim <- c(.00,.15) }
if (j=="Others") { xlim <- c(.00,.15) }
# raw polls
X <- surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party==j]
plot(density(X, bw=.01), col="white", xlim=xlim, xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F)
abline(v=seq(0,.5,.05), col="darkgrey", lty=2)
lines(density(X, bw=.01), col="black")
rug(X, ticksize = .1, side=1, lwd=.5)
abline(v=median(X), col="black")
if (j=="CDU/CSU") { axis(1, at=seq(.30,.50,.05), labels=seq(.30,.50,.05), col.axis="white") }
if (j=="SPD") { axis(1, at=seq(.20,.40,.05), labels=seq(.20,.40,.05), col.axis="white") }
if (j=="B'90/Die Grünen") { axis(1, at=seq(.05,.20,.05), labels=seq(.05,.2,.05), col.axis="white") }
if (j=="Die Linke") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05), col.axis="white") }
if (j=="FDP") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05), col.axis="white") }
if (j=="Others") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05), col.axis="white") }
title(main=j, line=2.5, cex=.7, font=3)
if (j=="CDU/CSU") { mtext("Raw polling results", at=.27, side=3,  line=.5, adj=0, font=3, cex=.8) }
# empirical bayes estimates
X.pred <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party==j]
plot(density(X.pred, bw=.01), col="white", xlim=xlim, xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F)
abline(v=seq(0,.5,.05), col="darkgrey", lty=2)
lines(density(X.pred, bw=.01), col="black")
rug(X.pred, ticksize = .1, side=1, lwd=.5)
abline(v=median(X.pred), col="black")
if (j=="CDU/CSU") { axis(1, at=seq(.30,.50,.05), labels=seq(.30,.50,.05)) }
if (j=="SPD") { axis(1, at=seq(.20,.40,.05), labels=seq(.20,.40,.05)) }
if (j=="B'90/Die Grünen") { axis(1, at=seq(.05,.20,.05), labels=seq(.05,.2,.05)) }
if (j=="Die Linke") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05)) }
if (j=="FDP") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05)) }
if (j=="Others") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05)) }
if (j=="CDU/CSU") { mtext("Model estimates", at=.27, side=3,  line=.5, adj=0, font=3, cex=.8) }
if (j=="CDU/CSU") { mtext("Forecasted vote share", side=1, at=.5, line = 3, outer=T) }
}
dev.off()

pdf(file="fig_estimates_hist_R.pdf", height=4, width=9, family="URWTimes")
par(oma=c(5,1,1,1))
par(mar=c(0,1,4,1))
par(pty="s")
par(yaxs = "i") 
par(xaxs = "i") 
par(mfcol=c(2,6))
for (j in parties) {
# set xlims
if (j=="CDU/CSU") { xlim <- c(.30,.50) }
if (j=="SPD") { xlim <- c(.20,.40) }
if (j=="B'90/Die Gr?nen") { xlim <- c(.05,.20) }
if (j=="Die Linke") { xlim <- c(.00,.15) }
if (j=="FDP") { xlim <- c(.00,.15) }
if (j=="Others") { xlim <- c(.00,.15) }
# raw polls
X <- surveys.df$poll[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party==j]
hist(X, freq = FALSE, breaks=seq(min(X)-.05, max(X)+.05,by=.01), xlim=xlim, xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F, border="white")
abline(v=seq(0,.5,.05), col="darkgrey", lty=2)
abline(v=median(X), col="red")
hist(X, freq = FALSE, breaks=seq(min(X)-.05, max(X)+.05,by=.01), xlim=xlim, xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F, add=T)
if (j=="CDU/CSU") { axis(1, at=seq(.30,.50,.05), labels=seq(.30,.50,.05), col.axis="white") }
if (j=="SPD") { axis(1, at=seq(.20,.40,.05), labels=seq(.20,.40,.05), col.axis="white") }
if (j=="B'90/Die Gr?nen") { axis(1, at=seq(.05,.20,.05), labels=seq(.05,.2,.05), col.axis="white") }
if (j=="Die Linke") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05), col.axis="white") }
if (j=="FDP") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05), col.axis="white") }
if (j=="Others") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05), col.axis="white") }
title(main=j, line=2.5, cex=.7, font=3)
if (j=="CDU/CSU") { mtext("Raw polling results", at=.27, side=3,  line=.5, adj=0, font=3, cex=.8) }
# empirical bayes estimates
X.pred <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party==j]
hist(X.pred, freq = FALSE, breaks=seq(min(X)-.05, max(X)+.05,by=.01), xlim=xlim, xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F, border="white")
abline(v=seq(0,.5,.05), col="darkgrey", lty=2)
abline(v=median(X.pred), col="red")
hist(X.pred, freq = FALSE, breaks=seq(min(X)-.05, max(X)+.05,by=.01), xlim=xlim, xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F, add=T)
if (j=="CDU/CSU") { axis(1, at=seq(.30,.50,.05), labels=seq(.30,.50,.05)) }
if (j=="SPD") { axis(1, at=seq(.20,.40,.05), labels=seq(.20,.40,.05)) }
if (j=="B'90/Die Gr?nen") { axis(1, at=seq(.05,.20,.05), labels=seq(.05,.2,.05)) }
if (j=="Die Linke") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05)) }
if (j=="FDP") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05)) }
if (j=="Others") { axis(1, at=seq(.00,.15,.05), labels=seq(.00,.15,.05)) }
if (j=="CDU/CSU") { mtext("Model estimates", at=.27, side=3,  line=.5, adj=0, font=3, cex=.8) }
if (j=="CDU/CSU") { mtext("Predicted vote share", side=1, at=.5, line = 3, outer=T) }
}
dev.off()



### precision-weighted forecast
surveys.df$variance <- (surveys.df$preds*(1-surveys.df$preds))/(surveys.df$N-1)
surveys.df$precision <- 1/surveys.df$variance

#CDU/CSU
weighted.mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU" & !is.na(surveys.df$N)], surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU" & !is.na(surveys.df$N)], na.rm=T)
#SPD
weighted.mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="SPD" & !is.na(surveys.df$N)], surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="SPD" & !is.na(surveys.df$N)], na.rm=T)
#B'90/Die Gr?nen
weighted.mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="B'90/Die Gr?nen" & !is.na(surveys.df$N)], surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="B'90/Die Gr?nen" & !is.na(surveys.df$N)], na.rm=T)
#Die Linke
weighted.mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Die Linke" & !is.na(surveys.df$N)], surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Die Linke" & !is.na(surveys.df$N)], na.rm=T)
#FDP
weighted.mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="FDP" & !is.na(surveys.df$N)], surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="FDP" & !is.na(surveys.df$N)], na.rm=T)
#Others
weighted.mean(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Others" & !is.na(surveys.df$N)], surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Others" & !is.na(surveys.df$N)], na.rm=T)


### bootstrap precision-weighted forecast to gain uncertainty estimates
num.fc <- length(surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU" & !is.na(surveys.df$N)])
n.subsample <- round(((num.fc-2)^2)/(num.fc-1)) # compute size of subsample (Kovar et al., 1988)

emp.bayes.estimates <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU" & !is.na(surveys.df$N)]
weights <- surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="CDU/CSU" & !is.na(surveys.df$N)]
set.seed(123)
(forecast.boot.cdsu.mean <- boot(emp.bayes.estimates, wtd.mean, weights=weights, 1000))
(forecast.boot.cdsu.quantile <- boot(emp.bayes.estimates, wtd.quantile, weights=weights, probs=c(.025, .1, .9, .975), 1000))
forecast.cdsu <- vector()
forecast.cdsu[1] <- mean(forecast.boot.cdsu.mean$t)
forecast.cdsu[2] <- mean(forecast.boot.cdsu.quantile$t[,2])
forecast.cdsu[3] <- mean(forecast.boot.cdsu.quantile$t[,3])
forecast.cdsu[4] <- mean(forecast.boot.cdsu.quantile$t[,1])
forecast.cdsu[5] <- mean(forecast.boot.cdsu.quantile$t[,4])

emp.bayes.estimates <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="SPD" & !is.na(surveys.df$N)]
weights <- surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="SPD" & !is.na(surveys.df$N)]
set.seed(123)
(forecast.boot.spd.mean <- boot(emp.bayes.estimates, wtd.mean, weights=weights, 1000))
(forecast.boot.spd.quantile <- boot(emp.bayes.estimates, wtd.quantile, weights=weights, probs=c(.025, .1, .9, .975), 1000))
forecast.spd <- vector()
forecast.spd[1] <- mean(forecast.boot.spd.mean$t)
forecast.spd[2] <- mean(forecast.boot.spd.quantile$t[,2])
forecast.spd[3] <- mean(forecast.boot.spd.quantile$t[,3])
forecast.spd[4] <- mean(forecast.boot.spd.quantile$t[,1])
forecast.spd[5] <- mean(forecast.boot.spd.quantile$t[,4])

emp.bayes.estimates <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="B'90/Die Gr?nen" & !is.na(surveys.df$N)]
weights <- surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="B'90/Die Gr?nen" & !is.na(surveys.df$N)]
set.seed(123)
(forecast.boot.gru.mean <- boot(emp.bayes.estimates, wtd.mean, weights=weights, 1000))
(forecast.boot.gru.quantile <- boot(emp.bayes.estimates, wtd.quantile, weights=weights, probs=c(.025, .1, .9, .975), 1000))
forecast.gru <- vector()
forecast.gru[1] <- mean(forecast.boot.gru.mean$t)
forecast.gru[2] <- mean(forecast.boot.gru.quantile$t[,2])
forecast.gru[3] <- mean(forecast.boot.gru.quantile$t[,3])
forecast.gru[4] <- mean(forecast.boot.gru.quantile$t[,1])
forecast.gru[5] <- mean(forecast.boot.gru.quantile$t[,4])

emp.bayes.estimates <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Die Linke" & !is.na(surveys.df$N)]
weights <- surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Die Linke" & !is.na(surveys.df$N)]
set.seed(123)
(forecast.boot.lin.mean <- boot(emp.bayes.estimates, wtd.mean, weights=weights, 1000))
(forecast.boot.lin.quantile <- boot(emp.bayes.estimates, wtd.quantile, weights=weights, probs=c(.025, .1, .9, .975), 1000))
forecast.lin <- vector()
forecast.lin[1] <- mean(forecast.boot.lin.mean$t)
forecast.lin[2] <- mean(forecast.boot.lin.quantile$t[,2])
forecast.lin[3] <- mean(forecast.boot.lin.quantile$t[,3])
forecast.lin[4] <- mean(forecast.boot.lin.quantile$t[,1])
forecast.lin[5] <- mean(forecast.boot.lin.quantile$t[,4])

emp.bayes.estimates <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="FDP" & !is.na(surveys.df$N)]
weights <- surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="FDP" & !is.na(surveys.df$N)]
set.seed(123)
(forecast.boot.fdp.mean <- boot(emp.bayes.estimates, wtd.mean, weights=weights, 1000))
(forecast.boot.fdp.quantile <- boot(emp.bayes.estimates, wtd.quantile, weights=weights, probs=c(.025, .1, .9, .975), 1000))
forecast.fdp <- vector()
forecast.fdp[1] <- mean(forecast.boot.fdp.mean$t)
forecast.fdp[2] <- mean(forecast.boot.fdp.quantile$t[,2])
forecast.fdp[3] <- mean(forecast.boot.fdp.quantile$t[,3])
forecast.fdp[4] <- mean(forecast.boot.fdp.quantile$t[,1])
forecast.fdp[5] <- mean(forecast.boot.fdp.quantile$t[,4])

emp.bayes.estimates <- surveys.df$preds[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Others" & !is.na(surveys.df$N)]
weights <- surveys.df$precision[surveys.df$election==2013 & surveys.df$mte>=8 & surveys.df$mte<= 10 & surveys.df$party=="Others" & !is.na(surveys.df$N)]
set.seed(123)
(forecast.boot.oth.mean <- boot(emp.bayes.estimates, wtd.mean, weights=weights, 1000))
(forecast.boot.oth.quantile <- boot(emp.bayes.estimates, wtd.quantile, weights=weights, probs=c(.025, .1, .9, .975), 1000))
forecast.oth <- vector()
forecast.oth[1] <- mean(forecast.boot.oth.mean$t)
forecast.oth[2] <- mean(forecast.boot.oth.quantile$t[,2])
forecast.oth[3] <- mean(forecast.boot.oth.quantile$t[,3])
forecast.oth[4] <- mean(forecast.boot.oth.quantile$t[,1])
forecast.oth[5] <- mean(forecast.boot.oth.quantile$t[,4])



### summarize all forecasts/estimates
forecast.table <-  as.data.frame(matrix(NA,nrow=6,ncol=7))
colnames(forecast.table) <- c("raw polling mean", "emp. bayes mean", "forecast mean, boot", "forecast 80lo, boot", "forecast 80hi, boot", "forecast 95lo, boot", "forecast95hi, boot")
rownames(forecast.table) <- parties

forecast.table[,1] <- polling.mean
forecast.table[,2] <- preds.mean

forecast.table[1,3:7] <- forecast.cdsu
forecast.table[2,3:7] <- forecast.spd
forecast.table[3,3:7] <- forecast.gru
forecast.table[4,3:7] <- forecast.lin
forecast.table[5,3:7] <- forecast.fdp
forecast.table[6,3:7] <- forecast.oth

forecast.table


### shares of potential coalitions
sum.shares <- sum(preds.mean[1:5])
preds.mean[1]/sum.shares #cdsu
preds.mean[2]/sum.shares #spd
preds.mean[3]/sum.shares #gru
preds.mean[4]/sum.shares #lin
preds.mean[5]/sum.shares #fdp
(preds.mean[1]+preds.mean[5])/sum.shares #cdsu + fdp
(preds.mean[2]+preds.mean[3])/sum.shares #spd + gru
(preds.mean[1]+preds.mean[3])/sum.shares #cdsu + gru
(preds.mean[1]+preds.mean[2])/sum.shares #cdsu + spd
(preds.mean[2]+preds.mean[3]+preds.mean[5])/sum.shares #spd + fdp + gru
(preds.mean[2]+preds.mean[3]+preds.mean[4])/sum.shares #spd + gru + lin




