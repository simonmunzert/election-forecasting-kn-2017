### -----------------------------
### simon munzert
### social media data
### -----------------------------

## peparations -------------------

source("packages.r")
source("functions.r")

library(foreign)
library(lme4)
library(boot)
library(Hmisc)


# load data
load("data/polls_btw.RData")
load("data/ger_model_df.RData")

# check polling data
names(wahlrecht_df)
wahlrecht_df$party <- as.character(wahlrecht_df$party)
wahlrecht_df <- filter(wahlrecht_df, party != "afd", date > "1998-01-01")


### prepare data -------------------------------

results <- select(ger_df_long, year, party, voteshare)
names(results) <- c("election", "party", "voteshare")
wahlrecht_df <- merge(wahlrecht_df, results, all.x = TRUE, by = c("election", "party"))



# plot support trend, by party, loess curve
parties <- c("cdu", "spd", "fdp", "gru", "lin")
parties_names <- c("CDU/CSU", "SPD", "FDP", "B'90/Die Grünen", "Die Linke")
parties_cols = c("#0000ff", "#e3000f","#ffed00", "#46962b","#ff0000")

#wahlrecht_df <- filter(wahlrecht_df, party != "afd", date > "2014-01-01")
par(mar=c(2.5,2,2,0)+.1)
par(oma=c(0,0,0,0)+.1)
par(mfrow = c(5, 1))
for (i in seq_along(parties)) {
  dat <- filter(wahlrecht_df, party == parties[i])
  plot(dat$date, dat$support, col= rgb(0, 0, 0, .5), pch = 20, cex=.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = parties_names[i])
  lines(loess.smooth(dat$date, dat$support, span = .2E-2, evaluation = 230), col = parties_cols[i], lwd = 2)
  dat_range <- c(min(ymd(wahlrecht_df$date), na.rm = TRUE), max(ymd(wahlrecht_df$date), na.rm = TRUE))
  axis(1, seq(dat_range[1], dat_range[2], by = "1 year"), seq(year(dat_range[1]), year(dat_range[2]), 1))
  axis(2, seq(0, 60, 2), seq(0, 60, 2))
}



### select lead time ---------------------------

# compute days and months to election
wahlrecht_df$mte = round(wahlrecht_df$days_to_election/30)
table(wahlrecht_df$mte)

# compute absolute forecasting error
wahlrecht_df$afe = abs(wahlrecht_df$support-wahlrecht_df$voteshare)
summary(wahlrecht_df$afe)

# compute monthly average of afe
for (i in unique(wahlrecht_df$mte)) {
wahlrecht_df$mafe[wahlrecht_df$mte == i] <- mean(wahlrecht_df$afe[wahlrecht_df$mte == i], na.rm = T)
}

# compute monthly average of afe, write in vector
mafe.mte <- vector()
for (i in 1:50) {
mafe.mte[i] <- mean(wahlrecht_df$mafe[wahlrecht_df$mte==(i-1)])
}

# plot monthly-level forecasting error
plot(wahlrecht_df$mte, wahlrecht_df$afe, pch=1, col="white", ylim=c(0,20), xlim=c(0,23.5), xlab="", ylab="", yaxt="n", xaxt="n", main="", axes=F)
polygon(x=c(7.5,7.5,9.5,9.5), y=c(0,20,20,0), col=rgb(220,220,220,200, max=255), border=NA)
points(wahlrecht_df$mte, wahlrecht_df$afe, pch=1, col=rgb(170,170,170, max=255))
lines(0:49, mafe.mte, lwd=2)
axis(1, at=seq(0,60,2), labels=seq(0,60,2))
axis(2, at=seq(0,20,5), labels=seq(0,20,5))
mtext("Absolute prediction error", side=2, at=10, line = 2, outer=F)
mtext("Months to election", side=1, at=10, line = 2)


### estimate model ---------------------------

# generate partyXinstitute interaction
wahlrecht_df$partyXinstitute <- interaction(wahlrecht_df$party,wahlrecht_df$institute)

# compute model
data_sub <- filter(wahlrecht_df, mte >= 8, mte <= 9)
model_out <- lmer(voteshare ~ support + (1|party) + (1|partyXinstitute), data = data_sub)
summary(model_out)

# retrieve REs
ranef(model_out)$party
ranef(model_out)$partyXinstitute

# retrieve fitted values
wahlrecht_df$fit <- NA
model_out_fit <- augment(model_out)
wahlrecht_df$fit[as.numeric(model_out_fit$.rownames)] <- model_out_fit$.fitted


### 2017 forecast ---------------------------

# how many polls for forecasting?
length(wahlrecht_df$support[wahlrecht_df$mte >= 8 & wahlrecht_df$mte <= 9 & wahlrecht_df$election == 2017])/6

data_2017 <- filter(wahlrecht_df, mte >= 8, mte <= 9, election == 2017)
nrow(data_2017)/7

pred_2017 <- augment(model_out, newdata = data_2017)

preds_parties <- group_by(pred_2017, party) %>% summarize(forecast = mean(.fitted, na.rm = TRUE))
polls_parties <- group_by(pred_2017, party) %>% summarize(forecast = mean(support, na.rm = TRUE))
preds_parties$forecast %>% sum # afd missing!

plot(preds_parties$forecast, polls_parties$forecast, ylab = "Polling average", xlab = "Model-based forecast")
text(preds_parties$forecast, polls_parties$forecast, polls_parties$party, pos = 3)
abline(0, 1, lty = 2)
grid()

