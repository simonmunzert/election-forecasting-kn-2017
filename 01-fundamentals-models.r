### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

# get packages and functions
source("packages.r")
source("functions.r")


### ZWEITSTIMME.ORG's FUNDAMENTAL MODEL

### prepare data ---------------------

# import historical data
load("data/ger_model_df.RData")
names(ger_df_long)
table(ger_df_long$party)
View(ger_df_long)

# create election id
election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)
head(election_years_df)


## plot relationship between response and predictors --------------

# past vote share
plot(ger_df_long$voteshare_l1, ger_df_long$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(a)", xlim = c(0, 50), ylim = c(0, 50))
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 25, "previous vote share (%)", line = 1, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(2, 25, "vote share (%)", line = 1, tick = F)
# run model, add regression line
model_out <- lm(voteshare ~ voteshare_l1 - 1, data = ger_df_long)
model_out_aug <- augment(model_out)
model_out_aug$case_label <- paste(ger_df_long$party, ger_df_long$year, sep = " ") %>% recode_partynames(longnames = FALSE) %>% .[model_out_aug$.rownames %>% num()] %>% recode_years
abline(model_out, lty = 2)
# identify important outliers
obs_id <- abs(model_out_aug$.std.resid) > 1.53
points(model_out_aug$voteshare_l1[obs_id], model_out_aug$voteshare[obs_id], pch = 20)
# plot labels of outliers based on resid or cooksd 
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
text(model_out_aug$voteshare_l1[obs_id], model_out_aug$voteshare[obs_id], label = model_out_aug$case_label[obs_id], cex = .7, pos = label_position[obs_id], offset = .47)
grid()

# polls
plot(ger_df_long$polls_200_230, ger_df_long$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(b)", xlim = c(0, 60), ylim = c(0, 60))
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 30, "polls, \n200-230 days before election", line = 2, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(2, 30, "vote share (%)", line = 1, tick = F)
# run model, add regression line
model_out <- lm(voteshare ~ polls_200_230 - 1, data = ger_df_long)
model_out_aug <- augment(model_out)
model_out_aug$case_label <- paste(ger_df_long$party, ger_df_long$year, sep = " ") %>% recode_partynames(longnames = FALSE) %>% .[model_out_aug$.rownames %>% num()] %>% recode_years
abline(model_out, lty = 2)
# identify important outliers
obs_id <- abs(model_out_aug$.std.resid) > 1.53
points(model_out_aug$polls_200_230[obs_id], model_out_aug$voteshare[obs_id], pch = 20)
# plot labels of outliers based on resid or cooksd 
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
text(model_out_aug$polls_200_230[obs_id], model_out_aug$voteshare[obs_id], label = model_out_aug$case_label[obs_id], cex = .7, pos = label_position[obs_id], offset = .47)
grid()

# chancellor party
dat <- filter(ger_df_long, major == 1)
dat$chancellor_party_lab <- ifelse(dat$chancellor_party == 0, "no chancellor party", "chancellor party")
plot(dat$chancellor_party,  dat$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(c)", xlim = c(-.5, 1.5), ylim = c(20, 52))
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(1, seq(0, 1, 1), c("no chancellor party", "chancellor party"), tick = F)
axis(2, 35, "vote share (%)", line = 1, tick = F)

# run model, add regression line
model_out <- lm(voteshare ~ chancellor_party, data = dat)
model_out_aug <- augment(model_out)
model_out_aug$case_label <- paste(dat$party, dat$year, sep = " ") %>% recode_partynames(longnames = FALSE) %>% .[model_out_aug$.rownames %>% num()] %>% recode_years
abline(model_out, lty = 2)
# identify important outliers
model_out_aug <- group_by(model_out_aug, chancellor_party) %>% arrange(.std.resid) %>% mutate(label_position = c(4, rep(c(2, 4), 8))) %>% ungroup
obs_id <- abs(model_out_aug$.std.resid) > 1.3
points(model_out_aug$chancellor_party[obs_id], model_out_aug$voteshare[obs_id], pch = 20)
# plot labels of outliers based on resid or cooksd 
text(model_out_aug$chancellor_party[obs_id], model_out_aug$voteshare[obs_id], label = model_out_aug$case_label[obs_id], cex = .7, pos = model_out_aug$label_position[obs_id], offset = .47)
grid()




### run models on full sample ---------------------------------------------
model_out <- lm(voteshare ~ chancellor_party + voteshare_l1 + polls_200_230, data = ger_df_long)
summary(model_out)

# evaluate fit
model_out_fit <- augment(model_out)
model_out_fit$party <- ger_df_long$party[as.numeric(model_out_fit$.rownames)]
model_out_fit$year <- ger_df_long$year[as.numeric(model_out_fit$.rownames)]
mean(abs(model_out_fit$.resid))
group_by(model_out_fit, party) %>% summarize(mae = mean(abs(.resid)))

plot(model_out_fit$.fitted, model_out_fit$voteshare, cex = .5, pch = 20)
text(model_out_fit$.fitted, model_out_fit$voteshare, paste0(model_out_fit$party, str_sub(as.character(model_out_fit$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)



### 2017 forecast, without and with prediction intervals-------------------

dat_2017 <- filter(ger_df_long, year == 2017)
rownames(dat_2017) <-  dat_2017$party

augment(model_out, newdata = dat_2017)
(predict_conf <- predict(model_out, dat_2017, se.fit = TRUE, interval = "confidence"))
(predict_pred <- predict(model_out, dat_2017, se.fit = TRUE, interval = "prediction"))

# manual computation of standard error used for prediction interval
se_pred <- sqrt(predict_pred$se.fit^2+sum((model_out$residuals^2 / model_out$df.residual))) # see http://stats.stackexchange.com/questions/154247/what-are-the-formulae-used-in-r-by-predict-lm-when-interval-a-none-b-pred

conf_fit <- data.frame(fit = predict_pred$fit[,1],
                        lwr = predict_pred$fit[,1] + qt(0.025, predict_pred$df) * predict_pred$se.fit,
                        upr = predict_pred$fit[,1] - qt(0.025, predict_pred$df) * predict_pred$se.fit
                        )
conf_fit

pred_fit <- data.frame(fit = predict_pred$fit[,1],
                       lwr = predict_pred$fit[,1] + qt(0.025, predict_pred$df) * se_pred,
                       upr = predict_pred$fit[,1] - qt(0.025, predict_pred$df) * se_pred
)
pred_fit

# plot forecast
pred_fit$party <-  dat_2017$party
preds_df <- arrange(pred_fit, fit)
preds_df$partyrank <- seq_along(preds_df$party)
preds_df$partyname <- recode_partynames(preds_df$party)

par(mar=c(3.5,7,0,3)+.1)
par(oma=c(0,0,0,0)+.1)
plot(preds_df$fit, preds_df$partyrank, ylim = c(0.5, 7.5), xlim = c(0, 45), xaxt = "n", yaxt = "n", ylab = "", xlab = "", pch = 20)
axis(1, seq(0, 45, 5), seq(0, 45, 5))
axis(1, mean(c(0, 45)), "Forecasted vote share (%)", line = 1, tick = F)
axis(2, preds_df$partyrank, labels = preds_df$partyname, las = 1, tick = F)
axis(4, preds_df$partyrank, labels = paste0(format(preds_df$fit, digits = 2, trim = TRUE),  "%")
     , line = 1.5, tick = F,las = 2, hadj = 1)

abline(v = seq(0, 45, 5), col = "darkgrey", lty = 2)
for (i in preds_df$partyrank){
  lines(x=c(preds_df$lwr[i],preds_df$upr[i]), y=c(i,i), lwd = 1)
}




### run all possible models -----------------------------------------
ger_df_long$chancellor_partyXunemp_swing <- ger_df_long$chancellor_party*ger_df_long$unemp_swing
ger_df_long$chancellor_partyXvoteshare_l1 <- ger_df_long$chancellor_party*ger_df_long$voteshare_l1

# define set of predictors and outcome
d <- select(ger_df_long, voteshare, chancellor_party, voteshare_l1, voteshare_l1_3, polls_200_230, major, gov, parl, chancellor_partyXunemp_swing, chancellor_partyXvoteshare_l1)
dep_var <- 'voteshare'
indep_vars <- setdiff(names(d), dep_var)

# run all possible models
lms <- Reduce(append, lapply(seq_along(indep_vars),
                             function(num_vars) {
                               Reduce(append, apply(combn(length(indep_vars), num_vars), 2, function(vars) {
                                 formula_string <- paste(c(dep_var, paste(indep_vars[vars], collapse = "+")), collapse = '~')
                                 structure(list(lm(as.formula(formula_string), data = d)), .Names = formula_string)
                               }))
                             }
))
length(lms)

# get overview of model performance
sum_tab <- data.frame(model_name = names(lms), 
                      num_vars = sapply(lms, function(x) { x %>% .$coefficients %>% length}) - 1,
                      #df = sapply(lms, function(x) { summary(x) %>% .$df[2,]}),
                      r_squared = sapply(lms, function(x) { summary(x) %>% .$r.squared}),
                      adj_r_squared = sapply(lms, function(x) { summary(x) %>% .$adj.r.squared})
)
sum_tab$ratio <- sum_tab$r_squared / sum_tab$num_vars

# create multiple predictions for 2017 
i = 2017
lms_best <- lms[sum_tab$r_squared > .35]
lms_best_predictions <- sapply(lms_best, predict.lm, newdata = filter(ger_df_long, year == i)) %>% t() %>% as.data.frame() 
lms_best_predictions <- apply(lms_best_predictions, 1, add, filter(ger_df_long, year == i)$voteshare_l1) %>% t() %>% as.data.frame
names(lms_best_predictions) <- filter(ger_df_long, year == i)$party
summary(lms_best_predictions)
lms_best_predictions$vote_sums <- rowSums(lms_best_predictions)

# summarize predictions
names(lms_best_predictions) <- filter(ger_df_long, year == i)$party
summary(lms_best_predictions)

# check if predictions are (more or less) logically consistent
lms_best_predictions$vote_sums <- rowSums(lms_best_predictions)
summary(lms_best_predictions$vote_sums)
hist(lms_best_predictions$vote_sums)



### out-of-sample checks --------------------------------------------------

# prepare formula
vars <- c("voteshare_l1", "chancellor_party", "polls_200_230")
fmla <- as.formula(paste("voteshare ~ ", paste(vars, collapse= "+")))

# run out-of-sample predictions
model_out <- list()
model_pred <- list()
for(i in seq_along(election_years)) {
  insample <- filter(ger_df_long, year != election_years[i])
  outsample <- filter(ger_df_long, year == election_years[i])
  model_out[[i]] <- lm(fmla, data = insample)
  model_pred[[i]] <- augment(model_out[[i]], newdata = outsample, type.predict = "response")
}

# evaluate fit
model_pred_df <- do.call(rbind, model_pred)
mean(abs(model_pred_df$voteshare - model_pred_df$.fitted), na.rm = TRUE)
group_by(model_pred_df, party) %>% summarize(mae = mean(abs(voteshare - .fitted), na.rm = TRUE))
plot(model_pred_df$.fitted, model_pred_df$voteshare, cex = .5, pch = 20)
text(model_pred_df$.fitted, model_pred_df$voteshare, paste0(model_pred_df$party, str_sub(as.character(model_pred_df$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)



### arriving at event probabilities / simulate predictions ---------------------

model_out <- lm(voteshare ~ chancellor_party + voteshare_l1 + polls_200_230, data = ger_df_long)
voteshare_pred <- predict(model_out, filter(ger_df_long, year == 2017), se.fit = TRUE, interval = "prediction")

voteshare_pred_sim <- replicate(1000, rnorm(rep(1, length(voteshare_pred$fit[,"fit"])), mean = voteshare_pred$fit[,"fit"], sd = sqrt(voteshare_pred$se.fit^2+var(model_out$residuals)))) %>% t() %>% as.data.frame

names(voteshare_pred_sim) <- filter(ger_df_long, year == 2017)$party %>% as.character
plot(density(voteshare_pred_sim$cdu), xlim = c(20, 50))
lines(density(voteshare_pred_sim$spd), col = "red")

prop.table(table(voteshare_pred_sim$cdu > voteshare_pred_sim$spd))
prop.table(table(voteshare_pred_sim$fdp < 5))
prop.table(table(voteshare_pred_sim$gru < 5))


