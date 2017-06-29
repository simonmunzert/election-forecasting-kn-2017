### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

# get packages and functions
source("packages.r")
source("functions.r")


### Gschwend's and Norpoth's chancellor model ----------------

# import historical data
ger_df <- read_dta("data/ger_nat.dta")
names(ger_df)
head(ger_df)

# inspect relationships between variables
plot(ger_df$gov_share_l3, ger_df$gov_share, pch = 20, ylim = c(30, 60))
abline(lm(gov_share ~ gov_share_l3, data = ger_df))
text(ger_df$gov_share_l3, ger_df$gov_share, labels = ger_df$year, pos = 3)

plot(ger_df$chancellor_polls, ger_df$gov_share, pch = 20, ylim = c(30, 65))
abline(lm(gov_share ~ chancellor_polls, data = ger_df))
text(ger_df$chancellor_polls, ger_df$gov_share, labels = ger_df$year, pos = 3)

plot(ger_df$term, ger_df$gov_share, pch = 20, ylim = c(30, 65))
abline(lm(gov_share ~ term, data = ger_df))
text(ger_df$term, ger_df$gov_share, labels = ger_df$year, pos = 2)

# run model
model_out <- lm(gov_share ~ gov_share_l3 + chancellor_polls + term, data = ger_df)
summary(model_out)

# evaluate fit
model_out_fit <- broom::augment(model_out)
head(model_out_fit)
model_out_fit$year <- ger_df$year[2:18]

plot(model_out_fit$year, model_out_fit$gov_share, type = "o", xaxt = "n")
axis(1, model_out_fit$year, model_out_fit$year)
points(model_out_fit$year, model_out_fit$.fitted, col = "red", pch = 20)

# 2013 forecast, without and with prediction intervals
augment(model_out, newdata = filter(ger_df, year == 2013))
(predict_conf <- predict(model_out, filter(ger_df, year == 2013), se.fit = TRUE, interval = "confidence"))
(predict_pred <- predict(model_out, filter(ger_df, year == 2013), se.fit = TRUE, interval = "prediction"))
# true value: CDU/CSU 41.5, FDP 4.8 --> 46.3

# manual computation of standard error used for prediction interval
se_pred <- sqrt(predict_pred$se.fit^2+sum((model_out$residuals^2 /model_out$df.residual))) # see http://stats.stackexchange.com/questions/154247/what-are-the-formulae-used-in-r-by-predict-lm-when-interval-a-none-b-pred
(conf_interval <- predict_pred$fit[1] + qt(0.025, predict_pred$df) * c(1,-1) * predict_pred$se.fit)
(pred_interval <- predict_pred$fit[1] + qt(0.025, predict_pred$df) * c(1,-1) * se_pred)



