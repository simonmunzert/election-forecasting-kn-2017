# ************************************************
### election forecasting
### simon munzert
### data preparation and descriptives
# ************************************************


source("packages.r")
source("functions.r")



### prepare data --------------------------------------------

# import historical data
load("./data/election_dates.RData")
election_date
election_year
ger_df <- read_dta("./data/ger_nat.dta")

# corrections of government variables
ger_df$spd_gov[ger_df$year == 1965] <- 1 
ger_df$fdp_gov[ger_df$year == 1980] <- 1
ger_df$spd_gov[ger_df$year == 2005] <- 1

# add vote shares for others
ger_df$npd_share <- NULL
ger_df$npd_seats <- NULL
ger_df <- mutate(ger_df, oth_share = 100 - cdsu_share - spd_share - fdp_share - gru_share - lin_share - afd_share)  

# make df long
ger_df_long <- select(ger_df, ends_with("_share"), year) %>% 
  select(-contains("gov_share")) %>%
  gather(key = "party", value = "voteshare")
voteshare_vars <- str_subset(names(ger_df), "^(?!.*gov).*_share$")
gov_vars <- str_subset(names(ger_df), "_gov$")
ger_df_long <- melt(ger_df, id.vars = c("year", "turnout", "chancellor", "unemp", "chancellor_polls", "term", gov_vars) , measure.vars = voteshare_vars, value.name = "voteshare")
ger_df_long <- rename(ger_df_long, party = variable)

# gen party id
ger_df_long$party <- str_extract(ger_df_long$party, "[:alpha:]+")
ger_df_long$party <- str_replace(ger_df_long$party, "cdsu", "cdu")

# gen election id
elections_df <- data.frame(year = unique(ger_df_long$year), election = seq_along(unique(ger_df_long$year)))
ger_df_long <- merge(ger_df_long, elections_df, by = c("year"), all.x = TRUE)

# gen lag vars
lag(ger_df_long$voteshare, 1)

ger_df_long <- group_by(ger_df_long, party) %>% arrange(year) %>% mutate(voteshare_l1 = lag(voteshare, 1), voteshare_l2 = lag(voteshare, 2), voteshare_l3 = lag(voteshare, 3), chancellor = lag(chancellor, 1), unemp_l1 = lag(unemp, 1))

# generate swing variable
ger_df_long$swing <- ger_df_long$voteshare - ger_df_long$voteshare_l1
ger_df_long <- group_by(ger_df_long, party) %>% arrange(year) %>% mutate(swing_l1 = lag(swing, 1))

# gen chancellor party indicator
ger_df_long$chancellor_party <- ifelse(ger_df_long$party == ger_df_long$chancellor, 1, 0)

# gen major party indicator
ger_df_long$major <- ifelse(ger_df_long$party == "spd" | ger_df_long$party == "cdu", 1, 0)

# gen government-opposition indicator
ger_df_long <- group_by(ger_df_long, party) %>% arrange(year) %>% mutate(cdsu_gov = lag(cdsu_gov, 1), spd_gov = lag(spd_gov, 1), fdp_gov = lag(fdp_gov, 1), gru_gov = lag(gru_gov, 1))
ger_df_long <- ungroup(ger_df_long)
ger_df_long <- mutate(ger_df_long, gov = ifelse(party == "cdu" & cdsu_gov == 1, 1,
                                                ifelse(party == "spd" & spd_gov == 1, 1,
                                                       ifelse(party == "fdp" & fdp_gov == 1, 1,
                                                              ifelse(party == "gru" & gru_gov == 1, 1, 0)))))

# gen parliament indicator
ger_df_long$parl <- ifelse(ger_df_long$voteshare_l1 >= 5, 1, 0)
ger_df_long$parl[ger_df_long$party ==  "lin" & ger_df_long$year == 1994] <- 1
ger_df_long$parl[ger_df_long$party ==  "lin" & ger_df_long$year == 1998] <- 1
ger_df_long$parl[ger_df_long$party ==  "lin" & ger_df_long$year == 2002] <- 1
ger_df_long$parl[ger_df_long$party ==  "lin" & ger_df_long$year == 2005] <- 1

# gen unemp swing indicator
ger_df_long$unemp_swing <- ger_df_long$unemp - ger_df_long$unemp_l1 

# add ltw swing indicators
ltw_df <- read_dta("./data/election_results_ltw_agg.dta")
ger_df_long <- merge(ger_df_long, ltw_df, by.x = c("year", "party"), by.y = c("election", "party"), all.x = TRUE)

# add polling indicators
load("./data/polls_df_agg.RData")
ger_df_long <- merge(ger_df_long, polls_df_agg, by.x = c("year", "party"), by.y = c("election", "party"), all.x = TRUE)
ger_df_long$polls_70_100[is.na(ger_df_long$polls_70_100)] <- ger_df_long$polls_1_30[is.na(ger_df_long$polls_70_100)] # ad hoc replacement of missing polling data with later polling data
ger_df_long$polls_100_130[is.na(ger_df_long$polls_100_130)] <- ger_df_long$polls_70_100[is.na(ger_df_long$polls_100_130)] # ad hoc replacement of missing polling data with later polling data
ger_df_long$polls_170_200[is.na(ger_df_long$polls_170_200)] <- ger_df_long$polls_100_130[is.na(ger_df_long$polls_170_200)] # ad hoc replacement of missing polling data with later polling data
ger_df_long$polls_200_230[is.na(ger_df_long$polls_200_230)] <- ger_df_long$polls_170_200[is.na(ger_df_long$polls_200_230)] # ad hoc replacement of missing polling data with later polling data
ger_df_long$polls_200_230_swing <- ger_df_long$polls_200_230 - ger_df_long$voteshare_l1

# gen long-term partisanship variable
ger_df_long$voteshare_l1_3 <- rowMeans(select(ger_df_long, voteshare_l1, voteshare_l2, voteshare_l3), na.rm = TRUE)

# drop cases where voteshare variable is 0 (= party effectively does not run)
ger_df_long <- filter(ger_df_long, voteshare != 0 | is.na(voteshare))

# export modified data frame
write_dta(ger_df_long, path = "./data/ger_model_df.dta")
save(ger_df_long, file = "./data/ger_model_df.RData")



### descriptives --------------------------------------------

# generate summary table
ger_df_sub <- select(ger_df_long, voteshare, voteshare_l1, chancellor_party, polls_200_230)
summary(ger_df_sub)
stargazer(ger_df_sub, title = "Summary statistics", type = "latex", out = "figures/sumstats.tex")
stargazer(ger_df_sub, title = "Summary statistics", type = "html", out = "figures/sumstats.html")

# stargazer cheatsheet
browseURL("http://jakeruss.com/cheatsheets/stargazer.html")

# generate contingency table
table(ger_df_long$party)
table(ger_df_long$gov, ger_df_long$party)
tab <- table(ger_df_long$gov, ger_df_long$party)
rownames(tab) <- c("Not in government", "In government")
colnames(tab) <- recode_partynames(colnames(tab))
tab <- tab[,c(2, 7, 3, 4, 5, 1, 6)]

xtab <- xtable(tab, align = c("l", "r","r","r", "r", "r", "r", "r"), digits = 0, caption = "Government status, by party")
print(xtab, booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!",  include.colnames = TRUE, include.rownames = TRUE, floating.environment = "table*", file = paste0("figures/tab-party-gov.tex"))
print(xtab, booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!",  include.colnames = TRUE, include.rownames = TRUE, floating.environment = "table*", type = "html", file = paste0("figures/tab-party-gov.html"))


# generate plot of bivariate relationship (makes most sense for continuous variables)
plot(ger_df_sub)
plot(ger_df_long$voteshare_l1, ger_df_long$voteshare)

# make it nicer 
pdf(file="figures/pred_past_voteshare.pdf", height=7, width=7, family="URWTimes")
par(oma=c(0,0.5,0.5,0.5))
par(mar=c(4, 4, 3, 0))
par(pty="s")
# past vote share
plot(ger_df_long$voteshare_l1, ger_df_long$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", xlim = c(0, 50), ylim = c(0, 50))
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 25, "Lag vote share (%)", line = 1, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(2, 25, "Vote share (%)", line = 1, tick = F)
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
dev.off()


# make it nicer 
pdf(file="figures/pred_past_chancellor.pdf", height=7, width=7, family="URWTimes")
par(oma=c(0,0.5,0.5,0.5))
par(mar=c(4, 4, 3, 0))
par(pty="s")

dat <- filter(ger_df_long, major == 1)
dat$chancellor_party_lab <- ifelse(dat$chancellor_party == 0, "no chancellor party", "chancellor party")
plot(dat$chancellor_party,  dat$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", xlim = c(-.5, 1.5), ylim = c(20, 52))
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(1, seq(0, 1, 1), c("No chancellor party", "Chancellor party"), tick = F)
axis(2, 35, "Vote share (%)", line = 1, tick = F)

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
dev.off()


