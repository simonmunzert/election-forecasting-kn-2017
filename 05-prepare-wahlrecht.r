### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

source("packages.r")
source("functions.r")
 

### import data ----------------------------------------------
load("data/election_dates.RData")
load("data/wahlrecht_df.RData")


# add election year
election_date <- ymd(election_date)
wahlrecht_df$date <- ymd(wahlrecht_df$date)
wahlrecht_df$election <- NA
election_periods <- data.frame(first = lag(election_date), last = election_date, year = election_year)
election_periods[1,1] <- ymd("1948-01-01")

 for (i in 13:length(election_date)){
   for(j in 1:length(wahlrecht_df$election)) {
       wahlrecht_df$election[j] <- ifelse(between(wahlrecht_df$date[j], election_periods$first[i], election_periods$last[i]), election_periods$year[i], wahlrecht_df$election[j])
     }
 }

# clean up institute and party variable
wahlrecht_df$institute <- wahlrecht_df$institute %>% str_replace("politbarometer", "fgruppe_wahlen") %>% str_replace("dimap", "infratest_dimap")
wahlrecht_df$party <- recode(wahlrecht_df$party, `AfD` = "afd", 
                       `CDU/CSU` = "cdu",  
                       `FDP` = "fdp", 
                       `LINKE` = "lin", 
                       `FW` = "oth", 
                       `PIRATEN` = "oth", 
                       `Rechte` = "oth", 
                       `REP` = "oth", 
                       `REP/DVU` = "oth", 
                       `Sonstige` = "oth", 
                       `SPD` = "spd"
)
wahlrecht_df$party <- str_replace(wahlrecht_df$party, "GR.+", "gru")
wahlrecht_df <- filter(wahlrecht_df, party != "oth")
wahlrecht_df_wide <- dcast(wahlrecht_df, election + sample.size + institute + date + actual.votes ~ party, value.var = "forecast", mean, na.rm = TRUE)
wahlrecht_df_wide <- wahlrecht_df_wide %>% mutate(oth = 100 - rowSums(.[,c("afd", "cdu", "fdp", "gru", "lin", "spd")], na.rm = TRUE))
wahlrecht_df_long <- melt(wahlrecht_df_wide, measure.vars = c("cdu", "spd", "fdp", "gru", "lin", "afd", "oth"), variable.name = "party", value.name = "support")
wahlrecht_df <- select(wahlrecht_df_long, election = election, sample_size = sample.size, institute = institute, date = date, party = party, support = support)
wahlrecht_df <- filter(wahlrecht_df, !(party == "oth" & support > 50)) # drop unrealistic polls
                      

# generate days to election variable
election_dates_df <- data.frame(election = str_extract(election_date, "[[:digit:]]{4}"), election_date = election_date)
wahlrecht_df <- merge(wahlrecht_df, election_dates_df, by = "election", all.x = TRUE)
wahlrecht_df$days_to_election <- wahlrecht_df$election_date - wahlrecht_df$date

# delete duplicates
wahlrecht_df <- wahlrecht_df[select(wahlrecht_df, institute, date, party) %>% duplicated %>% not,]

# delete observations with no date given
wahlrecht_df <- filter(wahlrecht_df, !is.na(date))

# delete observations with "sonstige" or "infas" as institute
wahlrecht_df <- filter(wahlrecht_df, institute != "sonstige", institute != "infas")
# delete sparse time series in emnid and forsa data
wahlrecht_df <- filter(wahlrecht_df, !(wahlrecht_df$institute == "emnid" & wahlrecht_df$date < "1998-04-03"))
wahlrecht_df <- filter(wahlrecht_df, !(wahlrecht_df$institute == "forsa" & wahlrecht_df$date < "1993-02-18"))



### export data ---------------------------------------------

# export long dataset
save(wahlrecht_df, file = "data/polls_btw.RData")

# export long dataset
wahlrecht_df_wide <- dcast(wahlrecht_df, institute + date + ... ~ party, value.var = "support") 
save(wahlrecht_df_wide, file = "data/polls_btw_wide.RData")

