### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

# get packages and functions
source("packages.r")
source("functions.r")


# retrieve odds for German federal election
url <- "https://www.oddschecker.com/politics/european-politics/german-politics/german-federal-election"
url_parsed <- read_html(url)
odds_table <- html_table(url_parsed)[[1]]
odds_table <- slice(odds_table, 7:14)
odds_cdu <- odds_table[1,] %>% str_subset("\\d.*")
odds_spd <- odds_table[2,] %>% str_subset("\\d.*")

# prepare odds
odds_cdu <- sapply(odds_cdu, function(x) eval(parse(text = x)))
odds_spd <- sapply(odds_spd, function(x) eval(parse(text = x)))

# compute probabilities
browseURL("http://www.wikihow.com/Read-Odds")
browseURL("https://www.pinnacle.com/en/betting-resources/betting-tools/conversion-calculator#height")
probs_cdu <- odds.hk2prob(odds_cdu)
probs_spd <- odds.hk2prob(odds_spd)

# correction for favorite longshot bias
# Leigh et al. 2007 suggest the following transformation:
# Pr = cum_norm(1.64*inv_cum_norm(price))
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), xlab = "market-implied probability", ylab = "true probability")
abline(0, 1, lty = 2)
curve(pnorm(1.64*qnorm(x)), add = TRUE)
grid()

# correct chancellor probabilities
debiasOdds <- function(x) {pnorm(1.64*qnorm(x))}
debiasOdds(probs_spd)
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), xlab = "market-implied probability", ylab = "true probability")
abline(0, 1, lty = 2)
points(probs_spd, debiasOdds(probs_spd), col = "red")
points(probs_cdu, debiasOdds(probs_cdu), col = "black")
curve(pnorm(1.64*qnorm(x)), add = TRUE)
grid()



#######################
### IT'S YOUR SHOT! ###
#######################

# oddschecker.com provides another market on who will become the next chancellor. Look up odds for Merkel vs. Schulz and compute the market-implied probabilities! Are they equivalent to the probabilities of who will become the largest party in parliament? Why? Why not?





