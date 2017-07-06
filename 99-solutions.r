### -----------------------------
### simon munzert
### social media data
### -----------------------------

## peparations -------------------

source("packages.r")
source("functions.r")


## Retrieving odds on the next German chancellor

# largest party
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
probs_cdu <- odds.hk2prob(odds_cdu)
probs_spd <- odds.hk2prob(odds_spd)

# chancellorship
url <- "https://www.oddschecker.com/politics/european-politics/german-politics/next-chancellor"
url_parsed <- read_html(url)
odds_table <- html_table(url_parsed)[[1]]
odds_table <- slice(odds_table, 7:14)
odds_merkel <- odds_table[1,] %>% str_subset("\\d.*")
odds_schulz <- odds_table[2,] %>% str_subset("\\d.*")

# prepare odds
odds_merkel <- sapply(odds_merkel, function(x) eval(parse(text = x)))
odds_schulz <- sapply(odds_schulz, function(x) eval(parse(text = x)))

# compute probabilities
probs_merkel <- odds.hk2prob(odds_merkel)
probs_schulz <- odds.hk2prob(odds_schulz)

summary(probs_cdu)
summary(probs_merkel)

summary(probs_spd)
summary(probs_schulz)


## Retrieving Wikipedia pageviews for articles on German parties

german_parties_views <- article_pageviews(
  project = "de.wikipedia", 
  article = c("Christlich Demokratische Union Deutschlands",
              "Christlich-Soziale Union in Bayern", 
              "Sozialdemokratische Partei Deutschlands", 
              "Freie Demokratische Partei", 
              "Bündnis 90/Die Grünen", 
              "Die Linke", 
              "Alternative für Deutschland"),
  user_type = "user", 
  start = "2017010100", 
  end = "2017060100"
)
table(german_parties_views$article)

parties <- unique(german_parties_views$article)
dat <- filter(german_parties_views, article == parties[1])
plot(ymd(dat$date), dat$views, col = "black", type = "l")
dat <- filter(german_parties_views, article == parties[2])
lines(ymd(dat$date), dat$views, col = "blue")
dat <- filter(german_parties_views, article == parties[3])
lines(ymd(dat$date), dat$views, col = "red")
dat <- filter(german_parties_views, article == parties[7])
lines(ymd(dat$date), dat$views, col = "brown")
