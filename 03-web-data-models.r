### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

# get packages and functions
source("packages.r")
source("functions.r")


## getting live data from Twitter ---------------------------

# how to register at Twitter as developer, obtain and use access tokens
browseURL("https://mkearney.github.io/rtweet/articles/auth.html")

# name assigned to created app
appname <- "TwitterToR"
## api key (example below is not a real key)
load("/Users/munzerts/rkeys.RDa")
key <- TwitterToR_twitterkey
## api secret (example below is not a real key)
secret <- TwitterToR_twittersecret
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

rt <- search_tweets("merkel", n = 200, token = twitter_token)
View(rt)

# set keywords used to filter tweets
q <- paste0("schulz,merkel,btw17,btw2017")

# set up directory and JSON dump
rtweet.folder <- "data"
streamname <- "btw17"
filename <- file.path(rtweet.folder, paste0(streamname, "_", format(Sys.time(), "%F-%H-%M-%S"), ".json"))

# sink stream into JSON file
stream_tweets(q = q, parse = FALSE,
              timeout = 60,
              file_name = filename,
              language = "de",
              token = twitter_token)

# parse from json file
rt <- parse_stream(filename)

# inspect meta data
names(rt)
head(rt)

# inspect users data
users_data(rt) %>% head()
users_data(rt) %>% names()

# examine tweets
rt <- parse_stream("data/btw17_2017-07-03-13-02-52.json") # keywords: merkel|schulz, language: de
head(rt$text)
schulz <- str_detect(rt$text, regex("schulz|spd", ignore_case = TRUE))
merkel <- str_detect(rt$text, regex("merkel|cdu", ignore_case = TRUE))
mentions_df <- data.frame(schulz,merkel)
colMeans(mentions_df, na.rm = TRUE)



## getting pageviews from Wikipedia ---------------------------

# the pageviews package accesses the official Wikipedia API. However, data before July 2015 are not available using this API
# if you want to gather pageviews data before July 2015, you need the statsgrokse package. 

ls("package:pageviews")
ls("package:statsgrokse")

# Clinton vs. Trump
trump_views <- article_pageviews(project = "en.wikipedia", article = "Donald Trump", user_type = "user", start = "2015070100", end = "2017040100")
head(trump_views)
clinton_views <- article_pageviews(project = "en.wikipedia", article = "Hillary Clinton", user_type = "user", start = "2015070100", end = "2017040100")

plot(ymd(trump_views$date), trump_views$views, col = "red", type = "l")
lines(ymd(clinton_views$date), clinton_views$views, col = "blue")

# German parties
german_parties_views <- article_pageviews(
  project = "de.wikipedia", 
  article = c("Christlich Demokratische Union Deutschlands", "Christlich-Soziale Union in Bayern", "Sozialdemokratische Partei Deutschlands", "Freie Demokratische Partei", "Bündnis 90/Die Grünen", "Die Linke", "Alternative für Deutschland"),
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



## getting data from Google Trends ---------------------------

# IMPORTANT: The current gtrendsR version that is available on CRAN does not work. Install the develper version from GitHub by uncommenting the following line and running it
#devtools::install_github('PMassicotte/gtrendsR')
library(gtrendsR)
gtrends_merkel <- gtrends("Merkel", geo = c("DE"), time = "2016-12-01 2017-06-26")
gtrends_schulz <- gtrends("Schulz", geo = c("DE"), time = "2016-12-01 2017-06-26")

plot(gtrends_merkel)
plot(gtrends_schulz)

