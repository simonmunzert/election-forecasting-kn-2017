# Workshop "Election Forecasting with R"

**Code**

- `00-r-refresher.r`: introduction to the most frequently used functions and operations
- `01-fundamentals-models.r`: how to build a fundamentals-based election forecasting model, with the German Federal election as a showcase
- `02-prediction-markets.r`: how to access and process odds from betting platforms
- `03-web-data-models.r`: how to access data from platforms such as Twitter, Wikipedia, and Google Trends
- `04-scraping-wahlrecht-de.r`: script to crawl polling data from [wahlrecht.de](http://wahlrecht.de)


**Data**

The `data` folder provides a bunch of Stata and R datasets relevant to build fundamentals- and polling-based forecasting models for the German federal election. They can be imported using either Stata or R. The datasets are:

- `election_results_ltw.dta`: results of all German State elections ("Landtagswahlen") between 1946 and September 2016. Only results from the following parties are included: CDU, CSU, SPD, FDP, Greens, Left, NPD, AfD. The dataset also provides a variable `election` that reports the year of the Bundestag election following the respective state election, as well as a variable `dist` that reports the distance to the next Bundestag election in days. **Use this dataset if you want to forecast state elections or want to use state election results to predict Bundestag election results.**
- `ger_model_df.dta`: results of all German Bundestag elections since 1949, together with many more variables. The units of this dataset are election-party units, i.e. each row represents one party in one election. **Use this dataset if you want to forecast party-specific vote shares at the Bundestag election.**
- `ger_nat.dta`: results of all German Bundestag elections since 1949, together with many more variables. The units of this dataset are elections, i.e. each row represents one election. This dataset can be thought of as a replication dataset for Gschwend/Norpoth. **Use this dataset if you want to build a forecasting model based on Gschwend/Norpoth and/or if you are interested in forecasting coalition vote shares or other national-level quantities, such as turnout.**
- `polls_btw.RData`: polling results for all major parties in Germany since 1998. The units of this dataset are poll-specific party results, i.e. it's a long format where one row reports the polling result for one party in one poll. **Use this dataset if you are interested in aggregating polls.**
- `polls_btw_wide.RData`: polling results for all major parties in Germany since 1998. The units of this dataset are polls, i.e. it's a wide format where one row reports the result of one poll. **Use this dataset if you are interested in aggregating polls.**
- `polls_df_agg.RData`: polling results, aggregated at the election level. The units of this dataset are aggregated polls by election and party. Several time windows are reported, e.g., the average polling performance of party X at election X, 200-230 days before the election. **Use this dataset if you are interested in using aggregated polls in your forecasting model.**


**Instructor** 

Simon Munzert ([website](https://simonmunzert.github.io), [Twitter](https://twitter.com/simonsaysnothin))

