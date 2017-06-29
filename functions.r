#
char <- function(x) as.character(x)
num <- function(x) as.numeric(x)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

recode_partynames <- function(x, longnames = FALSE) {
  require(stringr)
  x_recoded <- x %>% str_replace("cdu", "Union") %>%
    str_replace("fdp", "FDP") %>% 
    str_replace("spd", "SPD") %>%
    str_replace("gru", "Grüne") %>%
    str_replace("lin", "Die Linke") %>%
    str_replace("afd", "AfD") %>%
    str_replace("oth", "Andere")
  if(longnames == TRUE) {
    x_recoded <- x_recoded %>% str_replace("Grüne", "B'90/Die Grünen") %>% str_replace("Union", "CDU/CSU") %>% str_replace("Linke", "Die Linke")
  }
  x_recoded
}

recode_years <- function(x) {
  x_recoded <- x %>% str_replace("19|20", "'")
  x_recoded
}




# function to get quantities of interest from JAGS MCMC matrix
jags_summary <- function(x) {
  dat <- data.frame(var = colnames(x),
                    mean = apply(x, 2, mean),
                    sd = apply(x, 2, sd),
                    q95lo = apply(x, 2, quantile, probs = 0.025),
                    q95hi = apply(x, 2, quantile, probs = 0.975),
                    q90lo = apply(x, 2, quantile, probs = 0.05),
                    q90hi = apply(x, 2, quantile, probs = 0.95),
                    q80lo = apply(x, 2, quantile, probs = 0.10),
                    q80hi = apply(x, 2, quantile, probs = 0.90),
                    stringsAsFactors = FALSE
  )
  dat
}


### Transform Mean and Variance from Normal Prior to Beta

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = c(alpha,beta))
}



### GGplot Functtions

# Create a laballer function


party_labeller <- function(variable,value){
  
  party_names <- list(
    'cdu'="CDU/CSU",
    'fdp'="FDP",
    'gru'="B90/Die Grünen",
    'lin'="Die Linke",
    'spd'="SPD",
    "afd"="AfD"
  )
  
  return(party_names[as.character(value)])
}

party_labeller_eng <- function(variable,value){
  
  party_names <- list(
    'cdu'="CDU/CSU",
    'fdp'="FDP",
    'gru'="A90/The Greens",
    'lin'="The Left",
    'spd'="SPD",
    "afd"="AfD"
  )
  
  return(party_names[as.character(value)])
}
