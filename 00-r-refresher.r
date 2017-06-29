# ************************************************
### election forecasting
### simon munzert
### introduction to R
# ************************************************


source("packages.r")
source("functions.r")


# ************************************************
# WHAT'S R? --------------------------------------

# - software environment for numeric and visual data analysis
# - statistical programming language based on S
# - open source
# - works on all main platforms (Windows, OSX, Linux)
# - under continuous development
# - masses of addons ('packages') available, ~1-3 new ones every day (currently more than 10000 on CRAN)



# ************************************************
# WORKING WITH RSTUDIO ---------------------------

### a first view
# - point-and-click menu
# - many windows, many buttons

### console input

# - R is ready when the console offers '>'
# - input is incomplete if R answers with '+' (you are likely to have forgotten a ')' or ']')
1 + 2 - 3 + 
# - R is 'case sensitve'
sum(1,2)
Sum(1,2)
# the assignment operator '<-' stores something in the workspace. You can call this something again later. We can also  use '=' instead, but it is less common among R users and I do not recommend it
x <- log(10)
x
# R uses English vocabulary, therefore we have to follow the English default in using commas and dots:
3.1415
3,1415
# commas are very important when it comes to matrices and data frames: they separate row from column values
mat <- matrix(c("this", "is", "a", "matrix"), nrow = 2, byrow = TRUE)
mat[1,2]
mat[1,]
mat[,1]



# - we use # to comment in the code
# - the RStudio editor provides auto completion (use 'TAB')
# - very useful: recycle previous commands by using the arrow keys (up/down) in the console
# - CTRL+up provides a list of previous commands
# 
# list of helpful shortcuts:
# 
# CTRL+1         point cursor in editor
# CTRL+2         point cursor in console
# ESC            interrupt R
# CTRL+F         search and replace
# CTRL+SHIFT+C   comment code (and undo commenting)
# CTRL+ENTER     execute code
# CTRL+S         save document




# ************************************************
# IMPORTING PACKAGES -----------------------------

# in contrast to Stata, base R has a rather limited functionality
# when we start R, we usually have to load a bunch of packages for our analysis
# overview of packages: 
browseURL("http://cran.r-project.org/web/packages/")
# packages are installed once (usually after downloading them from CRAN)
# installing packages in RStudio is straightforward
# updating packages is straightforward, too
# we can also use the console to install packages
install.packages("dplyr")
# packages are loaded for every session
library(dplyr)




# ************************************************
# IMPORTING AND EXPORTING DATA -------------------------

# importing rectangular spreadsheet data
library(readr)
dir.create("data/spreadsheets")

readr_example("mtcars.csv")

# import and export comma-delimited files
mtcars <- read_csv(readr_example("mtcars.csv"))
head(mtcars)
write_csv(mtcars, "data/spreadsheets/mtcars-comma.csv")

# import and export semi-colon delimited files (Germans!)
write_delim(mtcars, delim = ";", path = "data/spreadsheets/mtcars-semicolon.csv")
mtcars <- read_csv2("data/spreadsheets/mtcars-semicolon.csv")
head(mtcars)

## FOR R NERDS
# why readr, not base R?
# readr is much faster (up to 10x)
# strings remain strings by default
# automatically parse common date/time formats
# progress bar if needed


# importing Stata files
library(haven)
dir.create("data/stata")

write_dta(mtcars, "data/stata/mtcars.dta")
mtcars_stata <- read_dta("data/stata/mtcars.dta")

## FOR R NERDS
# why not use functions from foreign package?
# haven works with binary files from newer Stata versions, too
# retains value/variable labels




# ************************************************
# PIPING -----------------------------------------

# what is piping?
# structures sequences of data operations as "pipes, i.e. left-to-right (as opposed to from the inside and out)
# serves the natural way of reading ("do this, then this, then this, ...")
# avoids nested function calls
# improves "cognitive performance" of code writers and readers
# minimizes the need for local variables and function definitions
# why name "magrittr"?
browseURL("https://upload.wikimedia.org/wikipedia/en/b/b9/MagrittePipe.jpg")


# traditional way of writing code
dat <- babynames 
dim(dat)
dat_filtered <- filter(dat, name == "Kim")
dat_grouped <- group_by(dat_filtered, year, sex)
dat_sum <- summarize(dat_grouped, total = sum(n))
qplot(year, total, color = sex, data = dat_sum, geom = "line") +
  ggtitle('People named "Kim"')

# traditional, even more awkward way of writing code
dat <- summarize(group_by(filter(babynames, name == "Kim"), year, sex), total = sum(n))
qplot(year, total, color = sex, data = dat, geom = "line") +  ggtitle('People named "Kim"')

# magrittr style of piping code
babynames %>%
  filter(name %>% equals("Kim")) %>%
  group_by(year, sex) %>%
  summarize(total = sum(n)) %>%
  qplot(year, total, color = sex, data = ., geom = "line") %>%
  add(ggtitle('People named "Kim"')) %>%
  print

# syntax and vocabulary
# by default, the left-hand side (LHS) will be piped in as the first argument of the function appearing on the right-hand side (RHS)
# %>% may be used in a nested fashion, e.g. it may appear in expressions within arguments. This is used in the mpg to kpl conversion
# when the LHS is needed at a position other than the first, one can use the dot,'.', as placeholder
# whenever only one argument is needed--the LHS--, the parentheses can be omitted



# ************************************************
# MANIPULATING DATA FRAMES -----------------------

# dplyr, by Hadley Wickham, provides a flexible grammar of data manipulation
# three main goals
# identify the most important data manipulation verbs and make them easy to use from R
# provide fast performance for in-memory data
# use the same interface to work with data no matter where it's stored, whether in a data frame, data table or database.

# get data from nycflights13 package
# source: [https://goo.gl/8hlrJb]
# info about the dataset:
browseURL("http://www.transtats.bts.gov/DatabaseInfo.asp?DB_ID=120&Link=0")
dat <- flights %>% as.data.frame
head(dat)

# filter observations
filter(dat, month == 1, day == 1) %>% head()
slice(dat, 1:10)

# arrange rows
arrange(dat, dep_time, arr_time) %>% View()
dat[order(dat$dep_time, dat$arr_time),] %>% head()
arrange(dat, desc(dep_time), arr_time)	 %>% head()

# select variables
select(dat, year, month, day) %>% head()
select(dat, year:day) %>% head()
select(dat, -(year:day)) %>% head()
select(dat, contains("time")) %>% head()
# also possible: starts_with("abc"), ends_with("xyz"), matches("(.)\\1"), num_range("x", 1:3)
?select_helpers

# extract unique rows
distinct(dat, tailnum) %>% head # similar to base::unique(), but faster

# rename variables
select(dat, tail_num = tailnum) %>% head
rename(dat, tail_num = tailnum) %>% head

# create variables (add new columns)
mutate(dat, gain = arr_delay - dep_delay, speed = distance / air_time * 60) %>% head
mutate(dat, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60)) %>% head # you can even refer to columns that you've created in the same call!

# create variables, only keep new ones
transmute(dat, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60)) %>% head

# summarize values; collapse data frame into single row
summarize(dat, delay_mean = (mean(dep_delay, na.rm = TRUE)))

# grouped operations with group_by()
# verbs above are useful on their own, but...
# can be applied to groups of observations within a dataset
# group_by() helps you break down your dataset into specified groups of rows
# afterwards, applying verbs from above on the grouped object, they'll be automatically applied by group
# very convenient: same syntax applies!

unique(dat$tailnum) %>% length
by_tailnum <- group_by(dat, tailnum)
class(by_tailnum)

delay <- summarize(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

# useful functions to feed summarize with
destinations <- group_by(dat, dest)
summarize(destinations,
          planes = n_distinct(tailnum), # equivalent to length(unique(x))
          flights = n()
)




# ************************************************
# DEALING WITH VECTORS -----------------------

# numeric vectors
x <- c(4,8,15,16,23,42)
x
mode(x)
length(x)
summary(x)

# character vectors
countries <- c("Germany", "France", "Netherlands", "Belgium")
countries
paste(countries, collapse = " ")
paste("Hello", "world!", sep = " ")
paste0("Hello", "world!")
c(countries, "Poland")
mode(countries)

# logical vectors
x > 15
x == sqrt(225)

# logical and relational operators
# <,>,>=,<=,==,!=, is.na(), & (logical AND), | (logical OR), ! (logical NOT)

# missing values
y <- c(1,10,NA,7,NA,11)
sum(y)
sum(y, na.rm = TRUE)
!is.na(y) # not: y == NA
y*3

# seq and rep
seq(1, 10, 3)
rep(c(1, 2, 3), 2)
rep(c(1, 2, 3), each = 2)
seq_along(x)

# sorting
vec1 <- c(2, 20, -5, 1, 200)
vec2 <- seq(1, 5)
sort(vec1, decreasing = FALSE) 
order(vec1, decreasing = FALSE)
vec2[order(vec1)]
vec3 <- c(1,10,NA,7,NA,11)
vec4 <- vec3[!is.na(vec3)]
vec4

# vectors with mixed element types are not possible
z <- c(1,2,"Bavaria", 4)
z
str(z)

# variables
zz <- c(1,2,Bavaria,4,5,6) # error
Bavaria <- 3
zz <- c(1,2,Bavaria,4,5,6)
zz
str(zz)

# transform vector type
zzchar <- as.character(zz)
zznum <- as.numeric(zzchar)

# combine vectors
xzz <- c(x,zz)

# subsetting
countries[2]
xzz[1:6] # xzz[seq(1,6)], xzz[c(1,2,3,4,5,6)]
xzz[c(2, 5, 10)]
xzz[-1]
xzz[Bavaria]
xzz[seq(0, 10, by = 2)]
xzz[c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)]
y
y[is.na(y)]
y[!is.na(y)]
y[y > 5 & !is.na(y)]

countries
countries[3] <- "Switzerland"
countries


xzz
xzz[c(1 ,3 ,5 )] <- c(100,110,120)
xzz_new <- xzz
xzz_new[xzz <= 100] <- 0
xzz_new[xzz > 100] <- 1
xzz_new
ifelse(xzz > 100, 1, 0)



# ************************************************
# BASIC DATA TIDYING -----------------------------

# clean variable names
# only lowercase letters, with _ as separator
# handles special characters and spaces 
# appends numbers to duplicated names
foo_df <- as.data.frame(matrix(ncol = 6))
names(foo_df) <- c("hIgHlo", "REPEAT VALUE", "REPEAT VALUE", "% successful (2009)",  "abc@!*", "")
foo_df
janitor::clean_names(foo_df)
make.names(names(foo_df)) # base R solution - not very convincing

# convert multiple values to NA
convert_to_NA(letters[1:5], c("b", "d"))
convert_to_NA(sample(c(1:5, 98, 99), 20, replace = TRUE), c(98,99))

# clean frequency tables
head(mtcars)
table(mtcars$cyl)
janitor::tabyl(mtcars$cyl, show_na = TRUE, sort = TRUE)
janitor::tabyl(mtcars$cyl, show_na = TRUE, sort = TRUE) %>% add_totals_row()

# clean cross tabulations
mtcars %$% table(cyl, gear)
mtcars %>% janitor::crosstab(cyl, gear)
mtcars %>% janitor::crosstab(cyl, gear) %>% adorn_crosstab(denom = "row", show_totals = TRUE)

# more functionality available; see
browseURL("https://cran.r-project.org/web/packages/janitor/vignettes/introduction.html")
browseURL("https://github.com/sfirke/janitor") # worth a look if you have to deal with messy Excel/spreadsheet data



# ************************************************
# TIDYING MODEL OUTPUT ---------------------------

# overview at
browseURL("ftp://cran.r-project.org/pub/R/web/packages/broom/vignettes/broom.html")
browseURL("ftp://cran.r-project.org/pub/R/web/packages/broom/vignettes/broom_and_dplyr.html")

## motivation
# model inputs usually have to be tidy
# model outputs less so...
# this makes dealing with model results (e.g., visualizing coefficients, comparing results across models, etc.) sometimes difficult

## example: linear model output
model_out <- lm(mpg ~ wt, mtcars) # linear relationship between miles/gallon and weight (in 1000 lbs)
model_out
summary(model_out)

# examine model object
str(model_out)
coef(summary(model_out)) # matrix of coefficients with variable terms in row names
broom::tidy(model_out)
?tidy.lm

# add fitted values and residuals to original data
broom::augment(model_out) %>% head
?augment.lm

# inspect summary statistics
broom::glance(model_out)
?glance.lm

# many supported models; see
?tidy # ... and click on "index"

# the true power of broom unfolds in settings where you want to combine results from multiple analyses (using subgroups of data, different models, bootstrap replicates of the original data frame, permutations, imputations, ...)

data(Orange)
Orange

# inspect relationship between age and circumference
cor(Orange$age, Orange$circumference) 
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line()

# using broom and dplyr together works like a charm
Orange %>% group_by(Tree) %>% summarize(correlation = cor(age, circumference))
cor.test(Orange$age, Orange$circumference)
Orange %>% group_by(Tree) %>% do(tidy(cor.test(.$age, .$circumference)))

# also works for regressions
Orange %>% group_by(Tree) %>% do(tidy(lm(age ~ circumference, data=.)))

# other examples online
browseURL("ftp://cran.r-project.org/pub/R/web/packages/broom/vignettes/kmeans.html") # k-means clustering
browseURL("ftp://cran.r-project.org/pub/R/web/packages/broom/vignettes/bootstrapping.html") # bootstrapping




# ************************************************
# SPLIT-APPLY-COMBINE WITH BASE R ----------------

# workflow:
# 1. take input (list, data frame, array)
# 2. split it (e.g., data frame into columns)
# 3. apply function to the single parts
# 4. combine it into new object
# lapply() and friends are among the best-known functionals, i.e. functions that take a function as input
# often more efficient than a for loop

# looping patters for a for loop:
# loop over elements: for (x in xs)
# loop over numeric indices: for (i in seq_along(xs))
# loop over the names: for (nm in names())

# basic patterns to use lapply():
lapply(xs, function(x) {})
lapply(seq_along(xs), function(i) {})
lapply(names(xs), function(nm) {})

# apply(): operating on matrices and arrays
a <- matrix(1:20, nrow = 5)
apply(a, 1, mean)
apply(a, 2, mean)

# lapply(): applying a function over a list or vector; returning a list
# sapply(): applying a function over a list or vector; similar to lapply() but simplifies output to produce an atomic vector

lapply(mtcars, is.numeric)
sapply(mtcars, is.numeric)


# multiple inputs: Map()
# with lapply(), only one argument varies, the others are fixed
# sometimes, you want more arguments to vary
# here, Map() comes into play

# example: computation of mean vs. weighted mean
xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)

vapply(xs, mean, numeric(1))
Map(weighted.mean, xs, ws) %>% unlist

# if some of the arguments should be fixed and constant, use an anomymous function:
Map(function(x, w) weighted.mean(x, w, na.rm = TRUE), xs, ws)

# apply function over ragged array with tapply()
dat <- data.frame(x = 1:20, y = rep(letters[1:5], each = 4))
tapply(dat$x, dat$y, sum) # data, index, function




# ************************************************
# FUNCTIONS --------------------------------------

# function that returns the mean of a vector
my_mean <- function(my_vector) {
  mean <- sum(my_vector)/length(my_vector) 
  mean
}
my_mean(c(1, 2, 3))
my_mean


# another function that finds the remainder after division ("modulo operation)"
remainder <- function(num = 10, divisor = 4) {
  remain <- num %% divisor
  remain
}
remainder()
args(remainder)

# implement conditions
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
has_name(c(1, 2, 3))
has_name(mtcars)


# when to use functions: example
# generate a sample dataset 
set.seed(1014) 
df <- data.frame(replicate(6, sample(c(1:5, -99), 6, rep = TRUE))) 
names(df) <- letters[1:6] 
df

# how to replace -99 with NA?
df$a[df$a == -99] <- NA
df$b[df$b == -99] <- NA
df$c[df$c == -98] <- NA
df$d[df$d == -99] <- NA
df$e[df$e == -99] <- NA
df$f[df$g == -99] <- NA

fix_missing <- function(x) { 
  x[x == -99] <- NA
  x 
}
# lapply is called a "functional" because it takes a function as an argument
df[] <- lapply(df, fix_missing) # littler trick to make sure we get back a data frame, not a list

# easy to generalize to a subset of columns
df[1:3] <- lapply(df[1:3], fix_missing)
df

# what if different codes for missing values are used?
fix_missing_99 <- function(x) { 
  x[x == -99] <- NA
  x 
}

fix_missing_999 <- function(x) { 
  x[x == -999] <- NA
  x 
}

# NOOO! Instead:
missing_fixer <- function(x, na.value) { 
  x[x == na.value] <- NA
  x
}

# applying multiple functions
summary_ext <- function(x) { 
  c(mean(x, na.rm = TRUE), 
    median(x, na.rm = TRUE), 
    sd(x, na.rm = TRUE), 
    mad(x, na.rm = TRUE), 
    IQR(x, na.rm = TRUE)) 
}
lapply(df, summary_ext)

# better: store functions in lists
summary_ext <- function(x) { 
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE)) 
}
sapply(df, summary_ext)

# using anonymous functions
sapply(mtcars, function(x) length(unique(x)))





# ************************************************
# EXERCISE: VECTORS ------------------------------

# create vectors
# simplify vector creation
# vector combination

# 1. Create a vector x with elements [0,4,8,12,16,20].

# 2. Create a vector y with elements [3,3,3,4,4,4,4,5,5,5,5,5].

# 3. Combine the first five elements of x mit elements 2 to 12 of vector y to create a new vector z.

# 4. What's the sum of all numbers between 1 and 100?

gauss <- function(n) (n*(n+1))/2
gauss(100)

# 5. What's the sum of all odd numbers between 1 and 100 squared?



# ************************************************
# EXERCISE: DATA FRAME MANAGMENT -----------------

# 1. Install and load the package "nycflights13"!
library(nycflights13)

# 2. How many variables and observations does the data.frame "flights" from the nycflights13 package contain?
names(flights)
View(flights)
dim(flights)
nrow(flights)
ncol(flights)

# 3. Identify the class of every column vector!
sapply(flights, class)

# 4. Select the flights that started from NYC at the first day of each month!
head(flights)
table(flights$origin)
flights_day1 <- filter(flights, day == 1)

# 5. Sort flights by time of arrival!
flights_sorted <- arrange(flights, arr_time)

# 6. Generate a sub data frame that contains only the department and arrival delay as well as the carrier!
flights3 <- select(flights, dep_delay, arr_delay, carrier)

# 7. Which carrier had the biggest department delay on average, which the lowest?
mean_delay <- tapply(flights$dep_delay, flights$carrier, mean, na.rm = TRUE)
mean_delay_sorted <- sort(mean_delay, decreasing = TRUE)



# ************************************************
# EXERCISE: FUNCTIONS ----------------------------

# 1. program a function ultimateAnswer() that always returns the number 42!

# 2. program a function normalize() that produces normalizes a numeric vector x to mean(x) = 0 and sd(x) = 1!

# 3. Use integrate and an anonymous function to find the area under the curve for the following functions:
# a) y = x ^ 2 - x, x in [0, 10]
# b) y = sin(x) + cos(x), x in [-pi, pi]


