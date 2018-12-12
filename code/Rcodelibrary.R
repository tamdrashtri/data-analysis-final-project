# SETUP-------

# this package helps to install and load packages at the same time.
install.packages("simpleSetup")
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive", "nycflights13",
              "gmodels", "stringr")
library_install(packages)

# # Set valid working directory
# possible_wd <- c('/examples/directory1', '/anotherExample/directory2')
# set_valid_wd(possible_wd)

# General -----------------------------------------------------------------

# store value in a variable
sample.100 <- rnorm(100, mean = mean.f, sd = sd.f)


# summarising and explore data ------



# using skim to see summaries of data
skim(iris) %>% summary()
skim(iris) %>% skimr::kable()
skim(iris) %>% pander()
mydata <- group_by(iris$Sepal.Length) %>%
  skim()

# use DataExplorer package to visualise data
# https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html 
# https://datascienceplus.com/blazing-fast-eda-in-r-with-dataexplorer/ 
merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))

# some basic functions to explore
introduce(final_data)
plot_intro(final_data)
plot_missing(final_data)
plot_bar(final_data)
plot_histogram(final_data)
create_report(final_data)


# String replace ----------------------------------------------------------


fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", toupper)
str_replace_all(fruits, "b", NA_character_)

str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace(fruits, c("a", "e", "i"), "-")


# Confidence Interval -----------------------------------------------------------------------

CI.f <- c((mean.f - 2 * se.f), (mean.f + 2 * se.f))


# sort data ---------------------------------------------------------------

var(fem[fem > 4])
var(fem[fem >= 4])



# Base R visualisation ----------------------------------------------------

# box plot 
# http://www.sthda.com/english/wiki/box-plots-r-base-graphs 

# histogram
hist(mammals$logmass, breaks = seq(0, 10, 2))

# multiple histogram
# https://rdrr.io/cran/GGally/man/ggally_facethist.html 
ggally_facethist(mammals, mapping = ggplot2::aes(x = logmass, y = status))

ggally_facethist(mammals, mapping = ggplot2::aes(x = logmass, y = status), binwidth = 0.05)



# table -------------------------------------------------------------------

mammals <- read_csv("mammals.csv", 
                    na = "")

table(mammals$continent) 

table(mammals$status)

#two way frequency
table(mammals$continent,mammals$status)

# group dplyr
# https://datacarpentry.org/R-genomics/04-dplyr.html
mammals %>% 
  group_by(status) %>% 
  summarise(median_size = median(logmass, na.rm = TRUE))


# manipulation ------------------------------------------------------------

## subset -------

x <- VADeaths[1:3, "Rural Male"]

## transform log ----
mammals$logmass <- log(mammals$mass.grams, base = 10)


# 6. Iteration - for loop, map and apply -------------------------------------

# https://stackoverflow.com/questions/45101045/why-use-purrrmap-instead-of-lapply

map(list, 2)
# is same as
map(list, function(x) x[[2]])

## some more examples
# NOT RUN {
1:10 %>%
  map(rnorm, n = 10) %>%
  map_dbl(mean)

# Or use an anonymous function
1:10 %>%
  map(function(x) rnorm(10, x))

# Or a formula
1:10 %>%
  map(~ rnorm(10, .x))

df <- tibble(
 a = rnorm(10),
 b = rnorm(10),
 c = rnorm(10),
 d = rnorm(10)
)

# return double
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

# short cuts
# instead of
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

# use this:
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

# instead of 
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

# use this:
models %>% 
  map(summary) %>% 
  map_dbl("r.squared")


# 6b. purrr vs base -------------------------------------------------------
# https://jennybc.github.io/purrr-tutorial/bk01_base-functions.html 

# map(YOUR_LIST, YOUR_FUNCTION)

library(purrr)
library(repurrrsive)

lapply(got_chars[1:3],
       function(x) x[["name"]])

# can also use
map(got_chars[1:3], "name")

# v apply
vapply(got_chars[1:3],
       function(x) x[["name"]],
       character(1))

map_chr(got_chars[1:3], "name")


# 7. statistical tests handy -------------------------------------------------

# need this dataset from the problem set to run
bird <- read_csv("BirdWindowCrash.csv")
View(bird)

# calculate proportion, calculate the sum and summarise them
proportionDeathBird <- bird %>% 
  group_by(angleDuringBirdCrash) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n),
         expectedDeath = 1/3)

# for shortcuts, use CrossTable is more handy
CrossTable(proportionDeathBird$angleDuringBirdCrash, proportionDeathBird$pct, expected = TRUE, prop.t = TRUE)  
# same thing, but CrossTable constructs the table for us. 
chisq.test(proportionDeathBird$angleDuringBirdCrash, proportionDeathBird$pct)


# 7b. different ways using inference --------------------------------------

dog <- read_csv("PrairieDogMultipleMating.csv")

# make a contigency table and get the result
CrossTable(dog$matingFrequency, dog$gaveBirth, expected = TRUE)

# also same as this:
obs_chisq <- dog %>%
  specify(gaveBirth ~ matingFrequency) %>% # alt: response = origin, explanatory = season
  calculate(stat = "Chisq") 


