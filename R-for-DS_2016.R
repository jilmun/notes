# Notes for R for Data Science
# http://r4ds.had.co.nz/

# GENERAL -----------------------------------------------------------------

dput(mtcars)  # generate R code to recreate data

# DATA VISUALIZATION ------------------------------------------------------

library(ggplot2)
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy))

### Exercises 3.2.1
ggplot(data=mpg)
?mpg
ggplot(data=mpg) + geom_point(mapping=aes(x=cyl, y=hwy))
ggplot(data=mpg) + geom_point(mapping=aes(x=drv, y=class))

# try different aesthetics
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, size=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, alpha=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, shape=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy), color="blue")

### Exercises 3.3.1
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color="blue"))  # color is mapped
str(mpg); names(mpg)[!sapply(mpg, is.numeric)]  # some numeric fields are categorical
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, size=cty))  # color|size OK
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color=displ))
?geom_point
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color=displ<5))  # cool!

# facets
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_wrap(~class, nrow=2)  # facet plot on one variable
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(.~class)  
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv~class)  # facet plot on two variables

### Exercises 3.5.1
###.1
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(.~cty)  
###.2
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv~cyl)  
ggplot(data=mpg) + geom_point(mapping=aes(x=drv, y=cyl))  # no data with those combos
###.3
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv~.)  # n rows by 1 column
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(.~cyl)  # 1 row by n columns
###.5
?facet_wrap
###.6
?facet_grid  # easier to compare values across

# try different geoms
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy, group=drv))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy, colour=drv), show.legend=FALSE)  
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy, linetype=drv))  # add legend
ggplot(data=mpg) + 
  geom_point(mapping=aes(x=displ, y=hwy)) +
  geom_smooth(mapping=aes(x=displ, y=hwy))  # loess
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +  # define global aes mapping
  geom_point() + geom_smooth()
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) + 
  geom_point(mapping=aes(color=class)) +
  geom_smooth(data=dplyr::filter(mpg, class=="subcompact"), se=FALSE)

# statistical transformations
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut))
ggplot(data=diamonds) + 
  # .._.. changes stat mapping to another transformed data, 
  # see computed variables in `?stat_count`
  geom_bar(mapping=aes(x=cut, y=..prop.., group=1))  

### Exercises 3.7.1
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, y=..prop..))  
?geom_bar

# position adjustments
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, colour=cut))  # border only
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=cut))
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=clarity))  # stacked
ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) + 
  geom_bar(alpha=1/5, position="identity")  # overlaid, transparent
ggplot(data=diamonds, mapping=aes(x=cut, colour=clarity)) + 
  geom_bar(fill=NA, position="identity")  # overlaid, no fill
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=clarity), position="fill")
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=clarity), position="dodge")
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy), position="jitter")

# coordinate systems
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) + geom_boxplot()
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) + geom_boxplot() + coord_flip()
library(maps); nz <- map_data("nz")
ggplot(nz, aes(long, lat, group=group)) + 
  geom_polygon(fill="white", colour="black")
ggplot(nz, aes(long, lat, group=group)) + 
  geom_polygon(fill="white", colour="black") +
  coord_quickmap()  # sets aspect ratio for maps

# DATA TRANSFORMATION -----------------------------------------------------

library(nycflights13)
library(dplyr)

# filter rows: excludes FALSE and NA values
filter(flights, month==1, day==1)
filter(flights, month==11 | month==12)
df <- data.frame(x=c(F,T,F), y=c(T,F,T))
filter(df,cumany(x))  # all rows on and after first TRUE
filter(df,cumall(y))  # all rows before first TRUE

# arrange rows: NA sorted at the end in ascending and descending
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))  # descending order

# select columns
select(flights, year, month, day)
# starts_with("abc")
# ends_with("xyz")
# contains("ijk")
# matches("(.)\\1")  # can use regular expression
# num_range("x", 1:3)  # matches x1, x2, x3
rename(flights, tail_num=tailnum)  # special case of select
select(flights, time_hour, air_time, everything())  # helpful to shift a few columns

# add new variables: can refer to columns just created
flights_sml <- flights %>% 
  select(year:day, 
         ends_with("delay"), 
         distance, 
         air_time )
mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours )
# transmute only keeps newly created columns
transmute(flights_sml, 
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours )
# x - lag(x)  # compute running differences
# x != lag(x)  # find when values change
x <- 1:10
lag(x)
lead(x)
# rolling aggregates: cumsum, cumprod, cummin, cummax, cummean
(y <- c(1, 2, 2, NA, 3, 4))
data.frame(row_number(y),
           min_rank(y),
           dense_rank(y),
           percent_rank(y),
           cume_dist(y)) %>% knitr::kable()

# grouped summaries
summarise(flights, delay=mean(dep_delay, na.rm=TRUE))
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay=mean(dep_delay, na.rm=TRUE))
delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm=TRUE),
    delay = mean(arr_delay, na.rm=TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# missing values
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
  
# counts
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay=mean(arr_delay))
ggplot(data=delays, mapping=aes(x=delay)) +
  geom_freqpoly(binwidth=10)
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay=mean(arr_delay, na.rm=TRUE), n=n())
# variation decreases as group size is larger, should exclude groups with few obs
ggplot(data=delays, mapping=aes(x=n, y=delay)) + geom_point()

# grouped mutates
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 2)  # worst 2 members of each group
popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)

# EXPLORATORY DATA ANALYSIS -----------------------------------------------

library(tidyverse)

# what type of variation occurs within variables
# what type of covariation occurs between variables

# count by variable
diamonds %>% count(cut)  # categorical var
diamonds %>% count(cut_width(carat, 0.5))  # continuous var, remember to zoom in too

# what values are most common, why?
# what values are rare, why?
# unusual patterns? check and explain/drop outliers

# outliers and missing values
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))                        # remove values
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y<3 | y>20, NA, y))            # replace outliers with NA
ggplot(data=diamonds2, mapping=aes(x=x, y=y)) + 
  geom_point()                                     # warns NA removed
ggplot(data=diamonds2, mapping=aes(x=x, y=y)) + 
  geom_point(na.rm=T)                              # suppress NA warning

# visualizing outliers and NAs
ggplot(data=diamonds, mapping=aes(x=cut, y=price)) + geom_boxplot()
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) + geom_boxplot()

# reorder class in x-axis by median of hwy
ggplot(data=mpg, mapping=aes(x=reorder(class,hwy,FUN=median), y=hwy)) + geom_boxplot()

# two categorical variables
ggplot(data=diamonds) + geom_count(mapping=aes(x=cut, y=color))
count(diamonds, color, cut)
diamonds %>% 
  count(color, cut) %>%
  ggplot(mapping=aes(x=color, y=cut)) +
    geom_tile(mapping=aes(fill=n))

# two continuous variables
ggplot(data=diamonds) + 
  geom_point(mapping=aes(x=carat, y=price))
ggplot(data=diamonds) + 
  geom_point(mapping=aes(x=carat, y=price), alpha=1/100)

# plot residual relationships
library(modelr)
mod <- lm(log(price) ~ log(carat), data=diamonds)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>%
  mutate(resid = exp(resid))
ggplot(data=diamonds2) +
  geom_point(mapping=aes(x=carat, y=resid))
# price v. cut, once carat effect is removed
ggplot(data=diamonds2) +
  geom_boxplot(mapping=aes(x=cut, y=resid))

# recommended further reading
# https://amzn.com/331924275X

# TIBBLES -----------------------------------------------------------------

library(tidyverse)
as_tibble(iris)

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)

# transposed tibble
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# print options
nycflights13::flights %>% 
  print(n = 10, width = Inf)
# options(dplyr.print_max = Inf)
# options(tibble.width = Inf)
package?tibble

# TIDY DATA ---------------------------------------------------------------

library(tidyverse)

# gather (uses dplyr select notation)
table4a
tidy4a <- table4a %>% gather(`1999`, `2000`, key="year", value="cases")
table4b
tidy4b <- table4b %>% gather(`1999`, `2000`, key="year", value="population")
left_join(tidy4a, tidy4b)

# spread
table2
spread(table2, key=type, value=count)

# separate, unite
table3
table3 %>% separate(rate, into=c("cases", "population"))
table3 %>% separate(rate, into=c("cases", "population"), convert=TRUE)
table5
table5 %>% unite(new, century, year)
table5 %>% unite(new, century, year, sep="")

# missing values
stocks <- data.frame(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks  # change implicit missing to explicit NA
stocks %>% spread(year, return)
stocks %>% 
  spread(year, return) %>% 
  gather(`2015`, `2016`, key=year, value=return)  # na.rm=TRUE
stocks %>% complete(year, qtr)  # finds all combos and fills NAs

# fill
treatment <- data.frame(
  person = c("Derrick Whitmore", NA, NA, "Katherine Burke"),
  treatment = c(1, 2, 3, 1),
  response = c(7, 10, 9, 4)
)
treatment
treatment %>% fill(person)  # fills NA with prior non-empty value

# RELATIONAL DATA ---------------------------------------------------------

library(tidyverse)
library(nycflights13)

# explore individual tables
airlines
airports
planes
weather

# check primary keys for uniqueness
planes %>%
  count(tailnum) %>%
  filter(n > 1)
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

# mutating joins
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
flights2 %>% 
  select(-origin, -dest) %>%
  left_join(airlines, by="carrier")
flights2 %>%
  left_join(airports, c("dest"="faa"))

# filtering joins
top_dest <- flights %>%  # top 10 destinations
  count(dest, sort=TRUE) %>%
  head(10)  
top_dest
flights %>%  # flights that went to top 10 destinations
  filter(dest %in% top_dest$dest)
flights %>%  # keep only matches
  semi_join(top_dest)
# semi_join can be useful to merge back summary stats
# anti_join can be useful to diagnose mismatches

# set operations
x1 <- c(1,2,3,5,7)
y1 <- c(1,3,5,6,9)
intersect(x1, y1)  # in both
union(x1, y1)      # in either
setdiff(x1, y1)    # in x1 but not in y1

# STRINGS -----------------------------------------------------------------

library(tidyverse)
library(stringr)

x <- c("\"", "\\")
x 
writeLines(x)  # raw contents
?'"'  # shows list of special characters

str_length(c("a", "R for data science", NA))
str_c("x", "y")
str_c("x", "y", sep=", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

# regular expressions
sum(str_detect(words, "^t"))  # number of common words start with t
mean(str_detect(words, "[aeiou]$"))  # percent of common words ending with a vowel
words[str_detect(words, "x$")]
str_subset(words, "x$")
x <- c("apple", "banana", "pear")
str_count(x, "a")
mean(str_count(words, "[aeiou]"))  # vowels per word average
str_count("abababa", "aba")  # count does not overlap

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse="|")
colour_match
has_colour <- str_subset(sentences, colour_match)
head(has_colour)
matches <- str_extract(has_colour, colour_match)  # only extracts first match
head(matches)
more <- sentences[str_count(sentences, colour_match) > 1]  # sentences w more than 1 color
more
str_extract(more, colour_match)
str_extract_all(more, colour_match)
str_extract_all(more, colour_match, simplify = TRUE)  # expands column count to longest row
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

# group matching
noun <- "(a|the) ([^ ]+)"  # after "a" or "the" and not a space
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>%
  str_extract(noun)
has_noun %>%
  str_match(noun)  # column for group match and each individual match
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",  # name column matching
    remove = FALSE
  )

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")  # only replaces first match
str_replace_all(x, "[aeiou]", "-")
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%  # flips 2rd and 3rd words
  head(5)

sentences %>%
  head(5) %>% 
  str_split(" ")

# FACTORS -----------------------------------------------------------------

library(forcats)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

x1 <- c("Dec", "Apr", "Jan", "Mar")
y1 <- factor(x1, levels=month_levels)
y1

gss_cat %>% count(race)

relig <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
relig
ggplot(relig, aes(tvhours, relig)) + geom_point()
ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

# group small groups together
gss_cat %>%  # agg is still smallest
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
gss_cat %>%  # set number of groups
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

# ITERATION ---------------------------------------------------------------

library(tidyverse)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))
models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)
models %>%  # even more condensed 
  map(summary) %>%
  map_dbl("r.squared")

x <- list(1, 10, "a")
x
y <- x %>% map(safely(log))
str(y)
str(transpose(y))
y <- transpose(y)
is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]  # which produced error
y$result[is_ok] %>% flatten_dbl()
# possibly() is similar to safely(), diff is you provide default if fail
# quietly() captures printed output, messages, warnings instead of errors

iris %>% 
  keep(is.factor) %>% 
  str()
iris %>% 
  discard(is.factor) %>% 
  str()

x <- sample(10)
x %>%
  detect(~ . >5)        # returns first match
x %>%
  detect_index(~ . >5)  # returns index of first match

# MODEL BASICS ------------------------------------------------------------

library(tidyverse)
library(modelr)
options(na.action = na.warn)  # warn when dropping NA rows

# look at simulated numbers
ggplot(sim1, aes(x, y)) + geom_point()

sim1_mod <- lm(y~x, data=sim1)

# takes data frame, find all unique var combos
grid <- sim1 %>% data_grid(x)
grid <- grid %>% add_predictions(sim1_mod)
grid

sim1 <- sim1 %>% add_residuals(sim1_mod)

# look at residuals
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth=0.5)
ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h=0) +
  geom_point()

# example with model matrix
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
); df
model_matrix(df, y ~ x1)
model_matrix(df, y ~ x1 - 1)  # drop intercept
model_matrix(df, y ~ x1 + x2)
df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
); df
model_matrix(df, response ~ sex)

# categorical variables
ggplot(sim2) + geom_point(aes(x,y))
mod2 <- lm(y ~ x, data=sim2)
grid <- sim2 %>% 
  data_grid(x) %>%
  add_predictions(mod2)
grid
ggplot(sim2, aes(x)) +
  geom_point(aes(y=y)) +
  geom_point(data=grid, aes(y=pred), colour="red", size=4)

# interactions: categorical with continuous
sim3
ggplot(sim3, aes(x1,y)) + geom_point(aes(colour=x2))
mod1 <- lm(y ~ x1 + x2, data=sim3)
mod2 <- lm(y ~ x1 * x2, data=sim3)
grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
grid
ggplot(sim3, aes(x1, y, colour=x2)) +
  geom_point() +
  geom_line(data=grid, aes(y=pred)) +
  facet_wrap(~ model)
sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)
ggplot(sim3, aes(x1, resid, colour=x2)) +
  geom_point() +
  facet_grid(model ~ x2)

# interactions: continuous with continuous
mod1 <- lm(y ~ x1 + x2, data=sim4)
mod2 <- lm(y ~ x1 * x2, data=sim4)
grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid
ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill=pred)) +
  facet_wrap(~ model)
ggplot(grid, aes(x1, pred, colour=x2, group=x2)) +
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour=x1, group=x1)) +
  geom_line() +
  facet_wrap(~ model)

# MODEL BUILDING ----------------------------------------------------------

# understand general relationship
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

# look at confounder 'carat'
library(hexbin)
ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins=50)

diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice=log2(price), lcarat=log2(carat))

# relationship is linear after transformation
ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins=50)

# remove linear relationship
mod_diamond <- lm(lprice ~ lcarat, data=diamonds2)

grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2^lprice)
grid
ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins=50) +
  geom_line(data=grid, colour="red", size=1)

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins=50)

# replace original plots with residuals
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

# more complicated model
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data=diamonds2)
grid <- diamonds2 %>%
  data_grid(cut, .model=mod_diamond2) %>%
  add_predictions(mod_diamond2)
grid
diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, "lresid2")
ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins=50)

# MANY MODELS -------------------------------------------------------------

library(modelr)
library(tidyverse)

library(gapminder)
gapminder

gapminder %>%
  ggplot(aes(year, lifeExp, group=country)) +
  geom_line(alpha=1/3)

# single country exploration
nz <- filter(gapminder, country=="New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")
nz_mod <- lm(lifeExp ~ year, data=nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")
nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept=0, colour="white", size=3) +
  geom_line() +
  ggtitle("Remaining pattern")

# applying process to multiple data splits
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
by_country  # nested data
country_model <- function(df) {
  lm(lifeExp ~ year, data=df)
}
models <- map(by_country$data, country_model)
# better yet, add models into nested data frame
by_country <- by_country %>%  
  mutate(model = map(data, country_model))
by_country
by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country
resids <- unnest(by_country, resids)  # turn back into regular data frame
resids
resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group=country), alpha=1/3) +
  geom_smooth(se=FALSE)
resids %>%
  ggplot(aes(year, resid, group=country)) +
  geom_line(alpha=1/3) +
  facet_wrap(~continent)

# using the broom package
library(broom)
broom::glance(nz_mod)  # makes one row
by_country %>%
  mutate(glance=map(model, broom::glance)) %>%
  unnest(glance)  # not quite what we want
glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)
# broom::glance(model)
# broom::tidy(model)
# broom::augment(model, data)

# more about nesting
gapminder %>% 
  nest(year:gdpPercap)

# MARKDOWN ----------------------------------------------------------------

comma <- function(x) format(x, digits = 2, big.mark = ",")
comma(123456)
comma(.124545)
comma(3.124545)
comma(13.124545)
comma(1123.124545)

# GRAPHICS FOR COMMUNICATION ----------------------------------------------

# fig.width = 6    # set width to 6''
# fig.asp = 0.618  # set aspect ratio = golder ratio
# out.width = "70%"  # percent of line width
# fig.align = "center"

# need to balance fig.width and out.width to keep font sizes the same
#   if fig.width was 6, out.width was 0.7, 
#   changing out.width to 0.5 will require fig.width = 6 x 0.5 / 0.7
