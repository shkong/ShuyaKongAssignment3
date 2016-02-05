# Shuya Kong
# Assignment 3

### Begin R Script ###

# 0.
print(paste("Shuya Kong","1505077","shkong@ucsc.edu", sep=","))

# 1.
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)

class(df.ex)


# 2.
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )

# number of obs that remain
print(nrow(df.ex.2))

# number of obs for summer 2013
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month ==8 | month ==9)
  )

print(nrow(df.ex.2))


# 3.
df.ex.3a <- arrange(df.ex, year,month)


# 4.
# select columns year through age
df.ex.4a <- select(df.ex, year:age)

# selec columns year, month, and those starting with i
df.ex.4b <- select(df.ex, year, month, starts_with("i"))

# distinc values of state
print(distinct(select(df.ex,state)))


# 5.
stndz <- function(x){
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

rw.stndz <- stndz(df.ex$rw)
rw_nrmlz <- nrmlz(df.ex$rw)

# create a data frame
df.ex.5a <- df.ex %>%
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw) 
  ) %>%
  select(rw.stndz,rw_nrmlz)


# another data frame
df.ex.5b <- df.ex %>%
  group_by(year,month) %>%
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count = n()
  ) %>%
  select(year,month,rw.stndz,rw_nrmlz,count)



# 6.

# summarize
df.ex.6 <- df.ex %>%
  group_by(year,month,state) %>%
  summarise(
    min_rw = min(rw, na.rm=T),
    q1_rw = quantile(rw, .25, na.rm=T),
    avg_rw = mean(rw, na.rm=T),
    med_rw = median(rw, na.rm=T),
    q3_rw = quantile(rw, .75, na.rm=T),
    max_rw = max(rw, na.rm=T),
    count = n()
  )


# find the highest mean real wage combination
df.ex.6b <- df.ex.6 %>%
  select(year,month,state,avg_rw) %>%
  arrange(desc(avg_rw)) %>%
  head(1)

# year month state corresponding to the highest mean real wage
print(
  paste(
  df.ex.6b$year,
  df.ex.6b$month, 
  df.ex.6b$state, 
  sep = ","
  )
)

# bonus question
# 7.
str(df.ex$state)

df.ex.7a <- df.ex %>%
  arrange(year, month, desc(state))
