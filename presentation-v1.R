

## Git repo
## https://github.com/adamleerich/pa-seminar-2022


## 3/41 ----------------------------------------------------------------------------------------------------
library(tidyverse)



## 7/41 ----------------------------------------------------------------------------------------------------
dracula <- readr::read_file('dracula.txt')
dracula_words <- strsplit(dracula, split = '\\s+')


## 8/41 ----------------------------------------------------------------------------------------------------
words <- unlist(dracula_words)
words_lower <- tolower(words)
nchar_words <- nchar(words_lower)
nchar_words10 <- pmin(nchar_words, 10)
table_words10 <- table(nchar_words10)
barplot(table_words10, names.arg = c(1:9, '10+'))


## 9/41 ----------------------------------------------------------------------------------------------------
unlist(dracula_words)
tolower(unlist(dracula_words))
nchar(tolower(unlist(dracula_words)))
pmin(nchar(tolower(unlist(dracula_words))), 10)
table(pmin(nchar(tolower(unlist(dracula_words))), 10))

# Fully nested!
barplot(table(pmin(
  nchar(tolower(unlist(dracula_words))), 10)),
  names.arg = c(1:9, '10+'))


## 10/41 ---------------------------------------------------------------------------------------------------
dracula_words %>% 
  unlist() %>% 
  tolower() %>% 
  nchar() %>% 
  pmin(10) %>% 
  table() %>% 
  barplot(names.arg = c(1:9, '10+'))


## 11/41 ---------------------------------------------------------------------------------------------------
## install.packages(
##   c('xts', 'sp'),
##   type = 'binary')
## 
## install.packages(
##   'CASdatasets',
##   repos = 'http://cas.uqam.ca/pub/')

library(CASdatasets)


## 12/41 ---------------------------------------------------------------------------------------------------
data(brvehins1a, package = 'CASdatasets')
brvehins1a <- as_tibble(brvehins1a)
print(brvehins1a)


## 13/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  select(ExposTotal, PremTotal)


## 14/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>%
  select(-Gender, -ClaimNbFire)

brvehins1a %>% 
  select(-DriverAge)


## 15/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>%
  pull(PremTotal)

brvehins1a %>% 
  pull(PremTotal, Gender) %>% 
  head


## 17/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>%
  select(contains('Veh'))

brvehins1a %>%
  select(ends_with('Total'))

brvehins1a %>%
  select(starts_with('claim'))

brvehins1a %>%
  select(matches('claim.*coll'))


## 19/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  filter(Gender == 'Male')

brvehins1a %>%
  filter(
    PremTotal > 250e3,
    VehYear == 2011,
    Gender != 'Corporate')


## 20/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  filter(
    PremTotal > 250e3,
    VehYear == 2011,
    !str_starts(Gender, 'C|c'))


## 21/41 ---------------------------------------------------------------------------------------------------
 brvehins1a %>%
   drop_na()

brvehins1a %>%
  drop_na(Gender, DrivAge, contains('total'))


## 22/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>%
  select(ExposTotal, PremTotal) %>% 
  slice(10:11)


## 23/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>%
  select(ExposTotal, PremTotal) %>% 
  mutate(
    PremPerExpos = PremTotal / ExposTotal,
    PremPerExpos_e3 = PremPerExpos / 1000)


## 24/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  select(DrivAge, State) %>% 
  rename(
    DriverAge = DrivAge,
    Province = State) %>% 
  filter(Province == 'Sao Paulo')


## 25/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  group_by(State) %>% 
  summarize(
    PremiumPerState = sum(PremTotal),
    ExposuresPerState = sum(ExposTotal)) %>% 
  mutate(
    AvgPremPerExposure = PremiumPerState / ExposuresPerState)


## 26/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  group_by(State) %>% 
  summarize(
    StateCount = n())


## 27/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  group_by(Gender) %>% 
  summarize(
    Premium = sum(PremTotal),
    Exposures = sum(ExposTotal))


## 28/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  group_by(Gender) %>% 
  summarize(
    Premium = sum(PremTotal),
    Exposures = sum(ExposTotal)) %>% 
  mutate(Gender = coalesce(Gender, 'UNKNOWN'))


## 29/41 ---------------------------------------------------------------------------------------------------
brvehins1a %>% 
  group_by(Gender) %>% 
  summarize(
    Premium = sum(PremTotal),
    Exposures = sum(ExposTotal)) %>% 
  mutate(Gender = coalesce(Gender, 'UNKNOWN')) %>% 
  arrange(desc(Exposures))


## 30/41 ---------------------------------------------------------------------------------------------------
data(
  brvehins1b, 
  brvehins1c, 
  brvehins1d,
  brvehins1e,
  package = 'CASdatasets')

brvehins1 <- brvehins1a %>% 
  bind_rows(brvehins1b, brvehins1c, brvehins1d, brvehins1e)

dim(brvehins1)


## 32/41 ---------------------------------------------------------------------------------------------------
pop <- read.csv('brazil-states.csv') %>% as_tibble
print(pop)
sum(pop$PopThousands) / 1000


## 33/41 ---------------------------------------------------------------------------------------------------
count_by_state <- brvehins1 %>% 
  group_by(State) %>% 
  summarize(Count = n())

count_by_state %>% 
  inner_join(pop)


## 34/41 ---------------------------------------------------------------------------------------------------
count_by_state %>% 
  inner_join(pop, by = 'State')


## 35/41 ---------------------------------------------------------------------------------------------------
count_by_prov <- brvehins1 %>% 
  group_by(State) %>% 
  summarize(Count = n()) %>% 
  rename(Province = State)

count_by_prov %>% 
  inner_join(pop, by = c('Province' = 'State'))


## 36/41 ---------------------------------------------------------------------------------------------------
count_by_state %>% 
  anti_join(pop)

pop %>% 
  anti_join(count_by_state)


## 37/41 ---------------------------------------------------------------------------------------------------
count_by_prov %>% 
  left_join(pop, by = c('Province' = 'State')) %>% 
  filter(str_starts(Province, 'S'))

count_by_prov %>% 
  right_join(pop, by = c('Province' = 'State')) %>% 
  filter(str_starts(Province, 'S'))


## 38/41 ---------------------------------------------------------------------------------------------------
count_by_state %>% 
  full_join(pop, by = 'State') %>% 
  filter(str_starts(State, 'S'))


## 39/41 ---------------------------------------------------------------------------------------------------
tall_data <- brvehins1 %>% 
  group_by(Gender, State) %>% 
  summarize(PremTotal = sum(PremTotal)) %>% 
  drop_na()

tall_data


## 40/41 ---------------------------------------------------------------------------------------------------
wide_data <- tall_data %>% 
  pivot_wider(
    id_cols = 'State',
    names_from = 'Gender',
    values_from = 'PremTotal') %>% 
  mutate(
    TotalPrem = Corporate + Female + Male,
    CorporateProp = Corporate / TotalPrem) %>% 
  arrange(desc(CorporateProp))

wide_data


## 41/41 ---------------------------------------------------------------------------------------------------
wide_data %>% 
  pivot_longer(
    cols = -c('State'),
    names_to = 'Key', 
    values_to = 'Value') %>% 
  filter(
    State == 'Amazonas')

