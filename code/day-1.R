#' Day 1 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-1.txt',
                   header = F,col.names = 'input') %>% 
  mutate('diff' = 2020- input)

# Find the couplet
prod(df[df$input%in%df$diff,1]) # solution 1

# Find the triplet
trip = lapply(df$diff,
              function(x){df$input[(x - df$input)%in%df$input]}
              )

prod(unique(unlist(trip))) # solution 2

# Answers ---------------------------------------------------------------------
#' Part 1: 658899
#' Part 2: 155806250