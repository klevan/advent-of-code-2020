#' Day 9 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-9.txt',
                header = F, col.names = 'input') %>% 
  mutate('note'=NA)
df$note[1:25] = 'preamble'

elig = df$input[25:1]
for(i in 26:nrow(df)){
  pos = elig[elig<df$input[i]]
  pos = pos[pos%in%(df$input[i]-pos)]
  pos = pos[(pos!=(df$input[i]-pos))]
  if(length(pos)>0){
    df$note[i] = 'valid'
  } else {
    df$note[i] = 'invalid'
  }
  elig = df$input[i:(i-24)]
}

df$input[which(df$note%in%'invalid')] # solution 1

df = df[1:(which(df$note%in%'invalid')),]
df$note = ''
df$note[df$input>df$input[nrow(df)]] = 'no'
df$note[nrow(df)] = 'no'

target = 105950735

df = df[1:(which(df$note%in%'no')-1),]
