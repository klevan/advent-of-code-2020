#' Day 7 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-7.txt',
                header = F, col.names = 'input') %>%
  mutate(
    container = gsub(' [bags]+ contain [a-z 0-9\\,\\.]+$','',input),
    inside = gsub('[a-z ]+ contain ','',input)
  )

colors <- function(y=df,target = 'shiny gold'){
  color = vector() # collects valid colors
  new = target
  
  while(length(new)>0){
    res = lapply(new, function(x){
      w = which(grepl(x,y$inside))
    })
    new = y$container[unique(unlist(res))]
    color = c(color,new)
  }
  
  return(length(unique(color)))
}
colors() # solution 1

colors <- function(y=df,target = 'shiny gold'){
  color = vector() # collects valid colors
  new = target
  
  while(length(new)>0){
    res = lapply(new, function(x){
      w = which(grepl(x,y$container))
    })
    bg = gsub('[ bags\\.]+$','',
               unlist(strsplit(y$inside[unlist(res)],split=', ')))
    bg = bg[!bg %in% 'no other']
    new = unlist(lapply(bg,function(x){
      nm = as.numeric(unlist(strsplit(x,split=' ')))
      nm = nm[!is.na(nm)]
      x = gsub('^[0-9]+ ','',x)
      return(rep(x,nm))
    }))
    color = c(color,new)
  }
  
  return(length(color))
}
colors() # solution 2

# Answers ---------------------------------------------------------------------
#' Part 1: 205
#' Part 2: 80902