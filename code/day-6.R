#' Day 6 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-6.txt',
                header = F, col.names = 'input',blank.lines.skip = F)

brk = c(which(df$input%in%''),nrow(df)) # break points between passports
b = c(1,which(df$input%in%''))

ans = lapply(brk, function(x){
  return(
    length(unique(unlist(strsplit(df$input[(b[which(brk%in%x)]):x],split = ''))))
  )
})

sum(unlist(ans)) # solution 1

ans = lapply(brk, function(x){
  vals = strsplit(df$input[(b[which(brk%in%x)]):x],split = '')
  
  v = unique(unlist(vals))
  for(i in 1:length(vals)){
    if(length(unlist(vals[i]))>0){
      v = v[v%in%unlist(vals[i])]
    }
  }
  
  return(
    length(unique(v))
  )
})

sum(unlist(ans)) # solution 2

# Answers ---------------------------------------------------------------------
#' Part 1: 6625
#' Part 2: 3360