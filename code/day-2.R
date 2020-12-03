#' Day 2 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-2.txt',
                header = F,col.names = 'input')

ans = lapply(df$input,function(x){
  rng = regmatches(x,regexpr('^[0-9\\-]+',x))
  let = regmatches(x,regexpr('[a-z]',x))
  pwd = regmatches(x,regexpr('[a-z]+$',x))
  
  n = length(which(unlist(strsplit(pwd,split = ''))%in%let))
  return(
    ifelse((n>=(as.numeric(gsub('-[0-9]+','',rng))))&
             (n<=(as.numeric(gsub('[0-9]+-','',rng)))),
           'valid','invalid'
    )
  )
})

length(unlist(ans)[ans%in%'valid']) # number of valid passwords

ans = lapply(df$input,function(x){
  rng = regmatches(x,regexpr('^[0-9\\-]+',x))
  let = regmatches(x,regexpr('[a-z]',x))
  pwd = regmatches(x,regexpr('[a-z]+$',x))
  
  n = unlist(strsplit(pwd,split = ''))[c(as.numeric(gsub('-[0-9]+','',rng)),
                                       as.numeric(gsub('[0-9]+-','',rng)))]
  return(
    ifelse(length(n[n%in%let])==1,
           'valid','invalid'
    )
  )
})

length(unlist(ans)[ans%in%'valid']) # number of valid passwords

# Answers ---------------------------------------------------------------------
#' Part 1: 416
#' Part 2: 688