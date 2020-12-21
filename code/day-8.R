#' Day 8 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-8.txt',
                header = F, col.names = 'input') %>% 
  mutate('action' = trimws(gsub('\\+','',gsub('-','',gsub('[0-9]+$','',input))),'both'),
         'step' = as.numeric(gsub('[a-z]+ ','',input)))

# df = data.frame('input' = 
#                   c(
#                     'nop +0',
#                     'acc +1',
#                     'jmp +4',
#                     'acc +3',
#                     'jmp -3',
#                     'acc -99',
#                     'acc +1',
#                     'jmp -4',
#                     'acc +6'
#                   )) %>% 
#   mutate('action' = trimws(gsub('\\+','',gsub('-','',gsub('[0-9]+$','',input))),'both'),
#          'step' = as.numeric(gsub('[a-z]+ ','',input)))

boot <- function(y = df){
  w = 1
  acc = 0
  while(!any(duplicated(w))){
    z = y$action[w[1]]
    if(z=='nop'){
      w = c((w[1]+1),w)
    } else if (z=='acc'){
      acc = acc + y$step[w[1]]
      w = c((w[1]+1),w)
    } else {
      w = c((w[1]+y$step[w[1]]),w)
    }
  }
  return(acc)
}
boot() # solution 1

boot <- function(y = df){
  w = 1
  acc = 0
  while(!any(duplicated(w))){
    z = y$action[w[1]]
    if(z=='nop'){
      w = c((w[1]+1),w)
    } else if (z=='acc'){
      acc = acc + y$step[w[1]]
      w = c((w[1]+1),w)
    } else {
      w = c((w[1]+y$step[w[1]]),w)
    }
    if(w>nrow(y)){
      return(TRUE)
    }
  }
  return(acc)
}
for(m in which(df$action%in%c('nop','jmp'))){
  l = df
  if(l$action[m]=='jmp'){
    l$action[m] = 'nop'
  } else {
    l$action[m] = 'jmp'
  }
  p = boot(y = l)
  if(p==T){
    print(m)
  }
}

df$action[314] = 'nop'
boot <- function(y = df){
  w = 1
  acc = 0
  while(!any(duplicated(w))){
    z = y$action[w[1]]
    if(z=='nop'){
      w = c((w[1]+1),w)
    } else if (z=='acc'){
      acc = acc + y$step[w[1]]
      w = c((w[1]+1),w)
    } else {
      w = c((w[1]+y$step[w[1]]),w)
    }
    if(w>nrow(y)){
      return(acc)
    }
  }
  return(acc)
}
boot() # solution 2
# Answers ---------------------------------------------------------------------
#' Part 1: 1810
#' Part 2: 969