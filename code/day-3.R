#' Day 3 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-3.txt',
                header = F)

tbgn = function(d=df,dn=1,rt=3){
  loc = c(1,1) # start location 1,1
  typ = vector()
  x = y = 0
  while(x<nrow(d)){
    x = (dn+loc[1])
    y = (rt+loc[2])
    if(y>31){y = y-31}
    typ = c(typ,unlist(strsplit(d[x,],''))[y])
    loc = c(x,y)
  }
 return(length(typ[typ%in%'#']))
}

tbgn() # solution 1

prod(tbgn(rt=1,dn=1),
     tbgn(rt=3,dn=1),
     tbgn(rt=5,dn=1),
     tbgn(rt=7,dn=1),
     tbgn(rt=1,dn=2)) # solution 2

# Answers ---------------------------------------------------------------------
#' Part 1: 265
#' Part 2: 3154761400