#' Day 5 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-5.txt',
                header = F, col.names = 'input')

ans = lapply(df$input, function(x){
  rw = 0:127
  cl = 0:7
  for(ins in unlist(strsplit(x,split = ''))){
    if(ins%in%'F'){
      rw = min(rw):trunc(median(rw),0)
    }
    if(ins%in%"B"){
      rw = (trunc(median(rw),0)+1):max(rw)
    }  
    
    if(ins%in%'L'){
      cl = min(cl):trunc(median(cl),0)
    }
    if(ins%in%"R"){
      cl = (trunc(median(cl),0)+1):max(cl)
    } 
  }
  return((rw*8)+cl)
})

max(unlist(ans)) # solution 1

ans = lapply(df$input, function(x){
  rw = 0:127
  cl = 0:7
  for(ins in unlist(strsplit(x,split = ''))){
    if(ins%in%'F'){
      rw = min(rw):trunc(median(rw),0)
    }
    if(ins%in%"B"){
      rw = (trunc(median(rw),0)+1):max(rw)
    }  
    
    if(ins%in%'L'){
      cl = min(cl):trunc(median(cl),0)
    }
    if(ins%in%"R"){
      cl = (trunc(median(cl),0)+1):max(cl)
    } 
  }
  return(c(rw))
})
# Which rws are incomplete?
for(i in 0:127){
  st = ans[ans%in%i]
  if (length(st)!=8){
    print(i)
  }
}
# rw 73 is incomplete
ans = lapply(df$input, function(x){
  rw = 0:127
  cl = 0:7
  for(ins in unlist(strsplit(x,split = ''))){
    if(ins%in%'F'){
      rw = min(rw):trunc(median(rw),0)
    }
    if(ins%in%"B"){
      rw = (trunc(median(rw),0)+1):max(rw)
    }  
    
    if(ins%in%'L'){
      cl = min(cl):trunc(median(cl),0)
    }
    if(ins%in%"R"){
      cl = (trunc(median(cl),0)+1):max(cl)
    } 
  }
  if(rw%in%73){
    return(c(cl))  
  }
  
})

sort(unique(unlist(ans))) # missing cl 3

# Solution 2
(73*8)+3

# Answers ---------------------------------------------------------------------
#' Part 1: 970
#' Part 2: 587