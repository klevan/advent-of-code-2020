#' Day 4 Code Challenge -------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)

df = read.delim('~/GitHub/advent-of-code-2020/inputs/day-4.txt',
                header = F, col.names = 'input',blank.lines.skip = F)

# Required fields
fld = c('byr', # (Birth Year)
        'iyr', # (Issue Year)
        'eyr', # (Expiration Year)
        'hgt', # (Height)
        'hcl', # (Hair Color)
        'ecl', # (Eye Color)
        'pid'  # (Passport ID)
        ) 

brk = c(which(df$input%in%''),
        nrow(df)) # break points between passports

i = 1
n = 0
for (b in brk){
  x = df$input[i:b]
  y = unlist(strsplit(x,split = ' '))
  y = gsub(':[0-9A-z\\#]+','',y)
  y = unique(y[y%in%fld])
  if(length(y)==7){
    n = n+1
  }
  i=b
}

n # solution 1

i = 1
n = 0
for (b in brk){
  x = df$input[i:b]
  y = unlist(strsplit(x,split = ' '))
  z = gsub(':[0-9A-z\\#]+','',y)
  z = unique(z[z%in%fld])
  if(length(z)==7){
    # check validdation
    byr = as.numeric(gsub('byr:','',y[grepl('byr',y)]))
    byr = (byr>1919)&(byr<2003)&grepl('^[0-9]{4}$',byr)
    
    iyr = as.numeric(gsub('iyr:','',y[grepl('iyr',y)]))
    iyr = (iyr>2009)&(iyr<2021)&grepl('^[0-9]{4}$',iyr)
      
    
    eyr = as.numeric(gsub('eyr:','',y[grepl('eyr',y)]))
    eyr = (eyr>2019)&(eyr<2031)&grepl('^[0-9]{4}$',eyr)
    
    hgt = gsub('hgt:','',y[grepl('hgt',y)])
    if(grepl('cm',hgt)){
      hgt = as.numeric(gsub('cm','',hgt))
      hgt = ifelse(hgt>149&hgt<194,T,F)
    } else if(grepl('in',hgt)){
      hgt = as.numeric(gsub('in','',hgt))
      hgt = ifelse(hgt>58&hgt<77,T,F)
    } else {hgt=F}
    
    hcl = gsub('hcl:','',y[grepl('hcl',y)])
    hcl = grepl('^#[0-9a-f]{6}$',hcl)
    
    ecl = gsub('ecl:','',y[grepl('ecl',y)])
    ecl = ecl%in%c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
    
    pid = gsub('pid:','',y[grepl('pid',y)])
    pid = grepl('^[0-9]{9}$',pid)
    
    if(byr&iyr&eyr&hgt&hcl&ecl&pid){
      n = n+1  
    }
    
  }
  i=b
}

n # solution 2

# Answers ---------------------------------------------------------------------
#' Part 1: 237
#' Part 2: 172