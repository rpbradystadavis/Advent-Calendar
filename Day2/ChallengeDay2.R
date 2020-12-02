########################
#Challenge Day 2
library(stringr)

setwd('Day2/')
day2pass = read.csv('Day2pass.csv',stringsAsFactors = F)
##Part 1
day2parse = as.data.frame(matrix(unlist(strsplit(day2pass$X1.14.b..bbbbbbbbbbbbbbbbbbb,split = ' ')),ncol = 3,byrow = T))
names(day2parse) = c('Range','letter','password') #rename cols For keepign myself straight
day2parse$Minrange = as.integer(matrix(unlist(str_split(day2parse$Range,'-')),ncol = 2,byrow = T)[,1])
day2parse$Maxrange = as.integer(matrix(unlist(str_split(day2parse$Range,'-')),ncol = 2,byrow = T)[,2])
day2parse$password = as.character(day2parse$password)
day2parse$letter = gsub(':',replacement = '',day2parse$letter)
day2parse$str_counts = sapply(1:length(day2parse$letter),function(X){
  str_count(pattern = day2parse$letter[X],string = day2parse$password[X])
})
str_count(pattern = day2parse$letter[1],string =  day2parse$password[1])
dim(day2parse[day2parse$str_counts >= day2parse$Minrange & day2parse$str_counts <= day2parse$Maxrange,])
##Part 2
day2parse$minletter =  sapply(1:length(day2parse$letter),function(X){
 day2parse$letter[X] == substr(day2parse$password[X],day2parse$Minrange[X],day2parse$Minrange[X]) 
})
day2parse$maxletter =  sapply(1:length(day2parse$letter),function(X){
  day2parse$letter[X] == substr(day2parse$password[X],day2parse$Maxrange[X],day2parse$Maxrange[X]) 
})
day2parse$equalletters = day2parse$minletter + day2parse$maxletter
table(day2parse$equalletters)
