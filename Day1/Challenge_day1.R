
#Advent Calendar Challenge Day1 :|
getwd()
First_Advent = read.csv('AOC1.csv',header = F)
#Part 1
diff2020 =  2020-First_Advent$V1 #Take diffs
listin2020 = sapply(First_Advent$V1,FUN = function(X){X %in% diff2020})
First_Advent$V1[which(listin2020)[1]] * First_Advent$V1[which(listin2020)[2]]


###Part 2
listin2020 = lapply(First_Advent$V1,FUN = function(X){2020 - (X + First_Advent$V1)}) #Get differences similar to before
testthis = unlist(listin2020) ##UNLIST THAT LAPPLY 
unlist(sapply(unique(testthis[testthis>0]),function(X){ #Drop negatives
  if(X %in% First_Advent$V1){ #Find possible items
    return(X)
  }
})) 
 #Verifying the answer
352*795*873

