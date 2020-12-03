###Challenge day 3

getwd()
setwd('Day3/')
treeslopes = read.csv('Day3Trees.csv',header = F,stringsAsFactors = FALSE)


#part 1 and 2 same functino
treesfunc = function(slopex,slopey){    
    t = 1
    treeshit = c()
    for(X in seq(1 + slopey,length(treeslopes$V1),by = slopey)){
      
      t = t + slopex
      if(t>31){
        t = t - 31
        
      }
 
      treeshit = append(treeshit,substr(treeslopes$V1[X],t,t))
    }
    return(as.double(length(treeshit[treeshit == '#'])))
}

treesfunc(3,1) * # part 1
treesfunc(1,1) *
treesfunc(5,1) *
treesfunc(1,2) *
treesfunc(7,1)
