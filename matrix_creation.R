##Creating Protein part of the matrix the x-axis
pr1 <- abpp_pr[,1:2]
pr1[,3] <- str_c(pr1[,2], pr1[,1], sep="-")
pr1 <- pr1[,2:3]

##Creating the y-axis metabolites
metab1 <- metab_neg[,1:2]
bad <- str_detect(metab1[,2], "m/z") 
metab2 <- metab1[!bad,]
bad <- str_detect(metab2[,2], "[0123456789][0123456789][0123456789][0123456789]n") 
metab2 <- metab2[!bad,]
metab2[,3]<- str_c(metab2[,2], metab2[,1], sep="-")
metab3 <- metab2
apply(metabname_ID,1,function(x) {
    metab3[metab2 == x[1]] <<- x[2]
})
metab3 <- metab3[,2:3]