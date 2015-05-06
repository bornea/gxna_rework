##Creating Protein part of the matrix the x-axis
pr1 <- abpp_pr[,2]
pr1 <- unique(pr1)
pr2 <- data.frame(Leading.Protein = pr1, stringsAsFactors=F)


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
metab_ids <- unique(metab3[,1])

##Compiling into a matrix
pr2[,2:22] <- " "
colnames(pr2) <- c("Leading.Protein", metab_ids)

##Populating the matrix with scores
for (j in 1:nrow(pr2)){
  pattern <- str_extract(pr2[j,1],"[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}") 
  match <- str_detect(scored_gra1[,1], pattern)
  sub_gra <- scored_gra1[match,]
  for (i in 2:ncol(pr2)){ 
    match2 <- str_detect(sub_gra[,2], colnames(pr2[i]))
    sub_sub_gra <- sub_gra[match2,]
    if (nrow(sub_sub_gra) > 0){
      pr2[j,i] <- sub_sub_gra[1,5]
    }
    else {
      pr2[j,i] <- "N/A"
    }
  }
}

