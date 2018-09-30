# pour tester certaines functions depuis un script
#sink("inst/extdata/log.txt",append=FALSE)
#sink()
vtest <- c(1,2,3,4)
countif(vtest)

sortBy <- function(a, field) a[order(sapply(a, "[", i = field))]
sortBy(a, "day")

