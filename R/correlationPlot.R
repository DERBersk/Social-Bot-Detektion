createCorPlot <- function(data){
  pkgTest("corrplot")
  
  testRes=cor.mtest(data[2:11])$p
  par(family = "CMU Serif")
  corrplot(
    corr = cor(data[2:11],use = "complete.obs"),
    p.mat=testRes,
    sig.level = 0.1,
    method = "square",
    addCoef.col = "#787878",
    type="lower",
    col=COL2('RdBu'),
    tl.col = "black",
    tl.srt = 45,
    col.lim=c(-1, 1),
  ) 
}
