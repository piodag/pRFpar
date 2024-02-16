#' Benjamini - Hochberg plot
#' @param pRFpar.list output list of pRFpar
#' @param FDRprob FDR probability tolarance. Default 0.2
#'@export
plotBH<-function(pRFpar.list,FDRprob = 0.2){

df<-cbind(pRFpar.list$Res.table,pRFpar.list$obs)
df.o <- df[order(df$p.value),]
# B-H ratios
df.o$ratio <- 1:nrow(df.o)/nrow(df.o)
df.o$id<-1:nrow(df.o)

BH.plot<-ggplot(df.o,mapping=aes(x=id,
                                 y=p.value,
                                 label=Feature.id))+
  geom_point()+
  ylim(0,1.2)+
  geom_text(size=4,angle=90,hjust=-0.2)+
  geom_line(aes(x=id,
                y=ratio*FDRprob),
            col="blue")+
  geom_line(aes(x=id,
                y=ratio*0.05),
            col="green")

BH.plot
}