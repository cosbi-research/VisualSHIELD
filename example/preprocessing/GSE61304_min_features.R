#   Data plots for selected GEO samples
library(GEOquery)
library(limma)
library(umap)
# annotationdbi for Affymetrix HG-U133_Plus_2 Array
library(hgu133plus2.db)
# human data
library(org.Hs.eg.db)
library(data.table)

rm(list=ls())

# load series and platform data from GEO
### GSE87377 ###

gset <- getGEO(GEO="GSE61304", GSEMatrix =TRUE, getGPL=FALSE)
genedata <- gset[[1]]
#genedata2 <- gset2[[1]]

p<- pData(genedata)
ex <- exprs(genedata)
#p2<- pData(genedata2)
#ex2 <- exprs(genedata2)

# map probe ID to gene symbol
genenames <- AnnotationDbi::select(hgu133plus2.db, 
                                   keys=rownames(ex), 
                                   columns=c("SYMBOL"), 
                                   keytype="PROBEID",
                                   multiVals="first")
genenames.unique <- genenames[!duplicated(genenames$PROBEID), ]

genenames.unique$cv <- apply(ex, MARGIN=1, FUN=function(el){
  sd(el)/mean(el)  
})

genenames.unique.ordered<-genenames.unique[order(genenames.unique$SYMBOL, genenames.unique$cv, decreasing = T),]
genenames.unique.ordered.cv.max <- genenames.unique.ordered[!duplicated(genenames.unique.ordered$SYMBOL), ]

ex.cv.max <- ex[rownames(ex) %in% genenames.unique.ordered.cv.max$PROBEID, ]

rownames(ex.cv.max) <- sapply(rownames(ex.cv.max), function(probe_id){
  vals<-genenames.unique.ordered.cv.max[which(genenames.unique.ordered.cv.max$PROBEID == probe_id),'SYMBOL']
  if(length(vals)<=1)
    if( is.na(vals) )
      # can't find an equivalent human gene symbol
      return(paste0("uwn_",probe_id))
  else
    return(gsub('@', '_',gsub('-','_',vals)))
  else
    return(gsub('@', '_',gsub('-','_',vals[1])))
})

# remove uwn_
ex.cv.max.clean <- ex.cv.max[!grepl('^uwn_', rownames(ex.cv.max)),]
ex.cv.max.clean.norm <- t(apply(ex.cv.max.clean, 1, function(x)(x-mean(x))/sd(x)))

common.genes<-readLines('VisualSHIELD_analysis/common_genenames.txt')
missing.genes<- setdiff(common.genes, rownames(ex.cv.max.clean))
common.genes<- setdiff(common.genes, missing.genes)

# 0: censored, 1: event occoured
status_class <- as.numeric(p[["distant metastasis:ch1"]])
# remove na fields

pid <- rownames(p)


lexid <- data.frame(ID=pid)#,risk_class=as.numeric(risk >= 21),risk=risk,age=age,cancer_stage_class=cancer_stage_class)
lex <- data.frame(ID=pid,status=status_class)#,risk_class=as.numeric(risk >= 21),risk=risk,age=age,cancer_stage_class=cancer_stage_class)
pex <- data.table(t(ex.cv.max.clean))
cex <- pex[, ..common.genes]
nex <- data.table(t(ex.cv.max.clean.norm))
cnex <- nex[, ..common.genes]

df<-cbind(lex,pex)
# remove NA
fwrite(df[!is.na(df$status),], file="VisualSHIELD_analysis/GSE61304_all_features_with_max_coefvar_with_status.csv", sep = ",")

cdf<-cbind(lex,cex)
# remove NA
fwrite(cdf[!is.na(cdf$status),], file="VisualSHIELD_analysis/GSE61304_all_features_with_max_coefvar_with_status_GSE2034_compatible.csv", sep = ",")

nf<-cbind(lex,nex)
# remove NA
fwrite(nf[!is.na(nf$status),], file="VisualSHIELD_analysis/GSE61304_all_features_with_max_coefvar_with_status_norm.csv", sep = ",")

cnf<-cbind(lex,cnex)
# remove NA
fwrite(cnf[!is.na(cnf$status),], file="VisualSHIELD_analysis/GSE61304_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")

