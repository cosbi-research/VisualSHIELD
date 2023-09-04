#   Data plots for selected GEO samples
library(GEOquery)
library(limma)
library(umap)
# annotationdbi for Affymetrix HG-U133_Plus_2 Array
library(hgu133plus2.db)
# human data
library(org.Hs.eg.db)
#library(propOverlap)
library(data.table)
#library(varImp)

rm(list=ls())

# load series and platform data from GEO
### GSE46222 ###
# Title 	Concurrent Gene Signatures for Han Chinese Breast Cancers [Affymetrix]
# Organism 	Homo sapiens
# Experiment type Expression profiling by array
# Genome variation profiling by array
# Overall design:	We performed 23 array CGHs and 81 gene expression microarrays in breast cancer samples from Taiwanese women. Genes with coherent patterns of both CNV and differential gene expression were identified from the 21 samples assayed using both platforms. We used these genes to derive signatures associated with clinical ER and HER2 status and disease-free survival.

gset <- getGEO("GSE19615", GSEMatrix =TRUE, getGPL=FALSE)
length(gset)
# GPL8490
#if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
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
                                   keytype="PROBEID")#,
                                   #multiVals="first")
genenames.unique <- genenames[!duplicated(genenames$PROBEID), ]

genenames.unique$cv <- apply(ex, MARGIN=1, FUN=function(el){
  sd(el)/mean(el)  
})

genenames.unique.ordered<-genenames.unique[order(genenames.unique$SYMBOL, genenames.unique$cv, decreasing = T),]
genenames.unique.ordered.cv.max <- genenames.unique.ordered[!duplicated(genenames.unique.ordered$SYMBOL), ]

# use gene symbol as rownames
# replace - with _ (opal doesn't like dash)
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
status_class <- as.numeric(p[["distant recur (yn):ch1"]] == 'Y')
pid <- rownames(p)

lex <- data.frame(ID=pid,status=status_class)#,survival_time=survival_time,status=status_class,survival_time=survival_time,risk=risk_class,er=er_class,her2=her2_class)
pex <- data.table(t(ex.cv.max.clean))
cex <- pex[, ..common.genes]
nex <- data.table(t(ex.cv.max.clean.norm))
cnex <- nex[, ..common.genes]

df<-cbind(lex,pex)
fwrite(df, file="VisualSHIELD_analysis/GSE19615_all_features_with_max_coefvar_with_status.csv", sep = ",")

nf<-cbind(lex,nex)
fwrite(nf, file="VisualSHIELD_analysis/GSE19615_all_features_with_max_coefvar_with_status_norm.csv", sep = ",")

cdf<-cbind(lex,cex)
fwrite(cdf, file="VisualSHIELD_analysis/GSE19615_all_features_with_max_coefvar_with_status_GSE2034_compatible.csv", sep = ",")

cnf<-cbind(lex,cnex)
fwrite(cnf, file="VisualSHIELD_analysis/GSE19615_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")
