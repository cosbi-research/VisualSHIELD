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
### GSE48390/GSE48297 ###
# Title 	Concurrent Gene Signatures for Han Chinese Breast Cancers [Affymetrix]
# Organism 	Homo sapiens
# Experiment type Expression profiling by array
# Genome variation profiling by array
# Overall design:	We performed 23 array CGHs and 81 gene expression microarrays in breast cancer samples from Taiwanese women. Genes with coherent patterns of both CNV and differential gene expression were identified from the 21 samples assayed using both platforms. We used these genes to derive signatures associated with clinical ER and HER2 status and disease-free survival.

gset <- getGEO(GEO=NULL, filename="VisualSHIELD_analysis/GSE102484_series_matrix.txt.gz", GSEMatrix =TRUE, getGPL=FALSE)
genedata <- gset
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
age <- as.numeric(p[["age at diagnosis:ch1"]])
risk <- as.numeric(p[['18-gene score:ch1']])
status_class <- as.numeric(p[["event_metastasis:ch1"]])
cancer_stage_class <- as.numeric(p[['Stage:ch1']])

pid <- gsub('(?:Breast cancer tissue|Normal breast tissue) ([0-9A-Z_]+).*', '\\1', p$title)
pid <- paste0('P_',gsub('[ -]', '_', pid))


lexid <- data.frame(ID=pid)#,risk_class=as.numeric(risk >= 21),risk=risk,age=age,cancer_stage_class=cancer_stage_class)
lex <- data.frame(ID=pid,status=status_class)#,risk_class=as.numeric(risk >= 21),risk=risk,age=age,cancer_stage_class=cancer_stage_class)
pex <- data.table(t(ex.cv.max.clean))
cex <- pex[, ..common.genes]
nex <- data.table(t(ex.cv.max.clean.norm))
cnex <- nex[, ..common.genes]

df<-cbind(lex,pex)
fwrite(df, file="VisualSHIELD_analysis/GSE102484_all_features_with_max_coefvar_with_status.csv", sep = ",")

cdf<-cbind(lex,cex)
fwrite(cdf, file="VisualSHIELD_analysis/GSE102484_all_features_with_max_coefvar_with_status_GSE2034_compatible.csv", sep = ",")

nf<-cbind(lex,nex)
fwrite(nf, file="VisualSHIELD_analysis/GSE102484_all_features_with_max_coefvar_with_status_norm.csv", sep = ",")

cnf<-cbind(lex,cnex)
fwrite(cnf, file="VisualSHIELD_analysis/GSE102484_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")

dfid<-cbind(lexid,pex)
tdf<-transpose(dfid)
rownames(tdf)<-colnames(dfid)
colnames(tdf)<-tdf[1,]
tdf <- tdf[-1,]
tdf$ID <- rownames(tdf)
tdf<-tdf[, c('ID', colnames(tdf)[colnames(tdf) != "ID"])]
fwrite(tdf, file="VisualSHIELD_analysis/GSE102484_all_features_with_max_coefvar_by_sample.csv", sep = ",")


# 18-gene score = 4 × TRPV6 + 3 × DDX39 + 8 × BUB1B + CCR1 + STIL + 3 × BLM + 11 × C16ORF7 + 4 × PIM1 + 
#                 TPX2 + 2 × PTI1 + 2 × TCF3 + CCNB1 + DTX2 + 2 × ENSA + 5 × RCHY1 + 4 × NFATC2IP + OBSL1 + 2 × MMP15
# We defined patients with scores of ≥21 as high-risk and <21 as low-risk for distant metastasis. 
# We also evaluated the 18-gene scores as continuous variables to estimate the likelihood of distant metastasis.

computed_risk

#sex <- Sel.Features(ex, status_class, K = "Min", Verbose = FALSE)
#rex <- Sel.Features(ex, risk_class, K = "Min", Verbose = FALSE)
#eex <- Sel.Features(ex, er_class, K = "Min", Verbose = FALSE)
#hex <- Sel.Features(ex, her2_class, K = "Min", Verbose = FALSE)

#final_features<- unique(c(rownames(sex$Features), 
#                          rownames(rex$Features),
#                          rownames(eex$Features),
#                          rownames(hex$Features),
#                          # the one mentioned in the paper PMID:24098497
#                          c("RCAN3", "MCOLN2", "DENND2D", "RWDD3", "ZMYM6", "CAPZA1", "GPR18", "WARS2", "TRIM45", "SCRN1", "CSNK1E", "CSDE1", "MRPL20", "IKZF1", "COL20A1")
#                        ))

final_features<-c('KDM1A','BRMS1','VIM', 'INSIG2', 'KLK11', 'MRPL33', 'COL5A2', 'OLFML3','SLC1A1')

# add annotations useful for analysis to experiment data
pex <- data.table(t(ex))
# take only wanted features
pex <- pex[ , ..final_features]

# get time,status for survival analysis
pid <- p$title
lex <- data.frame(ID=pid,cancer_stage=cancer_stage_class,metastasis=metastasis_class)

df<-cbind(lex,pex)
fwrite(df, file="GSE102484_paper_PMC6220148.csv", sep = ",")
