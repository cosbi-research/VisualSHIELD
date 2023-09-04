library(data.table)
library(ggplot2)
library("gridExtra")
library(svglite)
rm(list=ls())
# importance plots for the three Random forests separately
datasets<-list("Server1"="GSE10", "Server2"="GSE2034", "Server23"="GSE48", "Server24"="GSE61", "Server25"="GSE19")
datasets.name<-list("GSE10"="GSE102484", "GSE2034"="GSE2034", "GSE48"="GSE48390", "GSE61"="GSE61304", "GSE19"="GSE19615")

GSE10<-fread(file="VisualSHIELD_analysis/GSE102484_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")
GSE10_true_status<-GSE10$status

GSE2034<-fread(file="VisualSHIELD_analysis/GSE2034_all_features_with_max_coefvar_with_status_norm.csv", sep = ",")
GSE2034_true_status<-GSE2034$status

GSE48<-fread(file="VisualSHIELD_analysis/GSE48390_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")
GSE48_true_status<-GSE48$status

GSE61<-fread(file="VisualSHIELD_analysis/GSE61304_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")
GSE61_true_status<-GSE61$status

GSE19<-fread(file="VisualSHIELD_analysis/GSE19615_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm.csv", sep = ",")
GSE19_true_status<-GSE19$status

gsedatas<-list("GSE10"=GSE10, "GSE2034"=GSE2034, "GSE48"=GSE48, "GSE61"=GSE61, "GSE19"=GSE19)
gsestatus<-list("GSE10"=GSE10_true_status, "GSE2034"=GSE2034_true_status, "GSE48"=GSE48_true_status, "GSE61"=GSE61_true_status, "GSE19"=GSE19_true_status)

lapply(gsestatus, function(l){ sum(l)/length(l) })

tables.map<-list(
  'Server1'='GSE102484.GSE102484_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm',
  'Server2'='GSE2034.GSE2034_all_features_with_max_coefvar_with_status_norm',
  'Server23'='GSE48390.GSE48390_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm',
  'Server24'='GSE61304.GSE61304_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm',
  'Server25'='GSE19615.GSE19615_all_features_with_max_coefvar_with_status_GSE2034_compatible_norm'
)

tpredict <- function(model, newdata){
  coeffs<-model$coefficients[,'Estimate']
  varnames<-names(coeffs)
  # sample row to extract match between new data name, and model variables
  row<-newdata[1,]
  var.map <- sapply(varnames, function(name){
    parts<-strsplit(name, ':', fixed=T)[[1]]
    if(length(parts)>1){
      # multiple
      sapply(parts, function(part){which(paste0('D$',names(row)) == part)})
    }else{
      # single
      which(paste0('D$',names(row)) == name)
    }
  })
  predictions <- apply(newdata, 1, function(row){
    logity<-coeffs[1] + sum(sapply(2:length(varnames), function(i){
      idxs <- var.map[[i]]
      coeffs[i] * prod(as.numeric(row[idxs]))
    }))
    odds <- exp(logity)
    prob <- odds / (1 + odds)
    return(prob)
  })
  return(predictions)
}

cross.validated.glm.glm <- lapply(tables.map, function(l){
  n<-names(tables.map)[tables.map == l]
  print(datasets[[n]])
  dat<-gsedatas[[ datasets[[n]] ]]
  true.output<-gsestatus[[ datasets[[n]] ]]
  
  # test using the randomforests not trained on this dataset
  tglm<-readRDS(paste0('VisualSHIELD_analysis/paper_PMC6220148/GLM_risk_prediction/GLM_Tomasoni_leave_out_',datasets[[n]],'_norm.rds'))
  predicted.prob<-tpredict(tglm, newdata=dat)
  pos <- predicted.prob[true.output == '1']
  neg <- predicted.prob[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  # ROC
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    predicted.output<- as.integer(predicted.prob >= treshold)
    contingency_table <- table( data.frame(prediction=predicted.output, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    # prediction=prediction, confusion_matrix=contingency_table
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp)))
  }))
  
  return(list(df=df, auc=auc))
})

### ===== CHENG FORMULA ======
cross.validated.cheng.glm <- lapply(tables.map, function(l){
  n<-names(tables.map)[tables.map == l]
  print(datasets[[n]])
  dat<-gsedatas[[ datasets[[n]] ]]
  true.output<-gsestatus[[ datasets[[n]] ]]
  
  # test using the randomforests not trained on this dataset
  tglm<-readRDS(paste0('VisualSHIELD_analysis/paper_PMC6220148/GLM_risk_prediction/GLM_Cheng_leave_out_',datasets[[n]],'_norm.rds'))
  predicted.prob<-tpredict(tglm, newdata=dat)
  pos <- predicted.prob[true.output == '1']
  neg <- predicted.prob[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  # ROC
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    predicted.output<- as.integer(predicted.prob >= treshold)
    contingency_table <- table( data.frame(prediction=predicted.output, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    # prediction=prediction, confusion_matrix=contingency_table
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp)))
  }))
  
  return(list(df=df, auc=auc))
})

### ===== Chen FORMULA ======
cross.validated.chen.glm <- lapply(tables.map, function(l){
  n<-names(tables.map)[tables.map == l]
  print(datasets[[n]])
  dat<-gsedatas[[ datasets[[n]] ]]
  true.output<-gsestatus[[ datasets[[n]] ]]
  
  # test using the randomforests not trained on this dataset
  tglm<-readRDS(paste0('VisualSHIELD_analysis/paper_PMC6220148/GLM_risk_prediction/GLM_Chen_leave_out_',datasets[[n]],'_norm.rds'))
  predicted.prob<-tpredict(tglm, newdata=dat)
  pos <- predicted.prob[true.output == '1']
  neg <- predicted.prob[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  # ROC
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    predicted.output<- as.integer(predicted.prob >= treshold)
    contingency_table <- table( data.frame(prediction=predicted.output, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    # prediction=prediction, confusion_matrix=contingency_table
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp)))
  }))
  
  return(list(df=df, auc=auc))
})

### ===== Huang FORMULA ======
cross.validated.huang.glm <- lapply(tables.map, function(l){
  n<-names(tables.map)[tables.map == l]
  print(datasets[[n]])
  dat<-gsedatas[[ datasets[[n]] ]]
  true.output<-gsestatus[[ datasets[[n]] ]]
  
  # test using the randomforests not trained on this dataset
  tglm<-readRDS(paste0('VisualSHIELD_analysis/paper_PMC6220148/GLM_risk_prediction/GLM_Huang_leave_out_',datasets[[n]],'_norm.rds'))
  predicted.prob<-tpredict(tglm, newdata=dat)
  pos <- predicted.prob[true.output == '1']
  neg <- predicted.prob[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  # ROC
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    predicted.output<- as.integer(predicted.prob >= treshold)
    contingency_table <- table( data.frame(prediction=predicted.output, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    # prediction=prediction, confusion_matrix=contingency_table
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp)))
  }))
  
  return(list(df=df, auc=auc))
})

# ========== locally trained GLMs ===========
cross.validated.local.reduced.glm <- lapply(tables.map, function(l){
  n<-names(tables.map)[tables.map == l]
  print(datasets[[n]])
  # ALL EXCEPT ONE DATASET
  train.ls<-gsedatas
  train.ls[[datasets[[n]]]]<-NULL
  train <- do.call(rbind, train.ls)
  # THE TEST DATASET
  test<-gsedatas[[ datasets[[n]] ]]
  true.output<-gsestatus[[ datasets[[n]] ]]
  
  reduced<-glm(formula=status~EML1+IMP3+BTN2A2+FKBP5+IGFBP6+LRRC32+STX5+RABAC1+TXNRD1+KATNBL1+GLUD1:SLC26A3+SLC26A3:NDUFB1+RLN1:RLN2+SREK1:GORASP1+TXNIP:GORASP1+PHF10:POLQ+SREK1:NTM, 
               family=binomial(link = "logit"),
               data=train)
  
  predicted.prob<-predict(reduced, newdata=test, type="response")
  pos <- predicted.prob[true.output == '1']
  neg <- predicted.prob[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  # ROC
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    predicted.output<- as.integer(predicted.prob >= treshold)
    contingency_table <- table( data.frame(prediction=predicted.output, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    # prediction=prediction, confusion_matrix=contingency_table
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp)))
  }))
  
  return(list(df=df, auc=auc))
})

cross.validated.local.full.glm <- lapply(tables.map, function(l){
  n<-names(tables.map)[tables.map == l]
  print(datasets[[n]])
  # ALL EXCEPT ONE DATASET
  train.ls<-gsedatas
  train.ls[[datasets[[n]]]]<-NULL
  train <- do.call(rbind, train.ls)
  # THE TEST DATASET
  test<-gsedatas[[ datasets[[n]] ]]
  true.output<-gsestatus[[ datasets[[n]] ]]
  
  full<-glm(formula=status~BTN2A2+ALDH3B2+EML1+FKBP5+IGFBP6+LRRC32+STX5+RABAC1+BCAM+TFIP11+PLIN3+SGK1+TXNRD1+PPP1CC+KATNBL1+CHPT1+IMP3+GLUD1:SLC26A3+SLC26A3:NDUFB1+RLN1:RLN2+SREK1:GORASP1+SREK1:NTM+RLN1:RECQL4+TXNIP:GORASP1+PHF10:POLQ, 
               family=binomial(link = "logit"),
               data=train)
  
  predicted.prob<-predict(full, newdata=test, type="response")
  pos <- predicted.prob[true.output == '1']
  neg <- predicted.prob[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  # ROC
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    predicted.output<- as.integer(predicted.prob >= treshold)
    contingency_table <- table( data.frame(prediction=predicted.output, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    # prediction=prediction, confusion_matrix=contingency_table
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp)))
  }))
  
  return(list(df=df, auc=auc))
})

glm.rfs<-readRDS('VisualSHIELD_analysis/paper_PMC6220148/RF_risk_prediction/RF_top_10percent_GLM-FS_norm.rds')
#glm.rf<-do.call(randomForest::combine,glm.rfs)

cross.validated.glm.rfs <- lapply(tables.map, function(l){
  # test using the randomforests not trained on this dataset
  rmodel <- glm.rfs
  n<-names(tables.map)[tables.map == l]
  rmodel[[n]]<-NULL
  comb.rf<-do.call(randomForest::combine,rmodel)
  true.output <- gsestatus[[ datasets[[n]] ]]
  dat <- gsedatas[[ datasets[[n]] ]]
  print(datasets[[n]])
  
  ptable<-predict(comb.rf, dat, norm.votes=F, proximity=F, type='vote')
  scores <- sapply(seq(1,nrow(ptable)), function(i){ 
    ptable[i, '1'] / (ptable[i, '0']+ptable[i, '1'])
  })
  pos <- scores[true.output == '1']
  neg <- scores[true.output == '0']
  auc <- mean(replicate(50000, sample(pos, size=1) > sample(neg, size=1)))
  
  df <- do.call(rbind, lapply(seq(from=0.0,to=1.0, by=0.01), function(treshold){
    prediction <- sapply(seq(1,nrow(ptable)), function(i){ 
      if(scores[i] >= treshold) '1' else "0"
    })
    contingency_table <- table( data.frame(prediction=prediction, true=true.output) )
    tp<- tryCatch({
      contingency_table["1","1"]
    }, error=function(e){
      0
    })
    tn<- tryCatch({
      contingency_table["0","0"]
    }, error=function(e){
      0
    })
    fn<- tryCatch({
      contingency_table["0","1"]
    }, error=function(e){
      0
    })
    fp<- tryCatch({
      contingency_table["1","0"]
    }, error=function(e){
      0
    })
    return(data.frame(threshold=treshold, tpr=tp/(fn+tp), fpr=fp/(tn+fp) ))
  }))
  
  return(list(df=df, auc=auc))
})

federated.plots <- lapply(datasets, function(test.dataset){
  dataset.name <- datasets.name[[test.dataset]]
  server.name<-names(datasets[datasets == test.dataset])

  rf.df<-cross.validated.glm.rfs[[server.name]]$df
  rf.df$model <- 'RF'
  glm.df <- cross.validated.glm.glm[[server.name]]$df
  glm.df$model <- 'Logit'
  cheng.df <- cross.validated.cheng.glm[[server.name]]$df
  cheng.df$model <- 'Logit (Cheng)'
  chen.df <- cross.validated.chen.glm[[server.name]]$df
  chen.df$model <- 'Logit (Chen)'
  huang.df <- cross.validated.huang.glm[[server.name]]$df
  huang.df$model <- 'Logit (huang)'
  df<-rbind(rf.df,glm.df,cheng.df, chen.df, huang.df)
  
  p2<-ggplot(df, aes(y=tpr,x=fpr,colour=model)) + 
    scale_colour_manual(values = c("RF" = "red", "Logit" = "blue", "Logit (Cheng)" = "purple", "Logit (Chen)" = "magenta", "Logit (Huang)" = "orange")) +
    geom_line() +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
    theme_bw() + 
    ggtitle(paste0('Test dataset: ',dataset.name)) +
    xlab("FPR") + ylab("TPR") + 
    annotate(geom="text", color="red", size=3.5, x=0.8, y=0.5, label=paste0('RF AUC: ',cross.validated.glm.rfs[[server.name]]$auc)) +
    annotate(geom="text", color="blue", size=3.5, x=0.8, y=0.4, label=paste0('Logit AUC: ',cross.validated.glm.glm[[server.name]]$auc)) +
    annotate(geom="text", color="purple", size=3.5, x=0.8, y=0.3, label=paste0('Cheng AUC: ',cross.validated.cheng.glm[[server.name]]$auc)) +
    annotate(geom="text", color="magenta", size=3.5, x=0.8, y=0.2, label=paste0('Chen AUC: ',cross.validated.chen.glm[[server.name]]$auc)) +
    annotate(geom="text", color="orange", size=3.5, x=0.8, y=0.1, label=paste0('Huang AUC: ',cross.validated.huang.glm[[server.name]]$auc)) 
  
  return(p2)  
})

# federated plots against literature
do.call("grid.arrange", c(federated.plots, ncol=2))

federated.vs.local.plots <- lapply(datasets, function(test.dataset){
  dataset.name <- datasets.name[[test.dataset]]
  server.name<-names(datasets[datasets == test.dataset])
  
  rf.df<-cross.validated.glm.rfs[[server.name]]$df
  rf.df$model <- 'RF (Federated)'
  full.df <- cross.validated.local.full.glm[[server.name]]$df
  full.df$model <- 'Logit full (Local)'
  reduced.df <- cross.validated.local.reduced.glm[[server.name]]$df
  reduced.df$model <- 'Logit reduced (Local)'
  glm.df <- cross.validated.glm.glm[[server.name]]$df
  glm.df$model <- 'Logit reduced (Federated)'
  df<-rbind(rf.df,full.df,reduced.df,glm.df)
  
  p2<-ggplot(df, aes(y=tpr,x=fpr,colour=model)) + 
    scale_colour_manual(values = c("RF (Federated)" = "red", 
                                   "Logit full (Local)" = "black", 
                                   "Logit reduced (Local)" = "brown",
                                   "Logit reduced (Federated)" = "blue"
                                   )) +
    geom_line() +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
    theme_bw() + 
    ggtitle(paste0('Test dataset: ',dataset.name)) +
    xlab("FPR") + ylab("TPR") + 
    annotate(geom="text", color="red", size=3.5, x=0.6, y=0.4, label=paste0('RF (Federated) AUC: ',cross.validated.glm.rfs[[server.name]]$auc)) +
    annotate(geom="text", color="black", size=3.5, x=0.6, y=0.3, label=paste0('Logit full (Local) AUC: ',cross.validated.local.full.glm[[server.name]]$auc)) +
    annotate(geom="text", color="brown", size=3.5, x=0.6, y=0.2, label=paste0('Logit reduced (Local) AUC: ',cross.validated.local.reduced.glm[[server.name]]$auc)) +
    annotate(geom="text", color="blue", size=3.5, x=0.6, y=0.1, label=paste0('Logit reduced (Federated) AUC: ',cross.validated.glm.glm[[server.name]]$auc)) 
    
  return(p2)  
})

global.plots <- lapply(datasets, function(test.dataset){
  dataset.name <- datasets.name[[test.dataset]]
  server.name<-names(datasets[datasets == test.dataset])
  
  rf.df<-cross.validated.glm.rfs[[server.name]]$df
  rf.df$model <- 'RF'
  full.df <- cross.validated.local.full.glm[[server.name]]$df
  full.df$model <- 'Logit full'
  glm.df <- cross.validated.glm.glm[[server.name]]$df
  glm.df$model <- 'Logit restricted'
  cheng.df <- cross.validated.cheng.glm[[server.name]]$df
  cheng.df$model <- 'Cheng'
  chen.df <- cross.validated.chen.glm[[server.name]]$df
  chen.df$model <- 'Chen'
  huang.df <- cross.validated.huang.glm[[server.name]]$df
  huang.df$model <- 'Huang'
  df<-rbind(rf.df,full.df,glm.df,cheng.df, chen.df, huang.df)
  
  p2<-ggplot(df, aes(y=tpr,x=fpr,colour=model)) + 
    scale_colour_manual(values = c("RF" = "red", 'Logit full'="black", "Logit restricted" = "blue", "Cheng" = "purple", "Chen" = "magenta", "Huang" = "orange")) +
    geom_line() +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
    theme_bw() + 
    ggtitle(paste0('Test dataset: ',dataset.name)) +
    xlab("FPR") + ylab("TPR") + 
    annotate(geom="text", color="red", size=3.5, x=0.85, y=0.6, label=paste0('RF AUC: ', round(cross.validated.glm.rfs[[server.name]]$auc,3))) +
    annotate(geom="text", color="black", size=3.5, x=0.85, y=0.5, label=paste0('Logit full AUC: ', round(cross.validated.local.full.glm[[server.name]]$auc,3))) +
    annotate(geom="text", color="blue", size=3.5, x=0.85, y=0.4, label=paste0('Logit AUC: ', round(cross.validated.glm.glm[[server.name]]$auc,3))) +
    annotate(geom="text", color="purple", size=3.5, x=0.85, y=0.3, label=paste0('Cheng AUC: ', round(cross.validated.cheng.glm[[server.name]]$auc,3))) +
    annotate(geom="text", color="magenta", size=3.5, x=0.85, y=0.2, label=paste0('Chen AUC: ', round(cross.validated.chen.glm[[server.name]]$auc,3))) +
    annotate(geom="text", color="orange", size=3.5, x=0.85, y=0.1, label=paste0('Huang AUC: ', round(cross.validated.huang.glm[[server.name]]$auc,3))) 
  
  return(p2)  
})

cross.validated<-list(
  'Huang' = cross.validated.huang.glm,
  'Chen' = cross.validated.chen.glm,
  'Cheng' = cross.validated.cheng.glm,
  'Logit restricted' = cross.validated.glm.glm,
  # not needed because equal to logit reduced federated
  #'Logit reduced (Local)' = cross.validated.local.reduced.glm,
  "RF" = cross.validated.glm.rfs,
  'Logit full' = cross.validated.local.full.glm
)
colours<- c("Huang" = "orange", "Chen" = "magenta", "Cheng" = "purple", "Logit restricted" = "blue", "RF" = "red", "Logit full" = "black")


models.average.auc<-sapply(cross.validated, function(cross.validated.model){
  
  m.vals <- sapply(datasets, function(test.dataset){
    dataset.name <- datasets.name[[test.dataset]]
    server.name<-names(datasets[datasets == test.dataset])
    cross.validated.model[[server.name]]$auc
  })
  
  mean(m.vals)
})

models.sd.auc<-sapply(cross.validated, function(cross.validated.model){
  
  m.vals <- sapply(datasets, function(test.dataset){
    dataset.name <- datasets.name[[test.dataset]]
    server.name<-names(datasets[datasets == test.dataset])
    cross.validated.model[[server.name]]$auc
  })
  
  sd(m.vals)
})

df <- data.frame(x=names(models.average.auc), y=models.average.auc, sd=models.sd.auc)

summary.plot<- ggplot(df, aes(x = reorder(x,-y), y = y)) +
  geom_bar(stat = "identity", fill=colours, position=position_dodge(), colour="black") +
  geom_text(aes(label=paste0(round(y,3), ' ± ', round(sd, 3)) ), vjust=-8) +
  geom_errorbar(aes(ymin=y, ymax=y+sd), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Breast cancer prognostic classifiers average AUC")  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  ylab("AUC (mean and sd)") + xlab("") +
  scale_y_continuous(limits=c(0.0,1.0), breaks=seq(0,1,0.1))

# federated vs local plots
#global.plots[["summary"]] <- summary.plot

fname <- 'VisualSHIELD_analysis/paper_PMC6220148/ROC_Tomasoni-Cheng-Hao-Hung_GLM_vs_RF_local_vs_federated_1col.svg'
svglite(filename=fname, width=7, height=17)
do.call("grid.arrange", c(global.plots, ncol=1))
dev.off()

fname <- 'VisualSHIELD_analysis/paper_PMC6220148/AUC_Tomasoni-Cheng-Hao-Hung_GLM_vs_RF.svg'
svglite(filename=fname, width=7)
plot(summary.plot)
dev.off()
