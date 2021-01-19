
#Enter data Response column as string method = 'Fwd' for Forward Select or 'BSS' for Best Subsets Exhaustive
#measue = c('AIC','BIC','Ar2','CV') for criterion for the model that is to be returned
subset.select <- function(df , Response, method = 'Fwd', measure ='AIC'){
  #convert chars to factors
  require(tidyverse)
  require(gtools)
  
  #turn chars into factors
  if (!all(Vectorize(function(i)is.character(df[,i,drop=T]))(i =1:ncol(df)))){
    df <- df %>% mutate_if(is.character, as.factor)
  }
  
  preds <- colnames(df)[colnames(df) != Response]
  form <- as.formula(paste(Response,'~.'))
  pred.stor <- character(length(preds))
  
  if(method == 'Fwd'){
    for (i in seq_len(length(preds))){
      if (i == 1){
        r2 <- sapply(preds,function(x){summary(lm(form,data =df[,c(Response,x)]))$r.squared})
        
      } else{
        r2 <- sapply(preds,function(x){summary(lm(form,data =df[,c(Response,x,pred.stor[1:(i-1)])]))$r.squared})
      }
      
      pred.stor[i] <- as.character(preds[which(r2 == max(r2))])
      preds <- preds[!(preds %in% pred.stor)]
    }
    
    colnames(df)[colnames(df) == 'Y'] <- Response
    form.stor <- Vectorize(function(i) paste(Response , '~',paste(pred.stor[1:i],collapse = '+')), "i") (i=1:length(pred.stor))
  } else if(method == 'BSS'){  
    
    #Best Subsets
    combos <- Vectorize(function(i) combinations(length(preds),i,preds), "i") (i=1:length(preds)) 
    outputs <- Vectorize(function(i) combos[[i]][which.max(apply(combos[[i]],1,function(x)summary(lm(form,data =df[,c(Response,x)]))$adj.r.squared)),] ,'i') (i=1:length(preds)) 
    form.stor <- sapply(outputs, function(x) paste(Response,'~',paste(x,collapse = '+')))
    
  } 
  
  if(measure == 'Ar2'){
    Ar2.scores <- sapply(form.stor,function(x){summary(lm(as.formula(x),data =df))$adj.r.squared})
    form <- as.formula(form.stor[Ar2.scores == max(Ar2.scores)])
    return(list(model =lm(form,data=df), best.levels =form.stor))
  }
  
  if(measure == 'AIC'){
    
    AICs <- sapply(form.stor,function(x){AIC(lm(as.formula(x),data=df))})
    form <- as.formula(form.stor[AICs == min(AICs)])
    return(list(model =lm(form,data=df), best.levels =form.stor))
  }
  
  if(measure == 'BIC'){
    
    BICs <- sapply(form.stor,function(x){BIC(lm(as.formula(x),data=df))})
    form <- as.formula(form.stor[BICs == min(BICs)])
    return(list(model =lm(form,data=df), best.levels =form.stor))
  }
  
  if(measure == 'CV'){
    index <- sample( 1:nrow(df),size = round(nrow(df)/4) , replace = F )
    df.train <- df %>% filter(!(row_number() %in% index))
    Y.test <- df[index,Response,drop=F]
    df.test <- df[index,] %>% select(-Response)
    
    RSSs <- sapply(form.stor,function(x){model = lm(as.formula(x),data=df.train);
    predictions = predict(model,df.test);
    return(sum((Y.test-predictions)^2))})
    
    form <- as.formula(form.stor[RSSs == min(RSSs)])
    return(list(model =lm(form,data=df), best.levels =form.stor))
  }
}

