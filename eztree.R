#ライブラリ読み込み
#library(rpart) #CART
library(partykit) #ctree
library(evtree) #evtree
#library(RWeka) #J48
#library(C50) #C5.0




#methodの正式名から略号に変換
get.short <- function(x){
  short.name <- c("ctree", "evtree", "J48", "C5.0")
  names(short.name) <- c("Conditional Inference Trees", 
                  "Evolutionary Learning of Globally Optimal Trees", 
                  "J48", 
                  "C5.0")
  return(short.name[x])
}

#methodに選択できる名称
get.method <- function(y){
  if(is.numeric(as.vector(as.matrix(y)))){
    ret <- c("Conditional Inference Trees", 
               "Evolutionary Learning of Globally Optimal Trees")
  }else{
    ret <- c("Conditional Inference Trees", 
             "Evolutionary Learning of Globally Optimal Trees")
             #"J48", 
             #"C5.0","CART")
  }
  return(ret)
}


#Explanatory variable
get.explanatory <- function(df, purpose = NULL){
  df.name <- colnames(df)
  if(length(purpose) == 1){
    #https://www.trifields.jp/how-to-remove-an-element-with-a-string-in-a-string-vector-with-r-1776
    ret <- df.name[-which(df.name %in% purpose)]
    #checkboxGroupInput のchoicesに渡す値。
    #listじゃなくてもよかったみたい。
  }else{
    ret <- df.name
  }
  return(ret)
}

#ラッパー
eztree <- function(formula, data, method = "ctree"){
  #J48, C5.0の解析をしたいが、画像のプロットで失敗する。
  #CARTのプロットは、関数の中に入れたreturnをpartyでプロットすると失敗する。
  if(method == "CART"){
    result <- as.party(rpart(formula, data ))
    #return(result)
  }
  if(method == "ctree"){
    result <- ctree(formula, data = data)
    #return(result)
  }
  if(method == "evtree"){
    result <- evtree(formula, data = data)
    #return(result)
  }
  if(method == "J48"){
    result <- J48(formula, data = data)
    #return(result)
  }
  if(method == "C5.0"){
    result <- C5.0(formula, data = data)
    #return(result)
  }
  return(result)
  
}

#文字からformula
chr2formula <- function(y, x){
  ret <- paste0(y, "~", paste(x, collapse = "+"))
  ret <- as.formula(ret)
  return(ret)
  
}




