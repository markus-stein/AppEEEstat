
################# Usar as 3 primeiras funções para não ficar repetindo ###############


###################################### funções auxiliares ############################

QuantAux <- function(y){
  histy <- hist(as.numeric(y), plot=F)
  intervclasse <- cut(y, breaks = histy$breaks, include.lowest=TRUE)
  return(intervclasse)
}


TotalColuna <- function(x){
  totalcoluna <- colSums(x)
  x <- rbind(x, totalcoluna)
  return(x)
}

TotalLinha <- function(y){
  totallinha <- rowSums(y)
  y <- cbind(y, totallinha)
  return(y)
}

TotalColLin <- function(x){
  totalcol <- TotalColuna(x)
  totalcollin <- TotalLinha(totalcol)
  totalcollin <- round(totalcollin,3)
  return(totalcollin)
}

# PropPorColuna <- function(w,z){
#   Porcoluna <- table(w,z)
#   Porcoluna <- prop.table(Porcoluna,2)
#   return(Porcoluna)
# }

# PropPorLinha <- function(w,z){
#   Porlinha <- table(w,z)
#   Porlinha <- prop.table(Porlinha,1)
#   return(Porlinha)
# }

########################## Qualitativa Qualitativa ####################################

# Valor absoluto
TabelaBiQuali <- function(w,z){
  TabiQuali <- table(w,z)
  TabiQuali <- TotalColLin(TabiQuali)
  return(TabiQuali)
}
TabelaBiQuali(mtcars$vs,mtcars$am)

  

  # Frequência relativa   
TabelaBiQualifr <- function(w,z){
  TabiQualifr <- table(w,z)/length(w)
  TabiQualifr <- TotalColLin(TabiQualifr)
  return(TabiQualifr)
}
TabelaBiQualifr(mtcars$vs,mtcars$am)

  
  
  # Proporção em relação à coluna
PropPorColuna(mtcars$vs,mtcars$am)


  # Proporção em relação à linha
PropPorLinha(mtcars$vs,mtcars$am)


################## QUALI QUANT ######################################################  
  
# A primeira variável deve ser qualitativa e a segunda deve ser quantitativa
# Valor absoluto
TabelaQlQn  <- function(w,z){
  TabelaQualiQuant <- table(w, QuantAux(z))
  TabelaQualiQuant <- TotalColLin(TabelaQualiQuant)
  return(TabelaQualiQuant)
}
TabelaQlQn(mtcars$vs,mtcars$wt)


# Frequência relativa
TabelaQlQnfr <- function(w,z){
  TabelaQlQnfr <- table(w, QuantAux(z))/length(w)
  TabelaQlQnfr <- TotalColLin(TabelaQlQnfr)
  return(TabelaQlQnfr)
}
TabelaQlQnfr(mtcars$vs,mtcars$wt)
 


# Proporção em relação à coluna
# TabelaQlQnProCol <- function(w,z){
#   TabelaQlQncol <- PropPorColuna(w,QuantAux(z))
#   return(TabelaQlQncol)
#   }
# TabelaQlQnProCol(mtcars$vs, mtcars$wt)
PropPorColuna(mtcars$vs, QuantAux(mtcars$wt))


# Proporção em relação à linha
# TabelaQlQnProLin <- function(w,z){
#   TabelaQlQnlin <- PropPorLinha(w,QuantAux(z))
#   return(TabelaQlQnlin)
# }
# TabelaQlQnProLin(mtcars$vs, mtcars$wt)
PropPorLinha(mtcars$vs, QuantAux(mtcars$wt))


#################### Quantitativa Quantitativa ##########################

# Valor absoluto
TabelaQnQn <- function(w,z){
  Quantit <- table(QuantAux(w),QuantAux(z))
  Quantit <- TotalColLin(Quantit)
  return(Quantit)
}
TabelaQnQn(mtcars$wt,mtcars$qsec)



# Frequência relativa
TabelaQnQnfr <- function(w,z){
  Quantitfr <- table(QuantAux(w),QuantAux(z))/length(w)
  Quantitfr <- TotalColLin(Quantitfr)
  return(Quantitfr)
}
TabelaQnQnfr(mtcars$wt,mtcars$qsec)


 #  Proporção em relação à coluna
# TabelaQnQnProCol <- function(w,z){
#   Quantitcol <- PropPorColuna(QuantAux(w),QuantAux(z))
#   return(Quantitcol)
# }
# TabelaQnQnProCol(mtcars$wt,mtcars$qsec)
PropPorColuna(QuantAux(mtcars$wt),QuantAux(mtcars$qsec))


  # Proporção em relação à linha
# TabelaQnQnProLin <- function(w,z){
#   Quantitlin <- PropPorLinha(QuantAux(w),QuantAux(z))
#   return(Quantitlin)
# }
# TabelaQnQnProLin(mtcars$wt, mtcars$qsec)
PropPorLinha(QuantAux(mtcars$wt),QuantAux(mtcars$qsec))



####################### Proporção por Coluna ####################################

PropPorColunaQlQl <- function(w,z){
  Porcoluna <- table(w,z)
  Porcoluna <- prop.table(Porcoluna,2)
  return(Porcoluna)
}

PropPorColunaQlQn <- function(w,z){
  Porcoluna <- table(w,QuantAux(z))
  Porcoluna <- prop.table(Porcoluna,2)
  return(Porcoluna)
}

PropPorColunaQnQn <- function(w,z){
  Porcoluna <- table(QuantAux(w),QuantAux(z))
  Porcoluna <- prop.table(Porcoluna,2)
  return(Porcoluna)
}

PropPorColunaQlQn(mtcars$vs, mtcars$wt)


############################ Proporção por Linha ###########################

PropPorLinhaQlQl <- function(w,z){
  Porlinha <- table(w,z)
  Porlinha <- prop.table(Porlinha,1)
  return(Porlinha)
}

PropPorLinhaQlQn <- function(w,z){
  Porlinha <- table(w,QuantAux(z))
  Porlinha <- prop.table(Porlinha,1)
  return(Porlinha)
}

PropPorLinhaQnQn <- function(w,z){
  Porlinha <- table(QuantAux(w),QuantAux(z))
  Porlinha <- prop.table(Porlinha,1)
  return(Porlinha)
}





TabelaBivariada <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    TabelaBiQuali(w,z)
  } else { 
  if (is.factor(w) & !is.factor(z)){
    TabelaQlQn(w,z)
  } else {
    TabelaQnQn(w,z)
  }
  }
}
TabelaBivariada(mtcars$cyl,mtcars$qsec)


TabelaBivariadaRelativa <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    TabelaBiQualifr(w,z)
  } else { 
    if (is.factor(w) & !is.factor(z)){
      TabelaQlQnfr(w,z)
    } else {
      TabelaQnQnfr(w,z)
    }
  }
}
TabelaBiQualifr(mtcars$am,mtcars$cyl)



TabelaBivariadaPropColuna <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    PropPorColuna(w,z)
  } else { 
    if (is.factor(w) & !is.factor(z)){
     PropPorColuna(w,QuantAux(z))
    } else {
      PropPorColuna(QuantAux(w),QuantAux(z))
    }
  }
}
TabelaBivariadaPropColuna(mtcars$vs, mtcars$wt)



TabelaBivariadaPropLinha <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    PropPorLinha(w,z)
  } else { 
    if (is.factor(w) & !is.factor(z)){
      PropPorLinha(w,QuantAux(z))
    } else {
      PropPorLinha(QuantAux(w),QuantAux(z))
    }
  }
}

TabelaBivariadaPropLinha(mtcars$wt, mtcars$qsec)


################################ Proporção pro coluna final ##############
PropPorColuna <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    PropPorColunaQlQl(w,z) 
    } else { 
    if (is.factor(w) & !is.factor(z)){
      PropPorColunaQlQn(w,z)
      } else { PropPorColunaQnQn(w,z)}
    }
  }

PropPorColuna(mtcars$qsec,mtcars$wt)  


#################################### Proporção por coluna final ##################
PropPorLinha <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    PropPorLinhaQlQl(w,z) 
  } else { 
    if (is.factor(w) & !is.factor(z)){
      PropPorLinhaQlQn(w,z)
    } else { PropPorLinhaQnQn(w,z)}
  }
  }

PropPorLinha(mtcars$am,mtcars$vs)  


