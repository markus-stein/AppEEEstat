
################# Usar as 3 primeiras funções para não ficar repetindo ###############


###################################### funções auxiliares ############################

#### Criadora de intervalos de classes ####
QuantAux <- function(y){
  histy <- hist(as.numeric(y), plot=F)
  intervclasse <- cut(y, breaks = histy$breaks, include.lowest=TRUE)
  return(intervclasse)
}

#### Adiciona total de uma coluna
TotalColuna <- function(x){
  totalcoluna <- colSums(x)
  x <- rbind(x, totalcoluna)
  return(x)
}

#### Adiciona total de uma linha
TotalLinha <- function(y){
  totallinha <- rowSums(y)
  y <- cbind(y, totallinha)
  return(y)
}

#### Adiciona total de uma linha e de uma coluna
TotalColLin <- function(x){
  totalcol <- TotalColuna(x)
  totalcollin <- TotalLinha(totalcol)
  totalcollin <- round(totalcollin,3)
  return(totalcollin)
}


################## Frequência Absoluta ####################### 
TabelaQlQl <- function(w,z){
  TabiQlQl <- table(w,z)
  TabiQlQl <- TotalColLin(TabiQlQl)
  return(TabiQuali)
}

TabelaQlQn  <- function(w,z){
  TabelaQualiQuant <- table(w, QuantAux(z))
  TabelaQualiQuant <- TotalColLin(TabelaQualiQuant)
  return(TabelaQualiQuant)
}


TabelaQnQl  <- function(w,z){
  TabelaQnQl <- table(QuantAux(w),z)
  TabelaQnQl <- TotalColLin(TabelaQnQl)
  return(TabelaQualiQuant)
}


TabelaQnQn <- function(w,z){
  Quantit <- table(QuantAux(w),QuantAux(z))
  Quantit <- TotalColLin(Quantit)
  return(Quantit)
}

#################### Frequência Relativa ##########################


TabelaQlQlfr <- function(w,z){
  TabiQlQlfr <- table(w,z)/length(w)
  TabiQlQlfr <- TotalColLin(TabiQlQlfr)
  return(TabiQualifr)
}


TabelaQlQnfr <- function(w,z){
  TabelaQlQnfr <- table(w, QuantAux(z))/length(w)
  TabelaQlQnfr <- TotalColLin(TabelaQlQnfr)
  return(TabelaQlQnfr)
}


TabelaQnQlfr <- function(w,z){
  TabelaQnQlfr <- table(QuantAux(w), z)/length(w)
  TabelaQnQlfr <- TotalColLin(TabelaQnQlfr)
  return(TabelaQlQnfr)
}


TabelaQnQnfr <- function(w,z){
  Quantitfr <- table(QuantAux(w),QuantAux(z))/length(w)
  Quantitfr <- TotalColLin(Quantitfr)
  return(Quantitfr)
}


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

PropPorColunaQnQl <- function(w,z){
  Porcoluna <- table(QuantAux(w),z)
  Porcoluna <- prop.table(Porcoluna,2)
  return(Porcoluna)
}

PropPorColunaQnQn <- function(w,z){
  Porcoluna <- table(QuantAux(w),QuantAux(z))
  Porcoluna <- prop.table(Porcoluna,2)
  return(Porcoluna)
}



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

PropPorLinhaQnQl <- function(w,z){
  Porlinha <- table(QuantAux(w),z)
  Porlinha <- prop.table(Porlinha,1)
  return(Porlinha)
}

PropPorLinhaQnQn <- function(w,z){
  Porlinha <- table(QuantAux(w),QuantAux(z))
  Porlinha <- prop.table(Porlinha,1)
  return(Porlinha)
}



######################## Tabela bivariada final ##########


TabelaBivariada <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    TabelaQlQl(w,z)
      } else { 
        if (is.factor(w) & !is.factor(z)){
          TabelaQlQn(w,z)
            } else {
              if (!is.factor(w) & is.factor(z)){
                TabelaQnQl(w,z)
                  } else {
                    TabelaQnQn(w,z)
      }
    }
  }
}
######################## Tabela bivariada relativa final ##########

TabelaBivariadaRelativa <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    TabelaQlQlfr(w,z)
      } else { 
        if (is.factor(w) & !is.factor(z)){
          TabelaQlQnfr(w,z)
            } else {
              if (!is.factor(w) & is.factor(z)){
                TabelaQnQlfr(w,z)
                  } else { 
                    TabelaQnQnfr(w,z)
   }
  }
 }
}



################################ Proporção pro coluna final ##############
ProporcaoPorColuna <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    PropPorColunaQlQl(w,z) 
      } else { 
        if (is.factor(w) & !is.factor(z)){
          PropPorColunaQlQn(w,z)
            } else { 
              if (!is.factor(w) & is.factor(z)){
                PropPorColunaQnQl
                  } else {
                    PropPorColunaQnQn(w,z)}
    }
  }
}
PropPorColuna(mtcars$qsec,mtcars$wt)  


#################################### Proporção por linha final ##################
ProporcaoPorLinha <- function(w,z){
  if (is.factor(w) & is.factor(z)){
    PropPorLinhaQlQl(w,z) 
      } else { 
        if (is.factor(w) & !is.factor(z)){
          PropPorLinhaQlQn(w,z)
            } else { 
              if (!is.factor(w) & is.factor(z)){
                PropPorLinhaQnQl
                  } else {
                    PropPorLinhaQnQn(w,z)}
    }
  }
}
PropPorLinha(mtcars$wt,mtcars$qsec)  

##################################### Novas funções ###########################

# TabelaBi <- function(w,z){
#   Tabelabi <- table(w,z)
#   Tabelabi <- TotalColLin(Tabelabi)
#   return(Tabelabi)
# }
# 
# 
# 
# TabelaBifr <- function(w,z){
#   Tabelabifr <- table(w,z)/length(x)
#   Tabelabifr <- TotalColLin(Tabelabifr)
#   return(Tabelabifr)
# }
# 
# 
# PropPorColuna <- function(w,z){
#   Porcoluna <- table(w,z)
#   Porcoluna <- prop.table(Porcoluna,2)
#   return(Porcoluna)
# }
# 
# 
# PropPorLinha <- function(w,z){
#   Porlinha <- table(w,z)
#   Porlinha <- prop.table(Porlinha,1)
#   return(Porlinha)
# }
# 
# 
# TabelaBivariada <- function(w,z,arg = c(1,2,3,4)){
#  if (!is.factor(w)){
#   a <- QuantAux(w)
#    } else {
#     if (!is.factor(z)){
#      b <- QuantAux(z)
#       } else {
#         if(is.factor(w)){
#           a <- w
#            } else {
#             if(is.factor(z)){
#              b <- z
#               } else {
#                if (arg == 1){
#                 TabelaBi(a,b)
#                  } else {
#                    if(arg == 2){
#                      TabelaBifr(a,b)
#                       } else {
#                        if(arg == 3){
#                         PropPorColuna(a,b)
#                          } else {
#                           if(arg == 4){
#                            PropPorLinha(a,b)
#                           }
#        }
#       }
#      }
#     }
#    }
#   }
#  }
# }
# 
