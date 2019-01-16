

Tabelaux <- function(y){
  tabelaux   <-  table(y)
  tabelaux <- cbind(tabelaux, tabelaux/length(y))
  colnames(tabelaux) <- c("f", "fr")
  return(tabelaux)
}

TabelaNominal <- function(y){
  tabelanom   <-  Tabelaux(y)
  total <- colSums(tabelanom)
  tabelanom <- rbind(tabelanom, total)
  tabelanom <- round(tabelanom,3)
  return(tabelanom)
}

TabelaOrdinal <- function(y){
  tabelaux <- Tabelaux(y)
  tabelaord <- cbind(tabelaux, apply(tabelaux, 2, cumsum))
  tabelaord <- round(tabelaord, 3)
  total <- c(colSums(tabelaux), "-", "-")
  tabelaord <- rbind(tabelaord, total)
  colnames(tabelaord)  <-  c("f", "fr", "F", "Fr")
  return(tabelaord)
}

TabelaQuantitativa <- function(y){
  histy <- hist(as.numeric(y), plot=F)
  intervclasse <- cut(y, breaks = histy$breaks, include.lowest=TRUE)
  tabelaquanti <- TabelaOrdinal(intervclasse)
  return(tabelaquanti)
}

TabelaFrequenciaSimples <- function(y){
  if (is.factor(y) & !is.ordered(y)){
    TabelaNominal(y)
  } else {
    if (is.factor(y) & is.ordered(y)){
      TabelaOrdinal(y)
    } else {
      if (nrow(table(y)) <= 15){
        TabelaOrdinal(y)
      } else { 
        TabelaQuantitativa(y)}
    } 
  }
}