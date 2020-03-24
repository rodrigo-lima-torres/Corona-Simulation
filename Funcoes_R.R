atualizar.direcao <- function(dados) {
  # Definindo a direção e a nova posição dos que estão em casa
  ind.casa <- which(dados$casa==T)
  ind.sair <- ind.casa[sample(c(T, F), length(ind.casa), 
                              prob=c(p.sair.casa, 1-p.sair.casa),
                              replace=T)]
  # Calcula o angulo (em radianos) entre a posição atual e o centro da cidade
  angulo.aux <- atan2(centro[1] - dados$lat[ind.sair], 
                      centro[2] - dados$long[ind.sair])
  dados$direc[ind.sair] <- rnorm(length(ind.sair), angulo.aux, angulo.sigma)
  
  
  # Definindo a situação dos que estão fora de casa em movimento
  ind.fora.move <- which(dados$casa==F & dados$direc!=0)
  aux.fora.move <- ind.fora.move[sample(c(T, F), 
                                        length(ind.fora.move), 
                                        prob=c(p.parar.fora, 1-p.parar.fora),
                                        replace=T)]
  dados$direc[aux.fora.move] <- 0  # Parando os individuos selecionados
  
  
  # Definindo a situação dos que estão parados fora de casa
  ind.fora.parado <- setdiff(which(dados$casa==F), ind.fora.move)
  aux.fora.parado <- ind.fora.parado[sample(c(T, F), 
                                            length(ind.fora.parado), 
                                            prob=c(p.voltar.casa, 1-p.voltar.casa),
                                            replace=T)]
  # Calcula o angulo (em radianos) entre a posição atual e o centro da cidade
  angulo.aux <- atan2(casas$lat[aux.fora.parado] - dados$lat[aux.fora.parado], 
                      casas$long[aux.fora.parado] - dados$long[aux.fora.parado])
  dados$direc[aux.fora.parado] <- angulo.aux # Levando os individuoas de volta pra casa
  return(dados$direc)
}


atualizar.posicao <- function(dados) {
  ### Atualizar a posição dos indivíduos
  delta <- tamanho.passo * cbind(sin(dados$direc), 
                                 cos(dados$direc))
  delta[dados$direc==0, ] <- 0  # casos em que a direção é nula não devem se mover 
  dados[, c("lat", "long")] <- dados[, c("lat", "long")] + delta
  return(dados[, c("lat", "long")])
}


atualizar.casa <- function(dados) {
  ### Atualizar a coluna casa
  distancia.casa <- sqrt(rowSums((dados[, c("lat", "long")] - casas[, c("lat", "long")])^2))
  estao.casa <- which(distancia.casa < tamanho.passo)
  dados[estao.casa, c("lat", "long")] <- casas[estao.casa, c("lat", "long")]
  dados$casa <- (1:n.pop) %in% estao.casa
  return(dados$casa)
}


atualizar.risco <- function(dados) {
  ### Atualizar a superficie de risco
  tmp <- dados %>%
    filter(status==1)
  aux <- kde2d(tmp$long, tmp$lat, h=rep(raio.risco, 2), n=n.grid, 
               lims=c(range(long.extremos), range(lat.extremos)))
  superficie[, 2+tempo] <- superficie[, 3+tempo-2]*taxa.decaimento + 
    log(pmax(1, as.vector(t(aux$z))))
  return(superficie[, 2+tempo])
}


atualizar.status <- function(dados) {
  ### Atualizar a coluna status
  # Simulando infecções
  saudaveis <- dados$ind[dados$status==0]
  for(ind in saudaveis) {
    indice.lat <- which.min(abs(dados$lat[ind] - grid.lat))
    lat.mais.proxima <- unique(grid.lat[indice.lat])
    indices <- superficie[, 1] == lat.mais.proxima  # o tempo para essa operação é um desastre!
    # Para melhorar, é possível criar previamente uma matriz com os indices para cada latitude
    indice.long <- which.min(abs(dados$long[ind] - superficie[indices, 2]))
    risco <- superficie[indice.long, tempo+2]  # Nível de contaminação do ambiente
    p.infec <- 2*((1/(1+exp(-escala.risco*risco)))-.5)  # Probabilidade de infecção
    dados$status[ind] <- sample(1:0, 1, prob=c(p.infec, 1-p.infec))
  }
  
  # Simulando recuperações
  contaminados <- setdiff(1:n.pop, saudaveis)
  aux <- sample(c(T, F), length(contaminados), 
                    prob=c(p.recuperar, 1-p.recuperar),
                    replace=T)
  recuperados <- contaminados[aux]
  dados$status[recuperados] <- 2
  return(dados$status)
}
