### Carregando os pacotes necessários
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "MASS")
source("Funcoes_R.R")
set.seed(123) # Definindo a semente

### Definindo os parâmetros do modelo
TT <- 100 # 172800 # Número de passos (tempo)

### Parametros da cidade
n.pop <- 4600 # Tamanho da população
centro <- c(lat=-20.902805, long=-45.127847)
lat.extremos <- c(lat.min=-20.908992, lat.max=-20.897918)
long.extremos <- c(long.min=-45.132165, long.max=-45.123509)

### Parametros de deslocamento
p.sair.casa <- .01 # Probabilidade de uma pessoa em casa sair no minuto em questão
p.voltar.casa <- .01 # Probabilidade de uma pessoa parada fora de casa entrar em movimento de retorno
p.parar.fora <- .05 # Probabilidade de uma pessoa em movimento fora de casa parar
# A probabilidade abaixo induz uma distribuição geométrica do "número de movimentos até parar"
p.continuar <- .8  # Probabilidade de permanecer em movimento.
# Desvio padrão do ruído que desloca a direção de movimentação em relação ao centro da cidade
angulo.sigma <- 1  # Quanto maior o valor menor é a chance dos moradores irem para o centro
# .0001 é aproximadamente 30 metros
tamanho.passo <- .001 # Velocidade do deslocamento por unidade de tempo (minuto) 


### Parametros de disseminação do vírus
taxa.decaimento <- .999  # Taxa com que a contaminação do ambiente é preservada no tempo seguinte
raio.risco <- .00005  # parâmetro que define o rio de contaminação ao redor de um indivíduo
escala.risco <- .001  # valor de relaciona o risco (nível de contaminação) à probabilidade de infecção 
n.grid <- 100  # Número de pontos no grid de estimação da superficie de risco
p.recuperar <- .0001  # Probabilidade de uma pessoa infectada se recuperar em um instante de tempo


### Criando a lista que irá receber os dados simulados
dados <- vector(TT, mode="list")


### Definindo os locais de residência
casas <- data.frame(
  ind=1:n.pop,
  lat=runif(n.pop, lat.extremos[1], lat.extremos[2]),
  long=runif(n.pop, long.extremos[1], long.extremos[2])
)
# Induzir pessoas a morar juntas
casas[1:n.pop, ] <- casas[sample(n.pop/3, n.pop, replace=T), ]  


### Criando a população no tempo 1
dados[[1]] <- data.frame(
  ind=1:n.pop,
  lat=c(casas$lat[1:(n.pop/2)], 
        runif(n.pop/2, lat.extremos[1], lat.extremos[2])),
  long=c(casas$long[1:(n.pop/2)], 
         runif(n.pop/2, long.extremos[1], long.extremos[2])),
  casa=c(rep(1, n.pop/2), rep(F, n.pop/2)),
  status=c(rep(1, 10), rep(0, n.pop-10)), # 0=saudável, 1=doente, 2=recuperado
  # trocar a linha abaixo para mover as pessoas como na função atualizar.direcao 
  direc=c(rep(0, 3*n.pop/4), runif(n.pop/4, 0, 2*pi))  
) 


### Criando a superficie de risco
superficie <- matrix(nr=n.grid^2, nc=2+TT) # Superficie de risco
colnames(superficie) <- c("lat", "long", paste0("densidade_", 1:TT))
tmp <- dados[[1]] %>%
  filter(status==1)
aux <- kde2d(tmp$long, tmp$lat, h=rep(raio.risco, 2), n=n.grid, 
             lims=c(range(long.extremos), range(lat.extremos)))
superficie[, 1:2] <- unlist(with(aux, expand.grid(y, x)))
superficie[, 3] <- log(pmax(1, as.vector(t(aux$z))))
grid.lat <- unique(superficie[, 1])
grid.long <- unique(superficie[, 2])


### Simulando a evolução da população
for(tempo in 2:TT) {
  dados[[tempo]] <- dados[[tempo-1]]
  dados[[tempo]]$direc <- atualizar.direcao(dados[[tempo]])
  dados[[tempo]][, c("lat", "long")] <- atualizar.posicao(dados[[tempo]])
  dados[[tempo]]$casa <- atualizar.casa(dados[[tempo]])
  superficie[, 2+tempo] <- atualizar.risco(dados[[tempo]])
  dados[[tempo]]$status <- atualizar.status(dados[[tempo]])
}



# Visualizando a população no instante inicial
tempo=1
superficie[, c(1:2, 2+tempo)] %>%
  as.data.frame() %>% 
  rename(densidade=starts_with("dens")) %>% 
  filter(densidade>0) %>% 
  ggplot(aes(long, lat)) + 
  geom_point(aes(alpha=densidade)) +
  geom_point(aes(color=as.factor(status), shape=as.factor(casa)), 
             data=dados[[tempo]][n.pop:1, ]) 

