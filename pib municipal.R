basepath <- 'C:/Users/João/Documents/Projeto PIB Municipal'
setwd(basepath)
data <- read.csv('PIBv2.csv',sep = ';')
data_2017 <- subset(data,data$Ano == '2017')
data_2017_rj <- subset(data_2017,
                       data_2017$Sigla.da.Unidade.da.Federação == 'RJ')
dados_rj <- data.frame(data_2017_rj$Nome.do.Município,
                       data_2017_rj$Nome.da.Mesorregião,
                       data_2017_rj$Produto.Interno.Bruto..a.preços.correntes..R..1.000.,
                       data_2017_rj$Produto.Interno.Bruto.per.capita..a.preços.correntes..R..1.00.)
names(dados_rj) <- c('Municipio','Mesorregiao','PIB','PIBpc')
dados_rj$PIB <- as.numeric(gsub('\\.','',dados_rj$PIB))
dados_rj$PIBpc <- as.numeric(gsub('\\,','.',gsub('\\.','',dados_rj$PIBpc)))
dados_rj$PIBpc_k <- dados_rj$PIBpc/1000
dados_rm <- subset(dados_rj,
                   dados_rj$Mesorregiao == 'Metropolitana do Rio de Janeiro')
reg <- c()
grande_rio <- c('Rio de Janeiro','São Gonçalo','Duque de Caxias',
                'Nova Iguaçu','Niterói','Belford Roxo',
                'São João de Meriti','Petrópolis','Magé','Itaboraí',
                'Mesquita','Nilópolis','Maricá','Queimados','Itaguaí',
                'Japeri','Seropédica','Rio Bonito','Guapimirim',
                'Cachoeiras de Macacu','Paracambi','Tanguá')
                

for(i in 1:nrow(dados_rj)){
  if(dados_rj$Municipio[i] %in% grande_rio == TRUE){
    reg[i] <- 'Metropolitana'}
  else{
    reg[i] <- 'Foda-se'
  }}
dados_rj$reg <- reg
dados_rj

library(ggplot2)
ggplot(dados_rj,aes(x=reorder(Municipio,PIBpc_k),y=PIBpc_k)) + geom_col() + coord_flip()
library(geobr)
library(dplyr)
rj <- read_municipality(code_muni = 'RJ',year = 2018)
rj$name_muni <- gsub(' De',' de',gsub(' Do',' do',gsub(' Da',' da',
                    rj$name_muni)))
ggplot() + geom_sf(data = rj, color = 'white', fill = 'black')
resumo <- data.frame(dados_rj$Municipio,dados_rj$PIBpc_k,dados_rj$PIB)
names(resumo) <- c('name_muni','PIBpc_k','PIB')
resumo$pop <- as.numeric(resumo$PIB/(resumo$PIBpc_k))
rj_pib <- left_join(rj,resumo,by = 'name_muni')
rj_pib                    
resumo

library(ggrepel)
ppc <- ggplot(data = rj_pib, aes(fill = PIBpc_k,label = name_muni)) +
  geom_sf(data = rj_pib, color = 'white') +
  scale_fill_continuous(high = 'royalblue4',low = 'grey80') +
  labs(title = 'PIB per capita dos Municípios do Rio de Janeiro',
        subtitle = 'em milhares de Reais',
        fill = '',
        caption = 'PIB dos Municípios - IBGE 2017, Johann Marques') +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.subtitle = element_text(color="grey", size=12))

intervalo <- c()
for(i in 1:nrow(rj_pib)){
  if(rj_pib$pop[i] > 6*10^6){
    intervalo[i] <- '+ 6 milhões'
  }else
    if(rj_pib$pop[i] > 2*10^6){
      intervalo[i] <- '2-6 milhões'
    }else
      if(rj_pib$pop[i] > 10^6){
        intervalo[i] <- '1-2 milhões'
      }else
        if(rj_pib$pop[i] > 5*10^5){
          intervalo[i] <- '500.000-1 milhão'
        }else
          if(rj_pib$pop[i] > 10^5){
            intervalo[i] <- '100.000-500.000'
          }else
            if(rj_pib$pop[i] > 5*10^4){
              intervalo[i] <- '50.000-100.000'
            }else
              if(rj_pib$pop[i] < 5*10^4){
                intervalo[i] <- 'Menor que 50.000'
              }}

rj_pib$leg <- intervalo
rj_pib
colors <- c('+ 6 milhões'='orange','2-6 milhões'='royalblue4',
            '1-2 milhões'='royalblue1','500.000-1 milhão'='steelblue2',
            '100.000-500.000'='lightsteelblue4',
            '50.000-100.000'='lightsteelblue2',
            'Menor que 50.000'='grey')         
populacao <- ggplot(data = rj_pib, aes(fill = intervalo,label = name_muni)) +
  geom_sf(data = rj_pib, color = 'white') +
  scale_fill_manual(values = colors) +
  labs(title = 'População dos Municípios do Rio de Janeiro',
       fill = '',
       caption = 'Johann Marques') +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
populacao
ggsave('PIB per capita RJ.png',plot = ppc)
ggsave('População municípios.png',plot = populacao)


