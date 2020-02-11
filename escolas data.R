basepath <- "C:/Users/João/Documents/Projeto PIB Municipal/microdados_educacao_basica_2019"
setwd(basepath)
dados <- read.csv(file = "turmas_sg.csv", sep = ',')
dados <- subset(dados,dados$IN_EJA=='0')
dados <- subset(dados,dados$TP_MEDIACAO_DIDATICO_PEDAGO=='1')
dados2 <- subset(dados,dados$TP_ETAPA_ENSINO %in% c('1','2','3')==FALSE)
ef <- c('14','15','16','17','18','19','20','21','22','23','41','56')
em <- c('25','26','27','28','29','30','31','32','33','34','35','36',
        '37','38')
tec <- c('39','40','64')
nivel <- c()
for(i in 1:nrow(dados2)){
  if(dados2$TP_ETAPA_ENSINO[i] %in% ef == TRUE){
    nivel[i] <- 'Fundamental'
  }else{
    if(dados2$TP_ETAPA_ENSINO[i] %in% em == TRUE){
      nivel[i] <- 'Médio'
    }else{
      if(dados2$TP_ETAPA_ENSINO[i] %in% tec == TRUE){
        nivel[i] <- 'Técnico'
      }}}}
dados2$nivel <- nivel
nrow(subset(dados2,dados2$nivel == 'Técnico'))
base <- data.frame(dados2$QT_MATRICULAS,dados2$nivel,
                   dados2$CO_ENTIDADE)
names(base) <- c('matriculas','nivel','CO_ENTIDADE')
escolas <- read.csv('dados escolares sg filtrados.csv')
qt_mat <- c()
for(i in 1:nrow(escolas)){
  qt_mat[i] <- sum(as.numeric(subset(base,
          base$CO_ENTIDADE == escolas$cod_esc[i])$matriculas))
}
qt_mat
escolas$matriculas <- qt_mat

escolas_filt <- subset(escolas,escolas$matriculas != 0)
escolas_filt$maq <- escolas_filt$notebook + escolas_filt$desktop

for(i in 1:nrow(escolas_filt)){
  if(escolas_filt$maq[i] == 0){
    escolas_filt$maq_dum[i] <- 0
  }else{
    escolas_filt$maq_dum[i] <- 1
  }}

escolas_filt$maq_web <- escolas_filt$maq_dum * escolas_filt$internet
escolas_filt$prop <- escolas_filt$matriculas / escolas_filt$maq
mean(escolas_filt$prop)
esc_comp <- subset(escolas_filt,escolas_filt$maq != 0)
mean(esc_comp$prop)
library(ggplot2)

medias <- c()
rede <- c('Estadual','Federal','Municipal','Privada')
for(i in 1:length(rede)){
  nome <- rede[i]
  medias$med[i] <- as.numeric(mean(subset(esc_comp,
                                          esc_comp$rede == nome)$prop))
}
medias <- data.frame(medias)
medias$rede <- rede
md <- mean(esc_comp$prop)
round(medias$med,2)

library(ggrepel)
windowsFonts("Bookman" = windowsFont("Bookman Old Style"))
ggplot(data = medias,aes(x = rede,y = med,label=round(med,2))) +
  geom_hline(aes(yintercept = md,
              linetype = paste('Proporção média do município - ',
                               round(md,0),
                               ' alunos/computador',sep='')),
             color = 'purple', size = 1) +
  geom_col(fill = 'tomato') + coord_flip() +
  scale_linetype_manual(name = "", values = 2, 
                        guide = guide_legend(override.aes = list(color = c("purple")))) +
                        labs(title = 'Alunos por computador em Unidades\nEscolares de São Gonçalo-RJ',
              subtitle = 'Segundo o Censo Escolar',
              fill = '',x = '', y ='',
              caption = 'Fonte:MEC-2019\nElaboração:Johann Marques') +
  geom_text(hjust=1,size = 9.5,color='white') + theme_minimal() +
  theme(axis.ticks.x=element_line(),
        axis.ticks.y=element_line(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14,vjust=0.3),
    plot.title = element_text(size = 38, colour = 'gray25', family = 'Bookman'),
               plot.subtitle = element_text(size = 27, colour = 'gray35', family = 'Bookman'),
               legend.text = element_text(size = 20, colour = 'gray35', family = 'Bookman'),
               plot.caption = element_text(size = 20, colour = 'gray35', family = 'Bookman'),
              legend.position = 'bottom')

mean(esc_comp$prop)

mean(subset(esc_comp,esc_comp$rede=='Estadual')$prop)
mean(subset(esc_comp,esc_comp$rede=='Federal')$prop)
mean(subset(esc_comp,esc_comp$rede=='Municipal')$prop)
mean(subset(esc_comp,esc_comp$rede=='Privada')$prop)


df <- c()
for(i in 1:length(rede)){
  nome <- rede[i]
  atual <- subset(escolas_filt,escolas_filt$rede == nome)
  df$verde[i] <- as.numeric(mean(atual$verde))
  df$lab_info[i] <- as.numeric(mean(atual$lab_info))
  df$lab_ciencias[i] <- as.numeric(mean(atual$lab_ciencias))
  df$internet[i] <- as.numeric(mean(atual$internet))
  }
df$rede <- rede
df <- data.frame(df)
df
names(df) <- c('verde','info','ciencias','internet','rede')
df2 <- c()
df2$value <- c(df$verde,df$info,df$ciencias,df$internet) 
df2 <- data.frame(df2)
df2$variable <- c('Área Verde','Área Verde','Área Verde','Área Verde',
                  'Laboratório de Informática',
                  'Laboratório de Informática',
                  'Laboratório de Informática',
                  'Laboratório de Informática',
                  'Laboratório de Ciências',
                  'Laboratório de Ciências',
                  'Laboratório de Ciências',
                  'Laboratório de Ciências',
                  'Internet para aprendizagem',
                  'Internet para aprendizagem',
                  'Internet para aprendizagem',
                  'Internet para aprendizagem')
df2
df2$type <- c('Estadual','Federal','Municipal','Privada',
              'Estadual','Federal','Municipal','Privada',
              'Estadual','Federal','Municipal','Privada',
              'Estadual','Federal','Municipal','Privada')
df2
df
library(scales)
cores <- c('Área Verde' = 'chartreuse2',
           'Laboratório de Informática' = 'purple',
           'Laboratório de Ciências' = 'skyblue',
           'Internet para aprendizagem' = 'orange')
ggplot(data = df2,aes(x = reorder(type,value), label = percent(value),
                      fill = variable, y = value)) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = 1), size = 8,
            hjust = 1, color = 'white',fontface='bold') + coord_flip() +
  scale_fill_manual(values = cores) + theme_minimal() +
  labs(title = 'Estrutura em Unidades Escolares\nde São Gonçalo-RJ',
                                        subtitle = 'Segundo o Censo Escolar',
                                        fill = '', x = '', y = '',
                                        caption = 'Fonte:MEC-2019\nElaboração:Johann Marques') +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size = 14),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size = 38, colour = 'gray25', family = 'Bookman'),
        plot.subtitle = element_text(size = 27, colour = 'gray35', family = 'Bookman'),
        legend.text = element_text(size = 20, colour = 'gray35', family = 'Bookman'),
        plot.caption = element_text(size = 20, colour = 'gray35', family = 'Bookman'))

for(i in 1:length(rede)){
  nome <- rede[i]
  atual <- subset(escolas_filt,escolas_filt$rede == nome)
  df$unids[i] <- as.numeric(nrow(atual)/nrow(escolas_filt))
  df$matr[i] <- 
    as.numeric(sum(atual$matriculas)/sum(escolas_filt$matriculas))
  }
df
library(dplyr)
count.data <- data.frame(df$rede,df$unids)
names(count.data) <- c('rede','unids')
count.data %>% arrange(desc(unids))
count.data
count.data <- count.data %>%
  arrange(desc(rede)) %>%
  mutate(lab.ypos = cumsum(unids) - 0.5*unids)
count.data

count.data2 <- data.frame(df$rede,df$matr)
names(count.data2) <- c('rede','matr')
count.data2 %>% arrange(desc(matr))
count.data2
count.data2 <- count.data2 %>%
  arrange(desc(rede)) %>%
  mutate(lab.ypos = cumsum(matr) - 0.5*matr)
count.data2

library(ggrepel)

cores_2 <- c('Federal' = 'salmon4',
             'Estadual' = 'tomato3',
             'Municipal' = 'tomato',
             'Privada' = 'salmon')

ggplot(count.data,aes(x = '',y = unids,fill = rede)) + geom_col() +
  coord_polar('y',start = 0) + scale_fill_manual(values = cores_2) +
  labs(title = 'Unidades Escolares de São Gonçalo-RJ\npor rede de ensino',
       subtitle = 'Segundo o Censo Escolar',
       fill = '',x = '', y ='',
       caption = 'Fonte:MEC-2019\nElaboração:Johann Marques') +
  geom_text(aes(y = lab.ypos, label = percent(unids)),size = 8,
            color = 'white',fontface='bold') +
  theme_minimal() + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size = 14),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size = 38, colour = 'gray25', family = 'Bookman'),
        plot.subtitle = element_text(size = 27, colour = 'gray35', family = 'Bookman'),
        legend.text = element_text(size = 20, colour = 'gray35', family = 'Bookman'),
        plot.caption = element_text(size = 20, colour = 'gray35', family = 'Bookman'))

ggplot(count.data2,aes(x = '',y = matr,fill = rede)) + geom_col() +
  coord_polar('y',start = 0) + scale_fill_manual(values = cores_2) +
  labs(title = 'Proporção das matrículas em Unidades\nEscolares de São Gonçalo-RJ',
       subtitle = 'Segundo o Censo Escolar',
       fill = '',x = '', y ='',
       caption = 'Fonte:MEC-2019\nElaboração:Johann Marques') +
  geom_text(aes(y = lab.ypos, label = percent(matr)),size = 8,
            color = 'white',fontface='bold') +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_text(size = 14),
                          axis.ticks.x=element_blank(),
                          plot.title = element_text(size = 38, colour = 'gray25', family = 'Bookman'),
                          plot.subtitle = element_text(size = 27, colour = 'gray35', family = 'Bookman'),
                          legend.text = element_text(size = 20, colour = 'gray35', family = 'Bookman'),
                          plot.caption = element_text(size = 20, colour = 'gray35', family = 'Bookman'))
