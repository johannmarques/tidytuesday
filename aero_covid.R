library(BatchGetSymbols)
windowsFonts("Bookman" = windowsFont("Bookman Old Style"))
acao1<-c('AZUL4.SA','GOLL4.SA','CVCB3.SA')
bg<-'2020-02-01' 
lst<- '2020-02-29'
bench<-'^BVSP' 
data2<-BatchGetSymbols(tickers = acao1,bench.ticker = bench,
                       first.date = bg,last.date = lst)
df <- data.frame(data2$df.tickers)
df1 <- data.frame(df$ticker,df$price.close,df$ref.date,
                  df$ret.adjusted.prices)
names(df1) <- c('acao','preco_fechamento','dia','var')
df1 <- na.omit(df1)
df1
library(dplyr)
library(ggplot2)

cores <- c('AZUL4.SA' = 'dodgerblue', 'CVCB3.SA' = 'yellow',
           'GOLL4.SA' = 'orange')
df1$acao <- factor(df1$acao, levels = c('AZUL4.SA','GOLL4.SA','CVCB3.SA'))

ggplot(df1,aes(x = dia, y = preco_fechamento, group = acao,
               colour = acao)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(aes(xintercept=as.Date('2020-02-26')), size = 1,
                           colour = 'red', linetype = 'dashed') +
  scale_colour_manual(values = cores) +
  labs(title = 'Cotações diárias da AZUL4, GOLL4 e CVCB3',
       subtitle = 'Período de Fevereiro (em Reais)',
       colour = '', x = '', y = '',
       caption = 'Fonte:B3\nElaboração:Johann Marques') + theme_minimal() +
  theme(axis.text.x=element_text(size = 14),
        axis.text.y=element_text(size = 14),
        plot.title = element_text(size = 38, colour = 'gray25', family = 'Bookman'),
        plot.subtitle = element_text(size = 27, colour = 'gray35', family = 'Bookman'),
        legend.text = element_text(size = 20, colour = 'gray35', family = 'Bookman'),
        plot.caption = element_text(size = 20, colour = 'gray35', family = 'Bookman'))


