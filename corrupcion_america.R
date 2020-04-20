rm(list=ls())

library(rvest)
library(dplyr)
library(xts)
library(tidyr)

##WEB SCRAPPING, fuente: 'datosmacro.expansion.com'

countries <- c(Argentina= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/argentina',
               Bolivia= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/bolivia',
               Brasil= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/brasil',
               Chile= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/chile',
               Colombia= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/colombia',
               Ecuador= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/ecuador',
               Guyana= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/guyana', 
               Perú= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/peru',
               Paraguay= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/paraguay',
               Uruguay= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/uruguay',
               Venezuela= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/venezuela',
               Surinam= 'https://datosmacro.expansion.com/estado/indice-percepcion-corrupcion/surinam')


html_list <- list()

table_list <- list()

for (i in 1:length(countries)){
  html_list[[i]] <- read_html(countries[i])
  
  table_list[[i]] <- as.data.frame(html_list[[i]] %>%
                           html_table())
  
  table_list[[i]]$Pais <- as.character(attributes(countries[i]))
  
  table_list[[i]]$Ranking_Regional <- 0
  
  names(table_list[[i]]) <- c('Fecha','Ranking Mundial','Indice de Corrupcion','Pais','Ranking Regional') 
  
  
}

binded <- bind_rows(table_list)

binded <- binded %>%
              group_by(Fecha)%>%
              arrange(`Indice de Corrupcion`)%>%
              mutate(`Ranking Regional` = seq_along(`Ranking Regional`))%>%
              arrange(desc(Fecha))

binded$`Indice de Corrupcion` <- 100-binded$`Indice de Corrupcion`

binded <- as.data.frame(binded)

##manipulate 'binded' to be xts-friendly.

binded_xts_base <- binded[c(1,3,4)] %>% 
  filter(!(Pais %in% c('Surinam','Guyana')))%>%
  spread(Pais,value = `Indice de Corrupcion`)

binded_xts_base$Fecha <- as.Date(paste(as.character(binded_xts_base$Fecha),12,31,sep='-'))

binded_xts <- xts(binded_xts_base[-1], order.by = binded_xts_base$Fecha)

sapply(binded_xts,function(x) sum(is.na(x)))

binded_xts <- na.locf(binded_xts)

binded_xts <- na.locf(binded_xts,fromLast = T)
       
#plot time series

colors <- c('blue','green','light blue','yellow','black','gray','red','pink','dark red','dark blue')
par(xpd=T, mar=par()$mar+c(0,0,0,6))

plot(binded_xts,main =str_wrap('Percepción de corrupción del sector público por parte de sus habitantes, 100 (percepción de altos niveles de corrupción) 
a 0',width = 40),
     xlab = 'Fecha',ylab='Indice de Corrupcion',col = colors,bty = 'L',
     lty = c(1,2), xaxt="n")
  
xts::addLegend(9,0.5,legend.names=sort(colnames(binded_xts)), col=colors, text.col=colors
 ,bg="white", bty=1, horiz=FALSE, ncol = 3, cex = 0.6, xpd = TRUE,lty = c(1,2))


boxplot_graph <- as.data.frame(binded_xts)

boxplot_graph <- boxplot_graph %>%
  gather(key = Pais, value = Indice)

library(ggplot2)

ggplot(boxplot_graph,aes(x = Pais, y = Indice))+
  geom_boxplot()+
  scale_y_continuous(breaks = seq(10,max(boxplot_graph$Indice), by = 10))

#plot mean and acumulated results for South America

indice_anual_LATAM_list <- data.frame(Year = '', Acum_Index = '', stringsAsFactors = FALSE)

for(i in 1:nrow(binded_xts)){
  
  indice_anual_LATAM_list[i,2] <- sum(binded_xts[i,])
  indice_anual_LATAM_list[i,1] <- as.numeric(substr(index(binded_xts)[i],start = 1, stop = 4))
  
  
}

indice_anual_LATAM_list$Year <- as.numeric(indice_anual_LATAM_list$Year)
indice_anual_LATAM_list$Acum_Index <- as.numeric(indice_anual_LATAM_list$Acum_Index)

ggplot(indice_anual_LATAM_list,aes(x = Year, y = Acum_Index, group = 1))+
  geom_line()+
  geom_point(col = 'red')+
  ylab('Indice de corrupcion')+
  xlab('Año')+  
  ggtitle('Indice de corrupcion ACUMULADO ANUAL LATAM')+
  theme(plot.title = element_text(hjust = 0.5))


indice_prom_LATAM_list <- data.frame(Year = '', prom = '', stringsAsFactors = FALSE)

for(i in 1:nrow(binded_xts)){
  
  indice_prom_LATAM_list[i,2] <- round(mean(binded_xts[i,]))
  indice_prom_LATAM_list[i,1] <- as.numeric(substr(index(binded_xts)[i],start = 1, stop = 4))
  
  
}


indice_prom_LATAM_list$Year <- as.numeric(indice_prom_LATAM_list$Year)
indice_prom_LATAM_list$prom <- as.numeric(indice_prom_LATAM_list$prom)

ggplot(indice_prom_LATAM_list,aes(x = Year, y = prom, group = 1))+
  geom_line()+
  geom_point(col = 'red')+
  ylab('Indice corrupcion')+
  xlab('Año')+
  ggtitle('Promedio de indice de corrupcion LATAM')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(min(indice_prom_LATAM_list$prom),max(indice_prom_LATAM_list$prom),by = 1))+
  scale_x_continuous(breaks = seq(min(indice_prom_LATAM_list$Year),max(indice_prom_LATAM_list$Year),by = 1))
  


