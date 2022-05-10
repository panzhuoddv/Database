# email: qfsfdxlqy@163.com
# GitHub: https://github.com/2015qyliang

library(ggplot2)
library(maps)


map <- function(superfamily){
  path=(superfamily,".csv",sep="")
# make Run location dataset
  all.run.set <- read.csv(path,header = T)
  all.run.set <- all.run.set[order(all.run.set[,3],decreasing = T),]
  bradymonadales.yes <- all.run.set[1:501,]

  a<-bradymonadales.yes[,6]
  b<-function(i){
    switch(i,
           "Animal" = "#AFD888",
           "Plant" = "#63ADD0",
           "Saline" = "#D836C4",
           "Non-saline" = "#FF8F40")
  }
  c<-lapply(a,b)
  
  # make map figure
  worldmap <- map_data("world")
  p <<- ggplot() + 
    theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
    geom_polygon(data = worldmap,aes(x =long, y = lat, group = group),
                 fill = "grey89",colour="grey40",size = 0.05)+
    geom_point(data = bradymonadales.yes,
                      mapping = aes_string(x = "longitude_deg", y = "latitude_deg",size = superfamily,
                                    fill = "empo_2"),
                      alpha = 0.8,shape = 21,color = c,stroke = 0.05)+
    scale_fill_manual(values = c("#AFD888", "#63ADD0", "#D836C4", "#FF8F40"),
                             name = "Node color for sample environmental type",
                             limits=c("Animal", "Plant", "Saline", "Non-saline"))+

  #p <- p + scale_size_discrete(labels = c("0.1","0.5","1","3"))
  
  
  #p <- p + scale_y_discrete(labels = NULL,breaks = NULL)
  
  #p <- p + scale_x_continuous(labels = NULL,breaks = NULL)

  
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "bottom",axis.title =element_text(),
        legend.key = element_blank())+
  
       guides(size = guide_legend(label.position = "bottom",
                                        label.theme = element_text(angle = 0, hjust = 1, vjust = 0.6),
                                        title.theme = element_text(face = "bold"),
                                        nrow = 1,
                                        override.aes = list(colour = "black"),
                                        title.position = "top",
                                        title = "Abundance",
                                        title.hjust = 0.5),
                    fill = guide_legend(label.position = "bottom",
                                        label.theme = element_text(angle = 0, hjust = 0.5, vjust = 0.6),
                                        nrow = 1,
                                        override.aes = list(size=6),
                                        title = "Environmental type",
  
                                        title.theme = element_text(face = "bold"),
                                        title.position = "top",
                                        title.hjust = 0.5))
  path2<<-paste(superfamily,"_map.png",sep="")
  ggsave(path2,units = "in",width = 8.27, height = 4.8)}


map(map_demo)



