Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE)
rm(list=ls())
setwd('/Users/leoarrow/Desktop/RawData/2018-2022')
library(bibliometrix)

#importing data
wosfile=c('1-500.txt','501-1000.txt',
          '1001-1500.txt','1501-2000.txt',
          '2001-2500.txt','2501-3000.txt',
          '3001-3500.txt','3501-4000.txt',
          '4001-4500.txt','4501-4624.txt')
Bw=convert2df(file=wosfile,dbsource="wos",format='plaintext')
Br=biblioAnalysis(Bw)
#Overview summary
Bs=summary(Br)
Bs=summary(Br,k=10,pause=T)
Bs=summary(Br,k=100,pause=T,verbose=F)
#Summery Plot
plot(Br,k=10,pause=T)

###Country/Region
#first author
Bs=summary(Br,k=30,pause=T,verbose=F)
cc=Bs$MostProdCountries
#change as character
cc1=as.data.frame(lapply(cc[2:6],as.numeric))
cc1$country=cc$Country
cc1=cc1[,c(6,1:5)]
write.csv(cc1,file = '/Users/leoarrow/Desktop/一作2018-2022国家分析.csv')
plot(Br,k=10,pause=T) #first author output

#make it pretty! PS:700*350px output
cc2=cc1
cc2$country=factor(cc2$country,levels = cc2$country)
ccmelt=melt(cc2,measure.vars = c('SCP','MCP'),variable.name = 'Collaboration')
ccmelt$Collaboration = factor(ccmelt$Collaboration, levels = c('MCP','SCP'))
ggplot(ccmelt,aes(x=country,y=value,fill=Collaboration))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  scale_x_discrete(limits=rev(levels(ccmelt$country)))+
  scale_fill_manual(values = rev(c('#73AEDA','#EE8177')))+ # change color
  labs(x='',y='Publication',
       caption='SCP: Single Country Publications, MCP: Multiple Country Publications')+
  theme_bw()+
  theme(legend.position = c(0.85,0.15),
        plot.caption = element_text(size = 9, hjust = 0.5,
                                    color = 'black'))

#Geographical visualization analysis (Publication and citation)
easypackages::packages('bibliometrix','tidyverse','rio','reshape2','ggmap','cowplot')
source('/Users/leoarrow/Desktop/Bibliometrix base/Functions for bibliometrics base/bibliometrics analysis base.R')
#publication
acoFreq=get_allcountries_publication(M=Bw)[,1:2]
names(acoFreq)[1]='country/region'
#output
conames=acoFreq$`country/region` #Name
colcs=get_countryLCS(Bw,conames) #LCS and GCS
names(colcs)[1]='Country/Region'
coinfo=cbind(acoFreq,colcs[,2:3])
world=map_data('world') #world
world$region=toupper(world$region)
#Change the name of a special area for easy identification, such as the UNITED KINGDOM
coinfo$`country/region`=recode(coinfo$`country/region`,'UNITED KINGDOM' = 'UK')
coinfo$`country/region`=recode(coinfo$`country/region`,'KOREA' = 'SOUTH KOREA')
coinfo$Articles=log10(coinfo$Articles) #log10()
coinfo$GCS=log10(coinfo$GCS)
head(world)
head(coinfo)
#left_join
world_joined=left_join(world,coinfo,by=c('region'='country/region'))
#Final check
world_joined[which(world_joined$region=='SOUTH KOREA'),]
#It is the time to success!
world_joined1=melt(world_joined,measure.vars = c('Articles','LCS','GCS'),variable.name = 'index')
# color=c('#D02C2A','#FA8F2F','#FAE927','#C9DA36','#9ECB3C','#3DBA78','#2B55A1','#2D3D8E','#44388E','#6A368B')
# color2=rev(color)
gglist <- lapply(split(world_joined1,world_joined1$index), function(i){
  ggplot(i, aes(x = long, y = lat, group = group, fill = value))+
    geom_polygon()+
    scale_fill_gradientn(values = seq(0,1,0.1),colours = c('#2fa1dd',"white",'#f87669'),name = "Log10(Publication)")+
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(1,0),
          legend.justification = c(1,0),
          legend.background = element_blank())
})
# 1000px*1775px would be good choice
plot_grid(plotlist = gglist, ncol = 1, align = 'v',
          labels = c('Publication', 'LCS', 'GCS'), 
          label_x = c(-0.025,0,-0.001))
