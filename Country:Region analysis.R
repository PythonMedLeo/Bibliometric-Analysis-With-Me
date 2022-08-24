Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE)
rm(list=ls())
setwd('/Users/leoarrow/Desktop/20世纪葡萄膜炎/RawData/2018-2022')
library(bibliometrix)
wosfile=c('1-500.txt','501-1000.txt',
          '1001-1500.txt','1501-2000.txt',
          '2001-2500.txt','2501-3000.txt',
          '3001-3500.txt','3501-4000.txt',
          '4001-4500.txt','4501-4624.txt')

Bw=convert2df(file=wosfile,dbsource="wos",format='plaintext')
Br=biblioAnalysis(Bw)

#基本信息概览
Bs=summary(Br)
Bs=summary(Br,k=10,pause=T)
Bs=summary(Br,k=100,pause=T,verbose=F)
#基本信息作图
plot(Br,k=10,pause=T)

###国家情况
#一作国家
Bs=summary(Br,k=30,pause=T,verbose=F)
cc=Bs$MostProdCountries #这里是字符串类型
#change字符类型
cc1=as.data.frame(lapply(cc[2:6],as.numeric))
cc1$country=cc$Country
cc1=cc1[,c(6,1:5)]
write.csv(cc1,file = '/Users/leoarrow/Desktop/一作2018-2022国家分析.csv')
plot(Br,k=10,pause=T) #这个做出来就是一作的

#美化；PS:700*350px导出
cc2=cc1
cc2$country=factor(cc2$country,levels = cc2$country)

ccmelt=melt(cc2,measure.vars = c('SCP','MCP'),variable.name = 'Collaboration')
ccmelt$Collaboration = factor(ccmelt$Collaboration, levels = c('MCP','SCP'))

ggplot(ccmelt,aes(x=country,y=value,fill=Collaboration))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  scale_x_discrete(limits=rev(levels(ccmelt$country)))+
  scale_fill_manual(values = rev(c('#73AEDA','#EE8177')))+ # 手动修改颜色
  labs(x='',y='Publication',
       caption='SCP: Single Country Publications, MCP: Multiple Country Publications')+
  theme_bw()+
  theme(legend.position = c(0.85,0.15),
        plot.caption = element_text(size = 9, hjust = 0.5,
                                    color = 'black'))


#地理可视化Publication和引文量
easypackages::packages('bibliometrix','tidyverse','rio','reshape2','ggmap','cowplot')
source('/Users/leoarrow/Desktop/Bibliometrix base/Functions for bibliometrics base/bibliometrics analysis base.R')

#所有作者的国家publication
acoFreq=get_allcountries_publication(M=Bw)[,1:2]
# acoFreq
names(acoFreq)[1]='country/region'

####画图！！#####
conames=acoFreq$`country/region` #所有国家的名字
colcs=get_countryLCS(Bw,conames) #所有国家的名字和LCS和GCS
names(colcs)[1]='Country/Region'
coinfo=cbind(acoFreq,colcs[,2:3])


world=map_data('world') #调出world地图
world$region=toupper(world$region) #变大写字母

#改特殊地区的名字，便于识别，比如英国的名字
coinfo$`country/region`=recode(coinfo$`country/region`,'UNITED KINGDOM' = 'UK')
coinfo$`country/region`=recode(coinfo$`country/region`,'KOREA' = 'SOUTH KOREA')
coinfo$Articles=log10(coinfo$Articles) #因为美国发文太多，log10（）一下画图比较好看
coinfo$GCS=log10(coinfo$GCS)
head(world)
head(coinfo)

#用left_join
world_joined=left_join(world,coinfo,by=c('region'='country/region'))

#检查一下数据
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
