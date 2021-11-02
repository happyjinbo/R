# =====================================================
# title: hw02
# discriptions: this is a script about charts of hw02
# inputs:scv files and images
# outputs:images as pdf files
# =====================================================

# =====================
# 4)shot charts
# =====================
library(jpeg)
library(grid)
library(ggplot2)

klay_scatterplot <- ggplot(data = tp) +
  geom_point(aes(x=x,y=y,color=shot_made_flag))

court_file <- '../images/nba-court.jpg'
#create raste object
cout_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,'npc'),
  height = unit(1,'npc')
)

# shot charts of each player
klay_shot_chart <- ggplot(data = tp)+
  annotation_custom(cout_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('shot chart: klay thompson(2016 season)')+
  theme_minimal()
ggsave(filename = '../images/klay-thompson-short-chart.pdf',
       plot = klay_shot_chart,
       width = 6.5, height = 5)

andre_shot_chart <- ggplot(data = ig)+
  annotation_custom(cout_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('shot chart: andre-iguodala(2016 season)')+
  theme_minimal()
ggsave(filename = '../images/andre-iguodala-short-chart.pdf',
       plot = andre_shot_chart,
       width = 6.5, height = 5)

green_shot_chart <- ggplot(data = gr)+
  annotation_custom(cout_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('shot chart: draymond-green (2016 season)')+
  theme_minimal()
ggsave(filename = '../images/draymond-green-short-chart.pdf',
       plot = green_shot_chart,
       width = 6.5, height = 5)

durant_shot_chart <- ggplot(data = dr)+
  annotation_custom(cout_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('shot chart: kevin durant (2016 season)')+
  theme_minimal()
ggsave(filename = '../images/kevin-durant-short-chart.pdf',
       plot = durant_shot_chart,
       width = 6.5, height = 5)

curry_shot_chart <- ggplot(data = cr)+
  annotation_custom(cout_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('shot chart: stephen-curry (2016 season)')+
  theme_minimal()
ggsave(filename = '../images/stephen-curry-short-chart.pdf',
       plot = curry_shot_chart,
       width = 6.5, height = 5)

gsw_shot_chart <- ggplot(data=bin_dat)+
  annotation_custom(cout_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('shot chart: stephen-curry (2016 season)')+
  facet_wrap(~name)+
  theme_minimal()
ggsave(filename = '../images/gsw-shot-charts.pdf',
       plot = gsw_shot_chart,
       width = 8,height = 7)
























