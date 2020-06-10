# Fixed feature-table.tsv got on qiime2 and save it as "feature-table.txt"
# qiimeのアウトプットのq2~とか消した


library(vegan)
# prepare microbial count data at OTU lavel. I used one obtained from qiime2
community <- read.table("feature-table.tsv", header = T, row.names = 1) #used the one from qiime2.
community <- t(community)
# prepare metadata
env <- read.table("../2019malawi_pH_CN_moisture_texture/19mal_env.txt", header = T, sep = "\t") 

# rarefaction
# リード数の合計が最小のサンプルに合わせて、各サンプルからその数だけランダムに抽出
rared_com <- rrarefy(community, min(rowSums(community)))

# ordination
# 序列化？たぶん微生物の構成の違いにより順位付けを行っている
ord <- metaMDS(rared_com)

# permanova
# 土地利用による差を解析
landuse <- c(rep(1, 18), rep(2, 18)) # 即興で数値で農地と森林を表してみた
adonis(rared_com~landuse, method = "chao")

# NMDSにggplotを使うという試み。俺的にはplotよりggplotのほうがパラメータの変え方わかりやすいので
# using ggplot for the NMDS plot

data.scores <- as.data.frame(scores(ord)) # scoresで序列のスコアだけ取り出してるんだと思う
data.scores$sample <- rownames(data.scores) # ここ以下は名前つけたりデータの整形
data.scores <- cbind(data.scores, env)
head(data.scores)


# make a figure
library(ggplot2)
g <- ggplot() +
  geom_point(data=adata.scores, aes(x=NMDS1, y=NMDS2, shape=Landuse, colour=Landuse), size=8) +
  scale_colour_manual(values=c("Farm"="orange", "Natural"="darkgreen")) +
  scale_shape_manual(values = c("Farm"=16, "Natural"=17)) +
  theme_bw() +
  theme(panel.border = element_rect(size = 1.8, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=25, face = "bold", colour = "black"),
        axis.title=element_text(size=35,face="bold", colour = "black"))
  
# 自分の解析はサイトごとでやったのでファセット（以下）もつけていたが、今回は省略  
#facet_grid(. ~ Site) + 
  #theme(strip.text.x = element_text(face="bold", size=30),
        #strip.background = element_rect(fill = "skyblue", colour = "black", size=1))

# save it as a png
# gridextraはggplotで複数の図を1枚の画像に収められるやつ。plotではperとかでできる。今回は関係ない
png(filename = "200610mal_NMDS_all_presen.png", width = 2000, height = 750, res = 100)
#gridExtra::grid.arrange(ga, gb, nrow=1)
dev.off()

# just wanna make polygon plots
# 上のだけでも十分そうだが、点の周囲を囲んで領域を色付けしたい

# hull values for farm. Farmで点の周りを囲む数値たちを格納
f_hull <- data.scores[data.scores$Landuse == "Farm", ][chull(data.scores[data.scores$Landuse == "Farm", c("NMDS1", "NMDS2")]), ] 
# do the same things for the natural one
n_hull <- data.scores[data.scores$Landuse == "Natural", ][chull(data.scores[data.scores$Landuse == "Natural", c("NMDS1", "NMDS2")]), ]

# bind them
# これで、土地利用ごとに点を集めた場合の外側にある点だけの数値データができた
hull.data <- rbind(f_hull, n_hull)

# make the plot
g2 <- g +
  geom_polygon(data=hull.data,
               aes(x=NMDS1,y=NMDS2,fill=Landuse, group=Landuse),
               alpha=0.3 # 領域の透明度
               )

# output (save)
png(filename = "200610mal_NMDS_all_presen2.png", width = 2000, height = 750, res = 100)
dev.off()

