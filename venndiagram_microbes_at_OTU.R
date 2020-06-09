# data depends on what you analyse
# import microbial reference data
otu_micname <- read.table("2019zambia_mic_community/200204otu_micnames.txt", sep = "\t") #used the one from taxonomy.qzv on qiime2.
otu_micname <- otu_micname[2:10693,1:2]
colnames(otu_micname) <- c("ID", "Taxon")

# import microbial count data at OTU level.
community <- read.table("2019zambia_mic_community/200127zam_otu.txt", header = T, row.names = 1) #used the one from qiime2.
community <- t(community) #used the one from qiime2.

# change absolute values to relative values
rel_com <- community/rowSums(community) 

# delete microbes which have less than 0.1% of relative abundances
# convert the number less than 0.001 to 0
rel_com[rel_com <= 0.001 ] <- 0

# divide the data to each site-landuse & calculate the mean
# cut off the data (depends on yours)
af <- rel_com[1:9,]
an <- rel_com[10:18,]
bf <- rel_com[19:27,]
bn <- rel_com[28:36,]
cf <- rel_com[37:45,]
cn <- rel_com[46:54,]

# delete the columns which has only 0 data.
af <- af[, colSums(af)!=0]
an <- an[, colSums(an)!=0]
bf <- bf[, colSums(bf)!=0]
bn <- bn[, colSums(bn)!=0]
cf <- cf[, colSums(cf)!=0]
cn <- cn[, colSums(cn)!=0]

#prepare for making venn diagram
if (!require(VennDiagram))install.packages('VennDiagram')
library(VennDiagram) #load the package

# list the column names (names of microbes (OTU names))
afmic <- colnames(af)
anmic <- colnames(an)
bfmic <- colnames(bf)
bnmic <- colnames(bn)
cfmic <- colnames(cf)
cnmic <- colnames(cn)

# make the list of both land-uses
flist <- list(A.Farm = afmic, B.Farm = bfmic, C.Farm = cfmic)
nlist <- list(A.Natural = anmic, B.Natural = bnmic, C.Natural = cnmic)

# make venn diagrams
# I have made 2 diagrams of each land-use at first, and then I made a diagram to compare the microbes commonly exist in the both land-uses.

#first make it for farm
#by running this a figure will be automatically saved to the working directory
venn.diagram(flist,
             filename="farm_venn.jpg",
             fill=c(2,3,4),
             alpha=0.4,
             lty=1,
             main = "Distribution in Farm Soils (OTU)",
             main.fontface = "bold",
             main.fontfamily = "arial",
             main.cex = 1.5,
             lwd=2.5,
             cex=2,
             fontfamily="arial",
             fontface="bold",
             cat.cex=1.8,
             cat.dist=c(0.07,0.07, 0.04),
             cat.fontfamily="arial",
             cat.fontface="bold",
             margin=0.03)
#memorize the microbes commonly existed in farm
fcommon <- intersect(intersect(afmic, bfmic), cfmic)

#do the same things again for natural sites
venn.diagram(nlist,
             filename="nat_venn.jpg",
             fill=c(2,3,4),
             alpha=0.4,
             lty=1,
             main = "Distribution in Natural Soils (OTU)",
             main.fontface = "bold",
             main.fontfamily = "arial",
             main.cex = 1.5,
             lwd=2.5,
             cex=2,
             fontfamily="arial",
             fontface="bold",
             cat.cex=1.8,
             cat.dist=c(0.07, 0.07, 0.04),
             cat.fontfamily="arial",
             cat.fontface="bold",
             margin=0.03)
#memorize the microbes commonly existed in farm
ncommon <- intersect(intersect(anmic, bnmic), cnmic)

#make the list of common microbes in each landuses
compalist <- list(Farm = fcommon, Natural = ncommon)

#make the diagram of that listª
venn.diagram(compalist,
             filename="landuse_venn.jpg",
             fill=c(5,6),
             alpha=0.4,
             lty=1,
             main = "Difference between the land-uses (OTU)",
             main.fontface = "bold",
             main.fontfamily = "arial",
             main.cex = 1.5,
             lwd=2.5,
             cex=2,
             fontfamily="arial",
             fontface="bold",
             cat.cex=1.8,
             cat.dist=c(0.05, 0.05),
             cat.fontfamily="arial",
             cat.fontface="bold",
             cat.pos=c(25, 320),
             margin=0.2,
             main.pos = c(0.5, 0.8),
             inverted=TRUE)
# memorize the microbes commonly exsit in both land-uses
common <- intersect(fcommon, ncommon)
# do that for the microbes exsist only in farm
fdif <- setdiff(fcommon, ncommon)
# do that for the microbes exsist only in natural soil
ndif <- setdiff(ncommon, fcommon)

# change them to dataframes
common <- as.data.frame(common)
fdif <- as.data.frame(fdif)
ndif <- as.data.frame(ndif)

# make tables to refer the names of those microbes using "otu_micname" that I made at the beginning.
ftable <- merge(otu_micname, fdif,by.x="ID", by.y="fdif", sort=F)
ntable <- merge(otu_micname, ndif, by.x="ID", by.y="ndif", sort=F)
ctable <- merge(otu_micname, common, by.x="ID", by.y="common", sort=F)

# output the results (what kind of microbes are in the territories in the venn)
write.csv(ftable, "venn_farm.csv",append=F)
write.csv(ntable, "venn_nat.csv",append=F)
write.csv(ctable, "venn_common.csv",append=F)

