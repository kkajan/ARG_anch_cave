MAGs_PA_per_depth_ggplot2 <- ggplot(MAGs_PA_per_depth_f, aes(x=Tax2, y=value, fill=depth))+
  geom_bar(aes(), stat = 'identity', position = "stack", width=0.85) +
  scale_y_continuous(limits=c(0,250), breaks=c(0,50,100,150, 200,250),expand=c(0,0))+
  ggtitle("P/A MAGs per depth - total 418 MAGs")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))




heatmap_drug_class6_matrix <- as.matrix(heatmap_drug_class6)
Heatmap(heatmap_drug_class6_matrix, cluster_columns = FALSE, cluster_rows = FALSE)
col_fun3 = colorRamp2(c(0,1,2,3,4,5,6),  c( "#e6e6e6","#fe97fe","#fd59fd","#fc2ffc", "#ca26ca", "#971c97", "#4c0e4c"))

circos.par("start.degree" = 90, gap.after = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                              2,2,2,2,2,2,2,2, 2, 2,2, 50))
circos.heatmap(df_sum_A_V_M_order3_f5, col = col_fun3, rownames.side = "outside", track.height = 0.025,
               cluster=FALSE, split=df_sum_A_V_M_order3_f5$Order, rownames.cex = 0.4)
circos.heatmap(heatmap_drug_class6_matrix, col = col_fun3, 
               cluster=FALSE, track.height = 0.3)
lgd = Legend( col_fun = col_fun3)
grid.draw(lgd)

circos.clear()



MAGs_VFG_percentage_per_depth_ggplot <- ggplot(MAG_coverage_VFG_percentage_f2, aes(x=value, y=fct_rev(Category), fill=phylum))+
  geom_bar(aes(), stat = 'identity', position = "stack", width=0.85, colour="gray", linewidth=0.2) +
  theme_bw()+
  theme(legend.title=element_blank(),
        axis.line = element_line(linewidth=0.4),
        axis.text = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14),
        legend.text = element_text(color = "black", size = 8,margin = margin(t = 4)),
        legend.position = 'bottom', 
        legend.spacing.x = unit(0.2, 'cm'),
        legend.key.size = unit(0.15, "cm"),
        legend.key.height=unit(0.1, "cm"),
        legend.key.width=unit(0.5, "cm"),
        strip.text.y=element_text(angle=0)) + 
  scale_fill_manual(values = c(
    "Alphaproteobacteria"="#add097",
    "Gammaproteobacteria"="#15885a",
    "Actinobacteriota" ="#b2182b",
    "Bacteroidota"    ="#d6604d" ,
    "Campylobacterota" ="#f4a582",
    "Chloroflexota" ="#e78207",
    "Cloacimonadota" ="#a984a6",
    "Desulfobacterota" ="#dfd7ef",
    "Marinisomatota" ="#492686",
    "Omnitrophota"   ="#2166ac"  ,
    "Patescibacteria"  ="#63d0d0",
    "Verrucomicrobiota"="#6e3c3b", 
    "Other"="gray"))+
  guides(fill=guide_legend(ncol=3,nrow=5, byrow=TRUE)) +
  #scale_fill_manual(values=palette1_BacteriaAT_class)+
  #scale_fill_hue(palette1) +
  scale_x_continuous(limits=c(0,100.1), breaks=c(0,25,50,75,100),expand=c(0,0))+
  xlab("Relative abundance (%)")

ggsave(filename = "MAGs_VFG_percentage_per_depth_ggplot.pdf", plot= MAGs_VFG_percentage_per_depth_ggplot ,   width = 15, height = 10,units = "cm")



MAGs_MGE_percentage_per_depth_ggplot <- ggplot(MAG_coverage_MGE_percentage_f2, aes(x=value, y=fct_rev(Category), fill=phylum))+
  geom_bar(aes(), stat = 'identity', position = "stack", width=0.85, colour="gray", linewidth=0.2) +
  theme_bw()+
  theme(legend.title=element_blank(),
        axis.line = element_line(linewidth=0.4),
        axis.text = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14),
        legend.text = element_text(color = "black", size = 8,margin = margin(t = 4)),
        legend.position = 'bottom', 
        legend.spacing.x = unit(0.2, 'cm'),
        legend.key.size = unit(0.15, "cm"),
        legend.key.height=unit(0.1, "cm"),
        legend.key.width=unit(0.5, "cm"),
        strip.text.y=element_text(angle=0)) + 
  scale_fill_manual(values = c(
    "Alphaproteobacteria"="#add097",
    "Gammaproteobacteria"="#15885a",
    "Magnetococcia"="#5eb69d",
    "Actinobacteriota" ="#b2182b",
    "Bacteroidota"    ="#d6604d" ,
    "Campylobacterota" ="#f4a582"))+
  guides(fill=guide_legend(ncol=3,nrow=5, byrow=TRUE)) +
  #scale_fill_manual(values=palette1_BacteriaAT_class)+
  #scale_fill_hue(palette1) +
  scale_x_continuous(limits=c(0,100.1), breaks=c(0,25,50,75,100),expand=c(0,0))+
  xlab("Relative abundance (%)")

ggsave(filename = "MAGs_MGE_percentage_per_depth_ggplot.pdf", plot= MAGs_MGE_percentage_per_depth_ggplot ,   width = 15, height = 10,units = "cm")
