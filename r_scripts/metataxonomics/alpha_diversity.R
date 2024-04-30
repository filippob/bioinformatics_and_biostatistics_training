## R script to read in results from 16S rRNA-gene seq

library("ggplot2")
library("phyloseq")
library("tidyverse")
library("data.table")

## PARAMETERS
basefolder = "/home/filippo/Documents/rafaela"
fname = "results/phyloseq.RData"
outdir = "results"
  
## loading data previously imported in phyloseq
load(file.path(basefolder, fname))

## making figures folder
if(!file.exists(file.path(basefolder, "figures"))) dir.create(file.path(basefolder, "figures"), showWarnings = FALSE)

## making tables folder
if(!file.exists(file.path(basefolder, "tables"))) dir.create(file.path(basefolder, "tables"), showWarnings = FALSE)


# These are the stratifying variables available in the metadata
print(sample_variables(sample_data(myphyloseq)))

p <- plot_richness(myphyloseq, x="State.of.Brazil", color="State.of.Brazil")
ggsave(filename = file.path(basefolder, "figures", "richness_plot.png"), plot = p, device = "png")


#Prevelance and abundance table for each taxa
prevelancedf = apply(X = otu_table(myphyloseq),            
                     MARGIN = 1,
                     FUN = function(x){sum(x > 0)}
                     )
prevelancedf = data.frame(Prevalence = prevelancedf,
                          TotalAbundance = taxa_sums(myphyloseq),
                          tax_table(myphyloseq))
prevelancedf[1:10,]


#Whole phylum filtering: removal of the features with ambiguous phylum annotation
subset_data <- subset_taxa(myphyloseq, !is.na(Phylum) & !Phylum %in% c("", "uncharacterized"))
subset_data


#Investigation of low prevelance and abundance phylum and subset them out
plyr::ddply(prevelancedf, "Phylum", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})
#Investigation of low prevelance and abundance kingdom and subset them out
plyr::ddply(prevelancedf, "Kingdom", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})
#Investigation of low prevelance and abundance class and subset them out
plyr::ddply(prevelancedf, "Class", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})
#Investigation of low prevelance and abundance order and subset them out
plyr::ddply(prevelancedf, "Order", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})
#Investigation of low prevelance and abundance family and subset them out
plyr::ddply(prevelancedf, "Family", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})
#Investigation of low prevelance and abundance genus and subset them out
plyr::ddply(prevelancedf, "Genus", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})
#Investigation of low prevelance and abundance species and subset them out
plyr::ddply(prevelancedf, "Species", function(df1){
  data.frame(mean_prevalence=mean(df1$Prevalence),total_abundance=sum(df1$TotalAbundance,na.rm = T),stringsAsFactors = F)
})



#Individual taxa filtering: subset the phyla by prevalance 
prevelancedf1 = subset(prevelancedf, Phylum %in% get_taxa_unique(subset_data, taxonomic.rank = "Phylum"))
ggplot(prevelancedf1, aes(TotalAbundance, Prevalence / nsamples(subset_data),color=Phylum)) +
  
  geom_hline(yintercept = 0.05, alpha = 0.5, linetype = 2) + geom_point(size = 2, alpha = 0.7) +
  scale_x_log10() +  xlab("Total Abundance") + ylab("Prevalence [Frac. Samples]") +
  facet_wrap(~Phylum) + theme(legend.position="none")



#Definition of a prevalence threshold (following the website steps... I don't know if this step is really necessary in my case...)
prevalenceThreshold = 0.50 * nsamples(subset_data)
prevalenceThreshold


#Prevalence filter 
keepTaxa = rownames(prevelancedf1)[(prevelancedf1$Prevalence >= prevalenceThreshold)]
length(keepTaxa)

subset_data_2 = prune_taxa(keepTaxa, myphyloseq)
subset_data_2


#Agglomerate taxa at the Genus level and remove all taxa without genus level assignment
length(get_taxa_unique(subset_data_2, taxonomic.rank = "Genus"))

subset_data_3 = tax_glom(subset_data_2, "Genus", NArm = TRUE)
subset_data_3


#Out of curiosity: how many "reads" does this leave us at??? (following the website steps... I don't know if this step is really necessary in my case...)
sum(colSums(otu_table(subset_data_3)))


## CALCULATE ALPHA DIVERSITY
#Table with: observed richness, other forms of richness, diversity, evenness, dominance and rarity
alpha = estimate_richness(myphyloseq, split = TRUE)
fwrite(alpha, file = file.path(basefolder, "tables", "alpha.csv"))

alpha$id <- gsub("\\.","-",row.names(alpha))
temp <- sample_data(myphyloseq)
temp$id <- row.names(temp)

alpha <- alpha |> inner_join(temp, by = "id")

## average values of alpha diversity indices, per state of Brasil
alpha |>
  group_by(State.of.Brazil) |>
  summarise(across(where(is.numeric), mean))

## long format
mAlpha <- alpha |> gather(key = "index", value = "value", -c("id","State.of.Brazil","Somatic.Cell.Count", "Standard.Plate.Count"))

## plot
p <- ggplot(mAlpha, aes(x = State.of.Brazil, y = value)) + geom_boxplot(aes(fill=State.of.Brazil))
p <- p + geom_jitter(aes(colour = State.of.Brazil))
p <- p + facet_wrap(~index, scales = "free")
p <- p + theme(axis.text.x = element_text(angle=90))
p

ggsave(filename = file.path(basefolder, "figures", "alpha_boxplot.png"), plot = p, device = "png", width = 10, height = 10)


