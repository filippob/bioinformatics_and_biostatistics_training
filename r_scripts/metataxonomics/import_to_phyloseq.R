## R script to read in results from 16S rRNA-gene seq

library("readxl")   
library("phyloseq")  

## PARAMETERS
basefolder = "/home/filippo/Documents/rafaela"
fname = "filtered_otu/otu_table_filtered.biom"
mapfile = "Metadata_words_Rafaela.xlsx"
outdir = "results"
  
## READ FILTERED OTU TABLE FROM MICCA
biom_file = import_biom(BIOMfilename = file.path(basefolder,fname))

## Have a look at the OTU table and the taxonomic classification
print(otu_table(biom_file)[1:12, 1:10])

#To add names to the taxa columns: TAXONOMIC LEVELS
colnames(tax_table(biom_file)) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")  
print(tax_table(biom_file)[1:12,1:7])
         
# READ THE METADATA
metadata <- readxl::read_xlsx(file.path(basefolder, mapfile))
metadata <- as.data.frame(metadata) ## convert from tibble to dataframe to impot metadata into Phyloseq

## move sample IDs from file column to row names
row.names(metadata) = metadata$`Sample ID`
metadata$`Sample ID` <- NULL

## Now we are adding metadata to the phyloseq object
samples = sample_data(metadata)
otus = otu_table(biom_file)
taxa = tax_table(biom_file)

## creating the phyloseq object
myphyloseq = phyloseq(otus,taxa,samples)

## write out the phyloseq data
dir.create(file.path(basefolder, outdir), showWarnings = FALSE)
fname = file.path(basefolder, outdir, "phyloseq.RData")
save(myphyloseq, file = fname)

print("DONE!")
