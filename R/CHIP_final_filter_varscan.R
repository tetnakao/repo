file = commandArgs(trailingOnly = TRUE)[1]

library(tidyverse)

data.table::fread(file, data.table = F) %>%
	separate(`Sample_ID	"Gene.refGene"`, into = c("Sample_ID", "Gene.refGene"), remove = T, sep = '"') %>%
  	separate(Otherinfo13, into = c("GT", "GQ", "SDP", "DP", "RD", "AD", "FREQ", "PVAL", "RBQ", "ABQ", "RDF", "RDR", "ADF", "ADR"), remove = F, sep = "[:,]") %>% 
    dplyr::rename(FILTER = Otherinfo10,
                  INFO = Otherinfo11) %>% 
    rowwise() %>% 
  	mutate(Cosmic_hem_count = grep("haematopoietic_and_lymphoid_tissue", strsplit(cosmic96_coding,",|=")[[1]], value = T, fixed = T) %>% str_replace("\\(haematopoietic_and_lymphoid_tissue\\)", "") %>% as.integer %>% ifelse(length(.)>0, ., 0)) %>%
  	select(Sample,
           Gene.refGene,
           NonsynOI,
           Func.refGene,
           ExonicFunc.refGene,
           Accession,
           Chr,
           Start,
           End,
           Ref,
           Alt,
           FILTER,
           avsnp150,
           cosmic96_coding,
           Cosmic_hem_count,
           GT:ADR,
           transcriptOI,
           GeneDetail.refGene,
           AAChange.refGene,
           INFO,
           whitelist,
           wl.mis,
           wl.lof,
           wl.splice,
           wl.exception,
           manualreview
           ) %>%
data.table::fwrite("all_filtered.tsv", sep = "\t")
