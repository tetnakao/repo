file = commandArgs(trailingOnly = TRUE)[1]

library(tidyverse)

data.table::fread(file, data.table = F) %>%
  	separate(Otherinfo13, into = c("GT","AD_Ref", "AD_Alt","AF","DP","F1R2_Ref","F1R2_Alt","F2R1_Ref","F2R1_Alt"), remove = F, sep = "[:,]") %>% 
    dplyr::rename(FILTER = Otherinfo10,
                  INFO = Otherinfo11) %>% 
    rowwise() %>% 
  	mutate(across(AD_Ref:AD_Alt, as.integer),
  	       across(DP:F2R1_Alt, as.integer),
  	       AF = as.numeric(AF),
  	       minAD = min(AD_Ref, AD_Alt),
  	       minF1R2 = min(F1R2_Ref, F1R2_Alt),
  	       minF2R1 = min(F2R1_Ref, F2R1_Alt),
  	       PON = str_detect(INFO, ";PON;"),
  	       POPAF = grep("POPAF=", strsplit(INFO, ";")[[1]], value = T, fixed = T) %>% str_replace("POPAF=", "") %>% as.numeric(),
  	       Cosmic_hem_count = grep("haematopoietic_and_lymphoid_tissue", strsplit(cosmic96_coding,",|=")[[1]], value = T, fixed = T) %>% str_replace("\\(haematopoietic_and_lymphoid_tissue\\)", "") %>% as.integer %>% ifelse(length(.)>0, ., 0)) %>%
  	filter(DP>=20, minF1R2>0, minF2R1>0, minAD>=3) %>% 
  	filter(!(str_detect(transcriptOI, "ASXL1:NM_015338:exon12:c.1927delG:p.G645Vfs*58")&FILTER!="PASS")) %>% 
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
           PON,
           POPAF,
           avsnp150,
           cosmic96_coding,
           Cosmic_hem_count,
           GT,
           AD_Ref,
           AD_Alt,
           minAD,
           AF,
           DP,
           F1R2_Ref,
           F1R2_Alt,
           minF1R2,
           F2R1_Ref,
           F2R1_Alt,
           minF2R1,
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
