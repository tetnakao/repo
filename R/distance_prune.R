library(tidyverse)

distance_prune <- function(dat.2smr, SNP = NA, chr = NA, pos = NA, pval = NA, distance.mb = 1){
  
  if(!(is.na(SNP)&is.na(chr)&is.na(pos)&is.na(pval))){
    eval(parse(text = str_glue("dat.2smr <- dat.2smr %>% 
      dplyr::rename(SNP = {SNP}, chr.exposure = {chr}, pos.exposure = {pos}, pval.exposure = {pval})")))
  } 
    
    dat.diagnosis1 <- dat.2smr %>% 
      select(SNP, chr.exposure, pos.exposure, pval.exposure) %>% 
          arrange(chr.exposure, pos.exposure) %>% 
          rownames_to_column("row_number") %>% 
          transform(row_number = as.integer(row_number))
    
    for (i in 1:(nrow(dat.diagnosis1)-2)){
      dat.diagnosis2 <- dat.diagnosis1 %>% 
        filter(row_number <= (i+2)) %>% 
        tail(n = 3) %>% 
        mutate(dis = pos.exposure-lag(pos.exposure),
            c.dis = chr.exposure-lag(chr.exposure)) %>% 
        mutate(near = case_when(
            dis < (distance.mb * 1000000) & c.dis == 0 ~ TRUE,
            TRUE ~ FALSE
            )) %>% 
        mutate(near.p = case_when(
            dis < (distance.mb * 1000000) & c.dis ==0 ~ pval.exposure - lag(pval.exposure),
            lead(dis) < (distance.mb * 1000000) & lead(c.dis) == 0 ~ pval.exposure - lead(pval.exposure)
            )) 
      if (nrow(dat.diagnosis2 %>% filter(near))>1){
        if (dat.diagnosis2$pval.exposure[1] > dat.diagnosis2$pval.exposure[2]){
          dat.diagnosis1 <- dat.diagnosis1 %>% filter(row_number != i)
        } else {
          dat.diagnosis1 <- dat.diagnosis1 %>% filter(row_number != (i+1))
        }
      } else if (dat.diagnosis2[3,]$near) {
        if (dat.diagnosis2$pval.exposure[2] > dat.diagnosis2$pval.exposure[3]){
          dat.diagnosis1 <- dat.diagnosis1 %>% filter(row_number != (i+1))
        } else {
          dat.diagnosis1 <- dat.diagnosis1 %>% filter(row_number != (i+2))
        }
        }
    }
  
    dat.prune <- dat.2smr %>% 
    select(-one_of("distance.filter")) %>%
    left_join(dat.diagnosis1 %>% mutate(distance.filter = TRUE) %>% select(-row_number)) %>% 
    arrange(chr.exposure, pos.exposure)
    
    if(!(is.na(SNP)&is.na(chr)&is.na(pos)&is.na(pval))){
      eval(parse(text = str_glue("dat.prune <- dat.prune %>% 
      dplyr::rename({SNP} = SNP, {chr} = chr.exposure, {pos} = pos.exposure, {pval} = pval.exposure)")))
    } 
    
    return(dat.prune)
}

