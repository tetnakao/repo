library(tidyverse)
library(ggrepel)
library(cowplot)
library(patchwork)

plot.partial.table <- function(data, variable, title, italic = FALSE){
  
  eval(parse(text = str_glue("pdata <- data %>% 
                               select(variable = {variable}) %>% 
                                transform(variable = factor(variable, levels = rev(.$variable)))")))
             
  if (italic){
    font_face <- "bold.italic"
  } else {
    font_face <- "bold"
  }
  
  tp <- pdata %>% 
  ggplot(aes(y = variable))+
  ggtitle(title) +
  geom_text(aes(label = variable, x = 0))+
  theme_bw() + 
    theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, face = font_face)
          )
  
  return(tp)
}

ggforest <- function(data, 
                      variable, 
                      beta = "b", 
                      se = "se", 
                      p = "p", 
                      plot.title="", 
                      lo = NA, 
                      hi = NA, 
                      height = NA, 
                      width = 10, 
                      outprefix,
                      left.table = "",
                      caption = NA,
                      OR = F,
                      HR = F){

  if(is.na(lo)&is.na(hi)){
  eval(parse(text = str_glue("p.dat <- data %>%
  	                           	        select(variable = {variable},
  	                           	        		beta = {beta},
  	                           	        		se = {se},
  	                           	        		p = {p},
                                            one_of(left.table)
  	                           	        )")))
    
    
    if(OR|HR){
      
      p.dat <- p.dat %>%
        mutate(lo = exp(beta - 1.96 * se),
               hi = exp(beta + 1.96 * se),
               beta = exp(beta))
      
    } else {
      
      p.dat <- p.dat %>%
        mutate(lo = beta - 1.96 * se,
               hi = beta + 1.96 * se)
      
    }
    
  } else {
    eval(parse(text = str_glue("p.dat <- data %>%
  	                           	        select(variable = {variable},
  	                           	        		beta = {beta},
  	                           	        		lo = {lo},
  	                           	        		hi = {hi},
  	                           	        		p = {p},
                                            one_of(left.table)
  	                           	        )")))
    
  }

    p.dat <- p.dat %>%
        mutate(b.label = formatC(beta, digits = 2),
                 lo.label = signif(lo, digits = 2),
                 hi.label = signif(hi, digits = 2),
                 se.label = str_glue("[{lo.label}:{hi.label}]"),
                 p.label = formatC(p, digits = 2),
                 title.length = nchar(as.character(variable))) %>%
        transform(variable = factor(variable, levels = rev(.$variable)))

  #adjust size of the elements
  x.min <- min(p.dat$lo) - (max(p.dat$hi) - min(p.dat$lo))/10
  x.max <- max(p.dat$hi) + (max(p.dat$hi) - min(p.dat$lo))/10
  
  if(OR|HR){
    if(x.max<1){
      x.max <- (1 - x.min) * 0.1
    }
  } else {
    if(x.max<0){
      x.max <- (0 - x.min) * 0.1
    }
  }
  
  if(is.na(height)){
  	height <- (nrow(p.dat) * 0.5) + 1
  }

  tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F)
  tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
  tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
  title_length <- max(p.dat$title.length, nchar(variable))

  if (OR|HR) {
      p <- p.dat %>% 
        ggplot(aes(x = beta, y = variable))+
          geom_vline(xintercept = 1, linetype = "dashed", color = "grey")
      if(OR){
        tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "OR", italic = F)
      } else {
        tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "HR", italic = F)
      }

    } else {
      p <- p.dat %>% 
        ggplot(aes(x = beta, y = variable))+
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
      tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
    }
    
  p <- p +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
    geom_point() +
    theme_cowplot() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    xlim(x.min, x.max) + 
    ggtitle(plot.title)
  
  if(OR){
    p <- p + xlab("Odds Ratio")
  } else if (HR) {
    p <- p + xlab("Hazard Ratio")
  } else {
    p <- p + xlab("Effect Estimate")
  }

  #make folder
  fld <- str_replace(outprefix, paste0(basename(outprefix),"$"), "")
  system(str_glue("mkdir -p {fld}"))

  tl <- NULL
  if (length(left.table) != 0){
      for (i in 1:length(left.table)){
        v <- left.table[i]
        eval(parse(text = str_glue("t{i} <- plot.partial.table(data = p.dat, variable = v, title = v)")))
                                    tl <- paste0(tl,'t',i,' + ')
      }
    }
  eval(parse(text = str_glue("
    p.out <- tt + p + tb + tse + tp + {tl}
    plot_layout(widths = c(title_length/5, 7, 1, 2, 1, rep(1, length(left.table)))) +
    plot_annotation(title = basename(outprefix))
    ")))
  
  if (!is.na(caption)){
    p.out <- p.out + plot_annotation(caption = caption)
  }
  
  p.out

  #save
  ggsave(str_glue("{outprefix}.png"), height = height, width = width)
  ggsave(str_glue("{outprefix}.pdf"), height = height, width = width)
  saveRDS(p.out, str_glue("{outprefix}.rds"))
  write_tsv(p.dat, str_glue("{outprefix}.tsv"))

  return(p.out)
}

