library(tidyverse)
library(ggrepel)
library(cowplot)
library(patchwork)

plot.partial.table <- function(data, variable, title, italic = FALSE, color = NA_character_, manual_color = NULL){
  
  if(is.na(color)){
    
    eval(parse(text = str_glue("pdata <- data %>% 
                               dplyr::select(variable = {variable}) %>% 
                                rownames_to_column('row') %>%
                                transform(row = factor(row, levels = rev(.$row)))")))
  } else {
    
    eval(parse(text = str_glue("pdata <- data %>% 
                               dplyr::select(variable = {variable}, color = {color}) %>% 
                                rownames_to_column('row') %>%
                                transform(row = factor(row, levels = rev(.$row)))")))
  }
  
  if (italic){
    font_face <- "bold.italic"
  } else {
    font_face <- "bold"
  }
  
  tp <- pdata %>% 
    ggplot(aes(y = row))+
    ggtitle(title) +
    geom_text(aes(label = variable, x = 0))
  
  if(is.na(color)){
    tp <- tp +
      geom_text(aes(label = variable, x = 0))
  } else {
    tp <- tp +
      geom_text(aes(label = variable, x = 0, color = color), show.legend = FALSE)
    if (!is.null(manual_color)) {
      tp <- tp +
        scale_color_manual(values = manual_color)
    }
  }
  
  tp <- tp +
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


ggforest.facet <- function(data, 
                     variable, 
                     beta = "b", 
                     se = NA, 
                     p = "p", 
                     plot.title="", 
                     lo = NA, 
                     hi = NA, 
                     height = NA, 
                     width = 10, 
                     outprefix,
                     left.table = NULL,
                     caption = NA,
                     facet = NA,
                     manual_color = NULL,
                     text_color = F,
                     meta = NA,
                     OR = F,
                     HR = F,
                     xmax = NA){
  
  if (is.na(lo) & is.na(hi)) {
    eval(parse(text = str_glue("p.dat <- data %>%
	                           	        select(variable = {variable},
	                           	        		beta = {beta},
	                           	        		se = {se},
	                           	        		p = {p},
                                          one_of(left.table),
                                          one_of(facet),
                                          one_of(meta)
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
                                          one_of(left.table),
                                          one_of(facet),
                                          one_of(meta)
	                           	        )")))
  }
  
  p.dat <- p.dat %>%
    mutate(b.label = formatC(beta, format = "fg", digits = 2),
           lo.label = formatC(lo, format = "fg", digits = 2),
           hi.label = formatC(hi, format = "fg", digits = 2),
           se.label = str_glue("[{lo.label}:{hi.label}]"),
           p.label = prettyNum(p, digits = 2) %>% str_replace("e", " × 10"),
           title.length = nchar(as.character(variable))) %>%
    transform(variable = factor(variable, levels = rev(.$variable %>% unique)))
  
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
  
  if (text_color) {
    text_color_dummy <- "variable"
  } else {
    text_color_dummy <- NA
  }
  
  tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F, color = text_color_dummy, manual_color = manual_color)
  tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F, color = text_color_dummy, manual_color = manual_color)
  tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T, color = text_color_dummy, manual_color = manual_color)
  title_length <- max(p.dat$title.length, nchar(variable))
  
  ##main plot
  #facet
  if (is.na(facet)){
    facet_arg <- ""
  } else {
    facet_arg <- ", color = {variable}"
  }
  
  #shape
  if (is.na(meta)){
    meta_arg_p1 <- ""
    meta_arg_p2 <- ""
  } else {
    meta_arg_p1 <- str_glue("aes(shape = {meta}, size = {meta})")
    meta_arg_p2 <- str_glue(" + scale_shape_manual(values=c(T = 23, F = 16)) +
                          scale_size_manual(values=c(T = 10, F = 5))")
  }
  
  eval(parse(text = str_glue("
    p <- p.dat %>% 
      ggplot(aes(x = beta, y = variable{facet_arg}))
  ")))
  
  #OR HR
  if (OR|HR) {
    
    p <- p +
      geom_vline(xintercept = 1, linetype = "dashed", color = "grey")
    if(OR){
      tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "OR", italic = F, color = text_color_dummy, manual_color = manual_color)
    } else {
      tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "HR", italic = F, color = text_color_dummy, manual_color = manual_color)
    } 
    
  } else {
    
    p <- p +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey")
    tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T, color = text_color_dummy, manual_color = manual_color)
    
  }
  
  p <- p +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")
  
  eval(parse(text = str_glue("
      p <- p +
        geom_point({meta_arg_p1})
                             ")))
  
  p <- p +
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
  
  if(!is.na(xmax)){
    p <- p + 
      coord_cartesian(xlim = c(0, xmax))
  }
  
  if (!is.na(facet)){
    p <- p +
      facet_grid(rows = facet, switch = "both") +
      theme(axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            strip.text.y = element_text(size = 8))
  }
  
  if (!is.null(manual_color)){
    p <- p +
      scale_color_manual(values = manual_color, guide = guide_legend(reverse = TRUE))+
      theme(legend.position = c(0.8, 0.8),
            legend.title = element_blank())
  }
  
  eval(parse(text = str_glue("p <- p{meta_arg_p2}")))
  
  #make folder
  fld <- str_replace(outprefix, paste0(basename(outprefix),"$"), "")
  system(str_glue("mkdir -p {fld}"))
  
  tl <- ""
  if (length(left.table) != 0){
    for (i in 1:length(left.table)){
      v <- left.table[i]
      eval(parse(text = str_glue("t{i} <- plot.partial.table(data = p.dat, variable = v, title = v, color = text_color_dummy, manual_color = manual_color)")))
      tl <- paste0(tl,'t',i,' + ')
    }
  }
  
  print(p)
  print(tb)
  print(tse)
  print(tp)
  print(tl)
  
  if (is.na(facet)){
    facet_arg_out1 <- "tt + "
    facet_arg_out2 <- "title_length/5, "
  } else {
    facet_arg_out1 <- ""
    facet_arg_out2 <- ""
  }
  
  eval(parse(text = str_glue("
      p.out <- {facet_arg_out1}p + tb + tse + tp + {tl}
      plot_layout(widths = c({facet_arg_out2}7, 1, 2, 1, rep(1, length(left.table)))) +
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
