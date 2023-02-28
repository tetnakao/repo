library(tidyverse)
library(ggrepel)
library(cowplot)
library(patchwork)

str_eval <- function(x){
  eval(parse(text = str_glue(x)))
}

plot.partial.table <- function(data, variable, title, italic = FALSE){
  
  eval(parse(text = str_glue("pdata <- data %>% 
                               select(variable = {variable}) %>% 
                                rownames_to_column('row') %>%
                                transform(row = factor(row, levels = rev(.$row)))")))
             
  if (italic){
    font_face <- "bold.italic"
  } else {
    font_face <- "bold"
  }
  
  tp <- pdata %>% 
  ggplot(aes(y = row))+
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

ggforest2 <- function(data, variable, beta, se, p, plot.title="", lo = NA, hi = NA, height = NA, width = 10, outprefix){
eval(parse(text = str_glue("p.dat <- data %>%
                                      select(variable = {variable},
                                          beta = {beta},
                                          se = {se},
                                          p = {p}
                                      )")))
p.dat <- p.dat %>%
mutate(lo = beta - 1.96 * se,
    hi = beta + 1.96 * se) %>%
mutate(b.label = formatC(beta, format = "fg", digits = 2),
         lo.label = signif(lo, format = "fg", digits = 2),
         hi.label = signif(hi, format = "fg", digits = 2),
         se.label = str_glue("[{lo.label}:{hi.label}]"),
         p.label = prettyNum(p, digits = 2),
         title.length = nchar(variable)) %>%
transform(variable = factor(variable, levels = rev(.$variable)))

#adjust size of the elements
x.min <- min(p.dat$lo) - (max(p.dat$hi) - min(p.dat$lo))/10
x.max <- max(p.dat$hi) + (max(p.dat$hi) - min(p.dat$lo))/10
if(is.na(height)){
  height <- (nrow(p.dat) * 0.5) + 1
}

tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
title_length <- max(p.dat$title.length)
title_length <- title_length/7

p <- p.dat %>% 
ggplot(aes(x = beta, y = variable))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
  geom_point() +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = (title_length + 2.5)/(title_length + 5)),
        axis.title.y = element_blank()) +
  xlim(x.min, x.max) + 
  ggtitle(plot.title) +
  xlab("Effect Estimate")

  #make folder
  fld <- str_replace(outprefix, paste0(basename(outprefix),"$"), "")
  system(str_glue("mkdir -p {fld}"))

  p + tb + tse + tp +
  plot_layout(widths = c((title_length + 5), 1, 2, 1)) +
  plot_annotation(title = basename(outprefix))

  #save
  ggsave(str_glue("{outprefix}.png"), height = height, width = width)
  ggsave(str_glue("{outprefix}.pdf"), height = height, width = width)
}

#Plot label separately
ggforest <- function(data, 
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
                      left.table = "",
                      caption = NA){
  
  if (is.na(lo) & is.na(hi)) {
    eval(parse(text = str_glue("p.dat <- data %>%
	                           	        select(variable = {variable},
	                           	        		beta = {beta},
	                           	        		se = {se},
	                           	        		p = {p},
                                          one_of(left.table)
	                           	        )")))
    p.dat <- p.dat %>%
      mutate(lo = beta - 1.96 * se,
             hi = beta + 1.96 * se)
    
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
mutate(b.label = formatC(beta, format = "fg", digits = 2),
         lo.label = formatC(lo, format = "fg", digits = 2),
         hi.label = formatC(hi, format = "fg", digits = 2),
         se.label = str_glue("[{lo.label}:{hi.label}]"),
         p.label = prettyNum(p, digits = 2),
         title.length = nchar(as.character(variable))) %>%
transform(variable = factor(variable, levels = rev(.$variable)))

#adjust size of the elements
x.min <- min(p.dat$lo) - (max(p.dat$hi) - min(p.dat$lo))/10
x.max <- max(p.dat$hi) + (max(p.dat$hi) - min(p.dat$lo))/10
if(is.na(height)){
	height <- (nrow(p.dat) * 0.5) + 1
}

tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F)
tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
title_length <- max(p.dat$title.length, nchar(variable))

p <- p.dat %>% 
ggplot(aes(x = beta, y = variable))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
  geom_point() +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  xlim(x.min, x.max) + 
  ggtitle(plot.title) +
  xlab("Effect Estimate")

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

  return(p.out)
}

#JACC color
ggforest.jacc <- function(data, 
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
                     left.table = "",
                     caption = NA){
  eval(parse(text = str_glue("p.dat <- data %>%
	                           	        select(variable = {variable},
	                           	        		beta = {beta},
	                           	        		se = {se},
	                           	        		p = {p},
                                          one_of(left.table),
                                          lo = {lo},
                                          hi = {hi}
	                           	        )")))
  if (is.na(lo) & is.na(hi)) {
    p.dat <- p.dat %>%
      mutate(lo = beta - 1.96 * se,
             hi = beta + 1.96 * se)
  } 
  
  p.dat <- p.dat %>%
    mutate(b.label = formatC(beta, format = "fg", digits = 2),
           lo.label = signif(lo, format = "fg", digits = 2),
           hi.label = signif(hi, format = "fg", digits = 2),
           se.label = str_glue("[{lo.label}:{hi.label}]"),
           p.label = prettyNum(p, digits = 2),
           title.length = nchar(as.character(variable))) %>%
    transform(variable = factor(variable, levels = rev(.$variable)))
  
  #adjust size of the elements
  x.min <- min(p.dat$lo) - (max(p.dat$hi) - min(p.dat$lo))/10
  x.max <- max(p.dat$hi) + (max(p.dat$hi) - min(p.dat$lo))/10
  if(is.na(height)){
    height <- (nrow(p.dat) * 0.5) + 1
  }
  
  tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F)
  tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
  tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
  tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
  title_length <- max(p.dat$title.length, nchar(variable))
  
  p <- p.dat %>% 
    ggplot(aes(x = beta, y = variable))+
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
    geom_errorbarh(aes(xmin = lo, xmax = h), height = 0.1, color = "grey")+
    geom_point() +
    theme_cowplot() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "#DBEEFA")) +
    xlim(x.min, x.max) + 
    ggtitle(plot.title) +
    xlab("Effect Estimate")
  
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
  
  return(p.out)
}

#ggforest for 2smr packages
ggforest.mr <- function(data, exposure, outcome, beta, se, p, lo = NA, hi = NA, height = NA, width = 12, outprefix){
eval(parse(text = str_glue("p.dat <- data %>%
                                      select(exposure = {exposure},
                                          outcome = {outcome},
                                          beta = {beta},
                                          se = {se},
                                          p = {p}
                                      )")))
p.dat <- p.dat %>%
mutate(lo = beta - 1.96 * se,
    hi = beta + 1.96 * se) %>%
mutate(b.label = formatC(beta, format = "fg", digits = 2),
         lo.label = signif(lo, format = "fg", digits = 2),
         hi.label = signif(hi, format = "fg", digits = 2),
         se.label = str_glue("[{lo.label}:{hi.label}]"),
         p.label = prettyNum(p, digits = 2)) %>%
transform(variable = factor(variable, levels = rev(.$variable)))

#adjust size of the elements
x.min <- floor(min(p.dat$lo) * 4) / 4 
x.max <- ceiling(max(p.dat$hi) *4) /4
if(is.na(height)){
  height <- (nrow(p.dat) * 0.5) + 1
}

te <- plot.partial.table(data = p.dat, variable = "exposure", title = "Exposure", italic = F)
to <- plot.partial.table(data = p.dat, variable = "outcome", title = "Outcome", italic = F)

tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)

p <- p.dat %>% 
ggplot(aes(x = beta, y = exposure))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
  geom_point() +
  theme_cowplot() +
  theme(plot.title = element_text(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlim(x.min, x.max) + 
  ggtitle("") +
  xlab("Effect Estimate")

  #make folder
  fld <- str_replace(outprefix, paste0(basename(outprefix),"$"), "")
  system(str_glue("mkdir -p {fld}"))

  te + to + p + tb + tse + tp +
  plot_layout(widths = c(1, 1, 7, 1, 2, 1)) +
  plot_annotation(title = basename(outprefix))

  #save
  ggsave(str_glue("{outprefix}.png"), height = height, width = width)
  ggsave(str_glue("{outprefix}.pdf"), height = height, width = width)
}

ggforest.sem <- function(lavaan.fit, exposure, outcome, height = NA, width = 12, outprefix){

stopifnot(length(exposure) == length(outcome))
p.dat <- NULL
for (i in 1:length(exposure)){
  ex <- exposure[i]
  out <- outcome[i]

  t.dat <- lavaan.fit %>%
              summary() %>%
              .$PE %>%
              filter(lhs == {ex}, rhs == {out})

  p.dat <- rbind(p.dat, t.dat)
}
p.dat <- p.dat %>%
mutate(lo = est - 1.96 * se,
    hi = est + 1.96 * se) %>%
mutate(b.label = formatC(est, format = "fg", digits = 2),
         lo.label = signif(lo, format = "fg", digits = 2),
         hi.label = signif(hi, format = "fg", digits = 2),
         se.label = str_glue("[{lo.label}:{hi.label}]"),
         p.label = prettyNum(pvalue, digits = 2)) %>%
transform(variable = factor(lhs, levels = rev(.$lhs)))

#adjust size of the elements
x.min <- floor(min(p.dat$lo) * 4) / 4 
x.max <- ceiling(max(p.dat$hi) *4) /4
if(is.na(height)){
  height <- (nrow(p.dat) * 0.5) + 1
}

te <- plot.partial.table(data = p.dat, variable = "lhs", title = "Exposure", italic = F)
to <- plot.partial.table(data = p.dat, variable = "rhs", title = "Outcome", italic = F)

tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)

p <- p.dat %>% 
ggplot(aes(x = est, y = lhs))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
  geom_point() +
  theme_cowplot() +
  theme(plot.title = element_text(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlim(x.min, x.max) + 
  ggtitle("") +
  xlab("Effect Estimate")

  #make folder
  fld <- str_replace(outprefix, paste0(basename(outprefix),"$"), "")
  system(str_glue("mkdir -p {fld}"))

  te + to + p + tb + tse + tp +
  plot_layout(widths = c(1, 1, 7, 1, 2, 1)) +
  plot_annotation(title = basename(outprefix))

  #save
  ggsave(str_glue("{outprefix}.png"), height = height, width = width)
  ggsave(str_glue("{outprefix}.pdf"), height = height, width = width)
}


ggforest.lm <- function(data, 
                      plot.title="", 
                      lo = NA, 
                      hi = NA, 
                      height = NA, 
                      width = 10, 
                      outprefix,
                      left.table = ""){

  #assuming data <- summary(lm(...))$coef
  p.dat <- data %>%
            as.data.frame() %>%
            rownames_to_column("variable") %>%
                      select(variable,
                      beta = "Estimate", 
                      se = "Std. Error", 
                      p = "Pr(>|t|)", 
                      one_of(left.table)
                                      )
p.dat <- p.dat %>%
mutate(lo = beta - 1.96 * se,
    hi = beta + 1.96 * se) %>%
mutate(b.label = formatC(beta, format = "fg", digits = 2),
         lo.label = signif(lo, format = "fg", digits = 2),
         hi.label = signif(hi, format = "fg", digits = 2),
         se.label = str_glue("[{lo.label}:{hi.label}]"),
         p.label = prettyNum(p, digits = 2),
         title.length = nchar(as.character(variable))) %>%
transform(variable = factor(variable, levels = rev(.$variable)))

#adjust size of the elements
x.min <- min(p.dat$lo) - (max(p.dat$hi) - min(p.dat$lo))/10
x.max <- max(p.dat$hi) + (max(p.dat$hi) - min(p.dat$lo))/10
if(is.na(height)){
  height <- (nrow(p.dat) * 0.5) + 1
}

tt <- plot.partial.table(data = p.dat, variable = "variable", title = "Variable", italic = F)
tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "β", italic = T)
tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
title_length <- max(p.dat$title.length, nchar("Variable"))

p <- p.dat %>% 
ggplot(aes(x = beta, y = variable))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
  geom_point() +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  xlim(x.min, x.max) + 
  ggtitle(plot.title) +
  xlab("Effect Estimate")

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

  p.out

  #save
  ggsave(str_glue("{outprefix}.png"), height = height, width = width)
  ggsave(str_glue("{outprefix}.pdf"), height = height, width = width)

  return(p.out)
}

# #plot forest from metagen
# plot.forest.metagen.with.substudy <- function(t,
#                       plot.title="", 
#                       lo = NA, 
#                       hi = NA, 
#                       height = NA, 
#                       width = 10, 
#                       outprefix = NA,
#                       right.table = c("effect",'ci','p', "N", "Event"),
#                       right.table.labs = c("Estimate",'95% CI','P', "N", "CAD")){
#   meta::forest(t,
#        leftcols =c("studlab"),
#        leftlabs = c("Study"),
#        just='leftlabs',
#        colgap.left=unit(0.1,"cm"),
#        #xlim=c(0.8, 3),
#        #at=c(0.8,1,2,3),
#       smlab=plot.title,
#       colgap=unit(7, "mm"),
#       rightcols=right.table,
#       rightlabs=right.table.labs,
#       plotwidth=unit(6.5, "cm"),
#       print.Q=F,
#       overall=F,
#       comb.fixed=F,
#       print.byvar=F,
#       print.I2 = F,
#       print.tau2 = F,
#       common = F,
#       random = T,
#       test.subgroup.random = F,
#       subgroup.hetstat = T,
#       overall.hetstat = F,
#       addspace=T,
#       xlab = "Effect Estimate",
#       digits.ci=3)
# }
# if (is.na(outprefix)){
  
# }
# system("mkdir -p ../result/221115/il6/")
# for (lg in c("T", "F")){
# eval(parse(text = str_glue("ss <- ss.{lg}")))
# (t <- meta::metagen(TE = b,
#               seTE = se,
#               studlab = Study,
#               data = ss,
#               subgroup = CHIP,
#               sm = "EffectEstimate"))
# write_tsv(c(b = t$TE.random, p = t$pval.random) %>% as.data.frame() %>% rownames_to_column(), str_glue("../result/221115/il6/meta_rs2185834{lg}.tsv"))
# t$pval.random.w %>% 
#   as.data.frame() %>% 
#   setNames("p.random.study") %>% 
#   rownames_to_column("CHIP") %>% 
#   write_tsv(str_glue("../result/221115/il6/meta_study_rs2185834{lg}.tsv"))

# pdf(str_glue("../result/221115/il6/meta_rs2185834{lg}.pdf"), width = 10, height = 9)
# plot.forest.ss(t)
# dev.off()

# png(str_glue("../result/221115/il6/meta_rs2185834{lg}.png"), res = 300, width = 10, height = 9, units = "in")
# plot.forest.ss(t)
# dev.off()
# }

#ggforest for glm and OR
ggforest.glm <- function(data, 
                     variable, 
                     beta = "Estimate", 
                     se = "`Std. Error`", 
                     p = "`Pr(>|z|)`", 
                     plot.title="", 
                     lo = NA, 
                     hi = NA, 
                     height = NA, 
                     width = 10, 
                     outprefix,
                     left.table = "",
                     caption = NA){
  
  if (is.na(lo) & is.na(hi)) {
    eval(parse(text = str_glue("p.dat <- data %>%
	                           	        select(variable = {variable},
	                           	        		beta = {beta},
	                           	        		se = {se},
	                           	        		p = {p},
                                          one_of(left.table)
	                           	        )")))
    p.dat <- p.dat %>%
      mutate(lo = beta - 1.96 * se,
             hi = beta + 1.96 * se)
    
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
    mutate(beta = exp(beta),
           lo = exp(lo),
           hi = exp(hi)) %>% 
    mutate(b.label = formatC(beta, format = "fg", digits = 2),
           lo.label = formatC(lo, format = "fg", digits = 2),
           hi.label = formatC(hi, format = "fg", digits = 2),
           se.label = str_glue("[{lo.label}:{hi.label}]"),
           p.label = prettyNum(p, digits = 2),
           title.length = nchar(as.character(variable))) %>%
    transform(variable = factor(variable, levels = rev(.$variable)))
  
  #adjust size of the elements
  x.min <- min(p.dat$lo) - (max(p.dat$hi) - min(p.dat$lo))/10
  x.max <- max(p.dat$hi) + (max(p.dat$hi) - min(p.dat$lo))/10
  if(is.na(height)){
    height <- (nrow(p.dat) * 0.5) + 1
  }
  
  tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F)
  tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "OR", italic = F)
  tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
  tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
  title_length <- max(p.dat$title.length, nchar(variable))
  
  p <- p.dat %>% 
    ggplot(aes(x = beta, y = variable))+
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey")+
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "grey")+
    geom_point() +
    theme_cowplot() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    xlim(x.min, x.max) + 
    ggtitle(plot.title) +
    xlab("Odds Ratio")
  
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
  
  return(p.out)
}

#ggforest for coxph
ggforest.coxph.jacc <- function(data, 
                                variable, 
                                beta = "coef", 
                                se = "`se(coef)`", 
                                p = "`Pr(>|z|)`", 
                                plot.title="", 
                                lo = NA, 
                                hi = NA, 
                                height = NA, 
                                width = 10, 
                                outprefix,
                                left.table = "",
                                caption = NA,
                                xlog = FALSE,
                                xmin,
                                xmax){
  eval(parse(text = str_glue("p.dat <- data %>%
                                        select(variable = {variable},
                                            beta = {beta},
                                            se = {se},
                                            p = {p},
                                            one_of(left.table)
                                        )")))
  p.dat <- p.dat %>%
    mutate(lo = beta - 1.96 * se,
           hi = beta + 1.96 * se) %>%
    mutate(b.label = formatC(exp(beta), format = "fg", digits = 2),
           lo.label = formatC(exp(lo), format = "fg", digits = 2),
           hi.label = formatC(exp(hi), format = "fg", digits = 2),
           se.label = str_glue("[{lo.label}:{hi.label}]"),
           p.label = prettyNum(p, digits = 2),
           title.length = nchar(as.character(variable))) %>%
    transform(variable = factor(variable, levels = rev(.$variable)))
  
  #adjust size of the elements
  if (is.na(xmax)){
    xmax <- exp(max(p.dat$hi)) + (exp(max(p.dat$hi)) - exp(min(p.dat$lo)))/10
  }
  if(is.na(height)){
    height <- (nrow(p.dat) * 0.5) + 1
  }
  
  tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F)
  tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "HR", italic = F)
  tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
  tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
  title_length <- max(p.dat$title.length, nchar(variable))
  
  p <- p.dat %>% 
    ggplot(aes(x = exp(beta), y = variable))+
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey")+
    geom_errorbarh(aes(xmin = exp(lo), xmax = exp(hi)), height = 0.1, color = "grey")+
    geom_point() +
    theme_cowplot() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle(plot.title) +
    xlab("Hazard ratio")
  
  if (xlog){
    p <- p  +
      scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  }
  
  if (!is.null(xmin)){
    p <- p +
      xlim(c(xmin, xmax))
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
  
  return(p.out)
}

ggforest.coxph.jacc <- function(data, 
                           variable, 
                           beta = "coef", 
                           se = "`se(coef)`", 
                           p = "`Pr(>|z|)`", 
                           plot.title="", 
                           lo = NA, 
                           hi = NA, 
                           height = NA, 
                           width = 10, 
                           outprefix,
                           left.table = "",
                           caption = NA,
                           xlog = FALSE,
                           xmin,
                           xmax){
  eval(parse(text = str_glue("p.dat <- data %>%
                                        select(variable = {variable},
                                            beta = {beta},
                                            se = {se},
                                            p = {p},
                                            one_of(left.table)
                                        )")))
  p.dat <- p.dat %>%
    mutate(lo = beta - 1.96 * se,
           hi = beta + 1.96 * se) %>%
    mutate(b.label = formatC(exp(beta), format = "fg", digits = 2),
           lo.label = formatC(exp(lo), format = "fg", digits = 2),
           hi.label = formatC(exp(hi), format = "fg", digits = 2),
           se.label = str_glue("[{lo.label}:{hi.label}]"),
           p.label = prettyNum(p, digits = 2),
           title.length = nchar(as.character(variable))) %>%
    transform(variable = factor(variable, levels = rev(.$variable)))
  
  #adjust size of the elements
  if (is.na(xmax)){
    xmax <- exp(max(p.dat$hi)) + (exp(max(p.dat$hi)) - exp(min(p.dat$lo)))/10
  }
  if(is.na(height)){
    height <- (nrow(p.dat) * 0.5) + 1
  }
  
  tt <- plot.partial.table(data = p.dat, variable = "variable", title = variable, italic = F)
  tb <- plot.partial.table(data = p.dat, variable = "b.label", title = "HR", italic = F)
  tse <- plot.partial.table(data = p.dat, variable = "se.label", title = "95% CI", italic = F)
  tp <- plot.partial.table(data = p.dat, variable = "p.label", title = "P", italic = T)
  title_length <- max(p.dat$title.length, nchar(variable))
  
  p <- p.dat %>% 
    ggplot(aes(x = exp(beta), y = variable))+
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey")+
    geom_errorbarh(aes(xmin = exp(lo), xmax = exp(hi)), height = 0.1, color = "grey")+
    geom_point() +
    theme_cowplot() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "#ebf6fd")) +
    ggtitle(plot.title) +
    xlab("Hazard ratio") +
    scale_x_continuous(expand = c(0, 1))
  
  if (xlog){
    p <- p  +
      scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
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
  
  return(p.out)
}