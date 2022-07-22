#specify below
#CHR = chr.col, BP = pos.col, SNP = snpid.col, P = p.col, col = c("lightskyblue1", "lightskyblue3"), suggestiveline = F, highlight1 = h.id, logp = log.bl
#10:94200000,6:8940000 +- 500kb will be highlited
infile = commandArgs(trailingOnly = TRUE)[1]
outfile = commandArgs(trailingOnly = TRUE)[2]
chr.col = commandArgs(trailingOnly = TRUE)[3]
pos.col = commandArgs(trailingOnly = TRUE)[4]
snpid.col = commandArgs(trailingOnly = TRUE)[5]
p.col = commandArgs(trailingOnly = TRUE)[6]
log.p = commandArgs(trailingOnly = TRUE)[7]
highlite = commandArgs(trailingOnly = TRUE)[8]

log.bl <- as.logical(log.p)

library(data.table)
library(tidyverse)
library(qqman)

res <- fread(infile, fill=TRUE)
res <- data.frame(CHR = res[[chr.col]],
                  BP = res[[pos.col]],
                  P = res[[p.col]],
                  SNP = res[[snpid.col]])

h1 <- str_split(highlite, pattern = ",")
h.id <- NULL
for(i in 1:length(h1[[1]])){
  h2 <- str_split(h1[[1]][i], pattern = ":")
  chr <- as.integer(h2[[1]][1])
  pos.start <- as.integer(h2[[1]][2])-500000
  pos.end <- as.integer(h2[[1]][2])+500000
  h3 <- res %>%
    filter(CHR == chr, BP > pos.start, BP < pos.end) %>%
    .$SNP %>%
    as.character()
  h.id <- c(h.id, h3)
}

manhattan1 <- function (x, chr = "CHR", bp = "BP", p = "P", snp = "SNP", col = c("lightskyblue1", "lightskyblue3"), chrlabs = NULL,
    genomewideline = -log10(5e-08), highlight = NULL, logp = TRUE,
    annotatePval = NULL, annotateTop = TRUE, ...)
{
    CHR = BP = P = index = NULL
    if (!(chr %in% names(x)))
        stop(paste("Column", chr, "not found!"))
    if (!(bp %in% names(x)))
        stop(paste("Column", bp, "not found!"))
    if (!(p %in% names(x)))
        stop(paste("Column", p, "not found!"))
    if (!(snp %in% names(x)))
        warning(paste("No SNP column found. OK unless you're trying to highlight."))
    if (!is.numeric(x[[chr]]))
        stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
    if (!is.numeric(x[[bp]]))
        stop(paste(bp, "column should be numeric."))
    if (!is.numeric(x[[p]]))
        stop(paste(p, "column should be numeric."))
    d = data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[p]])
    if (!is.null(x[[snp]]))
        d = transform(d, SNP = x[[snp]])
    d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
    d <- d[order(d$CHR, d$BP), ]
    if (logp) {
        d$logp <- -log10(d$P)
    }
    else {
        d$logp <- d$P
    }
    d$pos = NA
    d$index = NA
    ind = 0
    for (i in unique(d$CHR)) {
        ind = ind + 1
        d[d$CHR == i, ]$index = ind
    }
    nchr = length(unique(d$CHR))
    if (nchr == 1) {
        d$pos = d$BP
        ticks = floor(length(d$pos))/2 + 1
        xlabel = paste("Chromosome", unique(d$CHR), "position")
        labs = ticks
    }
    else {
        lastbase = 0
        ticks = NULL
        for (i in unique(d$index)) {
            if (i == 1) {
                d[d$index == i, ]$pos = d[d$index == i, ]$BP
            }
            else {
                lastbase = lastbase + tail(subset(d, index ==
                  i - 1)$BP, 1)
                d[d$index == i, ]$pos = d[d$index == i, ]$BP +
                  lastbase
            }
            ticks = c(ticks, (min(d[d$index == i, ]$pos) + max(d[d$index ==
                i, ]$pos))/2 + 1)
        }
        xlabel = "Chromosome"
        labs <- unique(d$CHR)
    }
    xmax = ceiling(max(d$pos) * 1.03)
    xmin = floor(max(d$pos) * -0.03)
    def_args <- list(xaxt = "n", bty = "n", xaxs = "i", yaxs = "i",
        las = 1, pch = 20, xlim = c(xmin, xmax), ylim = c(0,
            ceiling(max(d$logp))), xlab = xlabel, ylab = expression(-log[10](italic(p))))
    dotargs <- list(...)
    do.call("plot", c(NA, dotargs, def_args[!names(def_args) %in%
        names(dotargs)]))
    if (!is.null(chrlabs)) {
        if (is.character(chrlabs)) {
            if (length(chrlabs) == length(labs)) {
                labs <- chrlabs
            }
            else {
                warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
            }
        }
        else {
            warning("If you're trying to specify chromosome labels, chrlabs must be a character vector")
        }
    }
    if (nchr == 1) {
        axis(1, ...)
    }
    else {
        axis(1, at = ticks, labels = labs, ...)
    }
    col = rep(col, max(d$CHR))
    if (nchr == 1) {
        with(d, points(pos, logp, pch = 20, col = col[1], ...))
    }
    else {
        icol = 1
        for (i in unique(d$index)) {
            with(d[d$index == unique(d$index)[i], ], points(pos,
                logp, col = col[icol], pch = 20, ...))
            icol = icol + 1
        }
    }
    if (genomewideline)
        abline(h = genomewideline, col = "gray50", lty = "dashed", lwd = 2)
    if (!is.null(highlight)) {
        if (any(!(highlight %in% d$SNP)))
            warning("You're trying to highlight SNPs that don't exist in your results.")
        d.highlight = d[which(d$SNP %in% highlight), ]
        with(d.highlight, points(pos, logp, col = "indianred1", pch = 20,
            ...))
    }
    if (!is.null(annotatePval)) {
        topHits = subset(d, P <= annotatePval)
        par(xpd = TRUE)
        if (annotateTop == FALSE) {
            with(subset(d, P <= annotatePval), textxy(pos, -log10(P),
                offset = 0.625, labs = topHits$SNP, cex = 0.45),
                ...)
        }
        else {
            topHits <- topHits[order(topHits$P), ]
            topSNPs <- NULL
            for (i in unique(topHits$CHR)) {
                chrSNPs <- topHits[topHits$CHR == i, ]
                topSNPs <- rbind(topSNPs, chrSNPs[1, ])
            }
            textxy(topSNPs$pos, -log10(topSNPs$P), offset = 0.625,
                labs = topSNPs$SNP, cex = 0.5, ...)
        }
    }
    par(xpd = FALSE)
}

png(str_glue("{outfile}.manhattan.png"), width = 1500, height = 800, res = 120)
res %>%
  manhattan1(suggestiveline = F, highlight = h.id, logp = log.bl)
dev.off()

png(str_glue("{outfile}.qq.png"), width = 800, height = 800, res = 120)
  res %>%
    filter(is.numeric(LOG10P)) %>%
    qq(10^(-(res %>% filter(is.numeric(LOG10P)) %>% .$LOG10P)))
dev.off()

#Running script on UGER
#script=/medpop/esp2/tnakao/tools/script/manhattan.R
#ss=result/07_sumstat/regenie_CAD_CHIP.10_gwis.inter.sumstat.txt
#out_prefix=result/08_manhattan/regenie_CAD_CHIP.10_gwis.2df
#Rscript ${script} $ss $out_prefix CHROM GENPOS ID LOG10P FALSE 6:161010118,9:22092257,10:93834153
#see below for the details
#/medpop/esp2/tnakao/analyses/CHIP/CAD_CHIP/UKBB/220713_regenie/script/08_manhattan.sh
