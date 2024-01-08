mht.loci <- function (x, chr = "#CHROM", bp = "GENPOS", log10p = "LOG10P", snp = "ID", af = "A1FREQ", col = c("grey 30", "grey 70"), 
                      AlleleFreq = 0, chrlabs = NULL, genomewideline = -log10(5e-08), loci = NULL, loglogp = FALSE,
    annotatePval = NULL, annotateTop = TRUE, full_mode = FALSE, ...) 
{
    CHR = BP = P = index = NULL
    if (!(chr %in% names(x))) 
        stop(paste("Column", chr, "not found!"))
    if (!(bp %in% names(x))) 
        stop(paste("Column", bp, "not found!"))
    if (!(log10p %in% names(x))) 
        stop(paste("Column", p, "not found!"))
    if (!(snp %in% names(x))) 
        warning(paste("No SNP column found. OK unless you're trying to highlight."))
    if (is.data.table(loci))
        stop(paste("data.table format does not work for loci file. Please retry with 'data.table = F' option when you read loci file with fread."))
    if (!is.numeric(x[[chr]])) 
        stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
    if (!is.numeric(x[[bp]])) 
        stop(paste(bp, "column should be numeric."))
    d = data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[log10p]], AF = x[[af]])
    if (all(d$P <= 1))
        stop(paste("P value must be transformed by -log10"))
    if (!is.null(x[[snp]])) 
        d = transform(d, SNP = x[[snp]])
    d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
    d <- subset(d, (AF >= AlleleFreq))
    if (!full_mode)
        d <- subset(d, (P > 2))
    d <- d[order(d$CHR, d$BP), ]
    if (loglogp) {
        d$logp <- log(as.numeric(d$P))
    }
    else {
        d$logp <- as.numeric(d$P)
    }
    
    #define color scheme
    highlit.color <- c("#00468BB2", "#ED0000B2")
    
    #identify loci
    if (!is.null(loci)){
        new.loci <- loci[which(loci$KNOWN == 0), ]
        known.loci <- loci[which(loci$KNOWN > 0), ]

        new.id <- NULL
        for(i in 1:nrow(new.loci)){
            new.chr <- new.loci[i, "LOCUS_CHR"]
            new.pos.start <- new.loci[i, "LOCUS_FROM"]
            new.pos.end <- new.loci[i, "LOCUS_TO"]

          new.id.each <- as.character(d[which(d$CHR == new.chr & d$BP > new.pos.start & d$BP < new.pos.end), ]$SNP)
          new.id <- c(new.id, new.id.each)
        }

        known.id <- NULL
        for(i in 1:nrow(known.loci)){
            known.chr <- known.loci[i, "LOCUS_CHR"]
            known.pos.start <- known.loci[i, "LOCUS_FROM"]
            known.pos.end <- known.loci[i, "LOCUS_TO"]

          known.id.each <- as.character(d[which(d$CHR == known.chr & d$BP > known.pos.start & d$BP < known.pos.end), ]$SNP)
          known.id <- c(known.id, known.id.each)
        }
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
    if (!is.null(known.id)) {
        if (any(!(known.id %in% d$SNP))) 
            warning("You're trying to highlight SNPs that don't exist in your results.")
        d.known = d[which(d$SNP %in% known.id), ]
        with(d.known, points(pos, logp, col = highlit.color[1], pch = 20, 
            ...))
    }
    if (!is.null(new.id)) {
        if (any(!(new.id %in% d$SNP))) 
            warning("You're trying to highlight SNPs that don't exist in your results.")
        d.new = d[which(d$SNP %in% new.id), ]
        with(d.new, points(pos, logp, col = highlit.color[2], pch = 20, 
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
