`AMMI` <-
function (ENV, GEN, REP, Y, MSE = 0, number = TRUE, graph = "biplot",
    ...)
{
    name.y <- paste(deparse(substitute(Y)))
    cat("\nANALYSIS AMMI: ", name.y, "\nClass level information\n")
    ENV <- as.factor(ENV)
    GEN <- as.factor(GEN)
    nenv <- length(unique(ENV))
    ngen <- length(unique(GEN))
    cat("\nENV: ", unique(as.character(ENV)))
    cat("\nGEN: ", unique(as.character(GEN)))
    minimo <- min(ngen, nenv)
    if (length(REP) > 1) {
        REP <- as.factor(REP)
        nrep <- length(unique(REP))
        cat("\nREP: ", unique(REP))
        cat("\n\nNumber of observations: ", length(na.omit(Y)),
            "\n\n")
        modelo <- aov(Y ~ ENV + REP %in% ENV + GEN + ENV:GEN)
        cat("model Y:", name.y, " ~ ENV + REP%in%ENV + GEN + ENV:GEN\n")
        cat("Random effect REP%in%ENV\n\n")
        mm <- anova(modelo)
        nn <- mm[2, ]
        mm[2, ] <- mm[3, ]
        mm[3, ] <- nn
        row.names(mm)[2] <- "REP(ENV)"
        row.names(mm)[3] <- "GEN     "
        mm[1, 4] <- mm[1, 3]/mm[2, 3]
        mm[1, 5] <- 1 - pf(mm[1, 4], mm[1, 1], mm[2, 1])
        print(mm)
        DFE <- df.residual(modelo)
        MSE <- deviance(modelo)/DFE
        medy <- mean(Y, na.rm = TRUE)
        cat("\nCoeff var", "\tMean", name.y, "\n")
        cat(sqrt(MSE) * 100/medy, "\t", medy, "\n")
    }
    else {
        DFE <- nenv * (ngen - 1) * (REP - 1)
        DFEa <- nenv * (REP - 1)
        nrep <- REP
        modelo <- aov(Y ~ ENV + GEN)
        xx <- as.matrix(anova(modelo))
        xx <- rbind(xx[1, ], xx[1, ], xx[2:3, ],xx[3,])
		row.names(xx)[4] <- "ENV:GEN"
		row.names(xx)[5] <- "Residuals"
        xx[2, 1] <- DFEa
        xx[2, 2:5] <- NA
        xx[, 2] <- xx[, 2] * nrep
        xx[, 3] <- xx[, 3] * nrep
        xx[5, 1] <- DFE
        xx[5, 3] <- MSE
        xx[5, 2] <- MSE * DFE
        xx[1, 4] <- NA
        xx[3, 4] <- xx[3, 3]/MSE
        xx[4, 4] <- xx[4, 3]/MSE
        xx[1, 5] <- NA
        xx[3, 5] <- 1 - pf(xx[3, 4], xx[3, 1], DFE)
        xx[4, 5] <- 1 - pf(xx[4, 4], xx[4, 1], DFE)
        row.names(xx)[1] <- "ENV     "
        row.names(xx)[2] <- "REP(ENV)"
        cat("\nREP: ", REP)
        cat("\n\nNumber of means: ", length(na.omit(Y)), "\n")
        cat("\nDependent Variable:", name.y, "\n\nAnalysis of variance\n")
        print(xx, na.print = "")
        medy <- mean(Y, na.rm = TRUE)
        cat("\nCoeff var", "\tMean", name.y, "\n")
        cat(sqrt(MSE) * 100/medy, "\t", medy, "\n")
    }

    raw <- data.frame(ENV, GEN, Y)
    MEDIAS <- tapply(raw[, 3], raw[, c(1, 2)], mean)
    xx <- rownames(MEDIAS)
    yy <- colnames(MEDIAS)
    fila <- length(xx)
    col <- length(yy)
    total <- fila * col
    x <- character(length = total)
    y <- character(length = total)
    z <- numeric(length = total)
    k <- 0
    for (i in 1:fila) {
        for (j in 1:col) {
            k <- k + 1
            x[k] <- xx[i]
            y[k] <- yy[j]
            z[k] <- MEDIAS[i, j]
        }
    }
    MEDIAS <- data.frame(ENV = x, GEN = y, Y = z)
    x <- MEDIAS[, 1]
    y <- MEDIAS[, 2]
    z <- MEDIAS[, 3]
    modelo2 <- lm(z ~ x + y)
    for (i in 1:length(z)) {
        if (is.na(z[i]))
            z[i] <- predict(modelo2, data.frame(x = MEDIAS[i,
                1], y = MEDIAS[i, 2]))
    }
    MEDIAS <- data.frame(ENV = x, GEN = y, Y = z)
    modelo1 <- lm(Y ~ ENV + GEN, data = MEDIAS)
    residual <- modelo1$residuals
    MEDIAS <- data.frame(MEDIAS, RESIDUAL = residual)
    mlabel <- names(MEDIAS)
    names(MEDIAS) <- c(mlabel[1:2], name.y, mlabel[4])
    OUTRES <- MEDIAS[order(MEDIAS[, 1], MEDIAS[, 2]), ]
    OUTRES2 <- by(OUTRES[, 4], OUTRES[, c(2, 1)], function(x) sum(x,na.rm=TRUE))
    OUTMED <- by(OUTRES[, 3], OUTRES[, c(2, 1)], function(x) sum(x,na.rm=TRUE))
    s <- svd(OUTRES2)
    U <- s$u
    L <- s$d
    V <- s$v
    L <- L[1:minimo]
    SS <- (L^2) * nrep
    SUMA <- sum(SS)
    percent <- round(((1/SUMA) * SS) * 100, 1)
    minimo <- min(ngen, nenv)
    DFAMMI <- rep(0, minimo)
    acum <- DFAMMI
    MSAMMI <- DFAMMI
    F.AMMI <- DFAMMI
    PROBF <- DFAMMI
    acumula <- 0
    for (i in 1:minimo) {
        DF <- (ngen - 1) + (nenv - 1) - (2 * i - 1)
        if (DF <= 0)
            break
        DFAMMI[i] <- DF
        acumula <- acumula + percent[i]
        acum[i] <- acum[i] + acumula
        MSAMMI[i] <- SS[i]/DFAMMI[i]
        F.AMMI[i] <- round(MSAMMI[i]/MSE, 2)
        PROBF[i] <- round(1 - pf(F.AMMI[i], DFAMMI[i], DFE), 4)
    }
    SS <- round(SS, 6)
    MSAMMI <- round(MSAMMI, 6)
    SSAMMI <- data.frame(percent, acum, Df = DFAMMI, `Sum Sq` = SS,
        `Mean Sq` = MSAMMI, `F value` = F.AMMI, Pr.F = PROBF)
    nssammi <- nrow(SSAMMI)
    SSAMMI <- SSAMMI[SSAMMI$Df > 0, ]
    nss <- nrow(SSAMMI)
    row.names(SSAMMI) <- paste("PC", 1:nss, sep = "")
    cat("\nAnalysis\n")
    print(SSAMMI)
    LL <- sqrt(diag(L))
    SCOREG <- U %*% LL
    SCOREE <- V %*% LL
    SCORES <- rbind(SCOREG, SCOREE)
    colnames(SCORES) <- paste("PC", 1:nssammi, sep = "")
    MSCORES <- SCORES[1:ngen, ]
    NSCORES <- SCORES[(ngen + 1):(ngen + nenv), ]
    MGEN <- data.frame(type = "GEN", Y = apply(OUTMED, 1, mean),
        MSCORES)
    MENV <- data.frame(type = "ENV", Y = apply(OUTMED, 2, mean),
        NSCORES)
    bplot <- rbind(MGEN, MENV)
    bplot <- bplot[, 1:(nss + 2)]
    mlabel <- names(bplot)
    names(bplot) <- c(mlabel[1], name.y, mlabel[c(-1, -2)])
    maxy <- max(bplot[, 4])
    miny <- min(bplot[, 4])
    maxx <- max(bplot[, 3])
    minx <- min(bplot[, 3])
    row.names(bplot) <- c(row.names(MGEN), row.names(MENV))
    cp.name <- rownames(SSAMMI)[1:3]
    cp.per <- SSAMMI[1:3, 1]
    cp1 <- paste("PC 1 (",cp.per[1],")",sep="")
    cp2 <- paste("PC 2 (",cp.per[2],")",sep="")
    cp3 <- paste("PC 3 (",cp.per[3],")",sep="")
    if (graph == "biplot") {
        plot(bplot[, 3], bplot[, 4], cex = 0, xlab = cp1, ylab = cp2,
            frame = TRUE, ...)
        if (number == TRUE) {
            text(MGEN[, 3], MGEN[, 4], cex = 0, text(MGEN[, 3],
                MGEN[, 4], labels = as.character(1:nrow(MGEN)),
                col = "blue"))
        }
        if (number == FALSE) {
            text(MGEN[, 3], MGEN[, 4], cex = 0, text(MGEN[, 3],
                MGEN[, 4], labels = row.names(MGEN), col = "blue"))
        }
        points(MENV[, 3], MENV[, 4], cex = 0, text(MENV[, 3],
            MENV[, 4], labels = row.names(MENV), col = "brown"))
        abline(h = 0, v = 0, lty = 2.5, col = "green", lwd = 2)
        s <- seq(length(MENV[, 3]))
        arrows(0, 0, 0.9 * MENV[, 3][s], 0.9 * MENV[, 4][s],
            col = "brown", lwd = 1.8, length = 0.1, code = 2)
    }
    if (graph == "triplot") {
        y <- bplot
        lugar <- y[y[, 1] == "ENV", 3:5]
        clones <- y[y[, 1] == "GEN", 3:5]
        maxcp <- max(y[, 3:5])
        mincp <- min(y[, 3:5])
        rango <- maxcp - mincp
        clones <- (clones - mincp)/rango
        nclon <- nrow(clones)
        lugar <- (lugar - mincp)/rango
        nlugar <- nrow(lugar)
        point1 <- cbind(clones[, 1], clones[, 2], clones[, 3])
        point2 <- cbind(lugar[, 1], lugar[, 2], lugar[, 3])
        point3 <- cbind(c(0.6, 0.6, 0), c(0, 0.8, 0.8), c(0.6, 0, 0.6))
        suppressWarnings(warning(triplot(point1, cex = 0, grid = TRUE,
            label = "", center=TRUE,frame=TRUE)))
        if (number == TRUE)
            suppressWarnings(warning(text(tritrafo(point1), as.character(1:nclon),
                adj = c(0.5, 0), col = "blue", cex = 0.8)))
        if (number == FALSE)
            suppressWarnings(warning(text(tritrafo(point1), rownames(clones),
                adj = c(0.5, 0), col = "blue", cex = 0.8)))
        suppressWarnings(warning(text(tritrafo(point2), rownames(lugar),
            adj = c(0.5, 0), col = "red", cex = 0.8)))
        suppressWarnings(warning(text(tritrafo(point3), c(cp1,cp2,cp3),
            adj = c(0.5, 0), cex = 1)))
#        legend("topleft", NULL, pch = c("1", "2", "3"), cp.per,
#            , title = "PC     %", lty = 0)
        trilines(centerlines(3), lty = 2.5, col = "green", lwd = 2)
        for (i in 1:nlugar) {
            suppressWarnings(warning(trilines(c(point2[i, 1],
                1/3), c(point2[i, 2], 1/3), c(point2[i, 3], 1/3),
                col = "red", lty = 1)))
        }
    }
    return(list(genXenv = OUTRES2, analysis = SSAMMI, means = MEDIAS,
        biplot = bplot))
}
