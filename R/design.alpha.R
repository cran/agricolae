`design.alpha` <-
function (trt, k, r, number = 1, seed = 0, kinds = "Super-Duper")
{
    name.trt <- c(paste(deparse(substitute(trt))))
    ntr <- length(trt)
    if (seed != 0)
        set.seed(seed, kinds)
    s <- ntr/k
    if (ntr%%k!= 0)
        cat("\nThe size of the block is not appropriate", "\nthe number of treatments must be multiple of k (size block) \n")
    else {
        serie <- ""
        if (r == 2 & k<=s  ) {
            alpha <- matrix(0, nrow = k, ncol = r)
            alpha[2, 2] <- 1
            for (i in 3:k) {
                alpha[i, 2] <- alpha[i - 1, 2] + 1
            }
            serie <- "I"
        }
        if (r == 3 & s%%2 != 0 & k <= s  ) {
            alpha <- matrix(0, nrow = k, ncol = r)
            alpha[2, 2] <- 1
            alpha[2, 3] <- s - 1
            for (i in 3:k) {
                alpha[i, 2] <- alpha[i - 1, 2] + 1
                alpha[i, 3] <- alpha[i - 1, 3] - 1
            }
            serie <- "II"
        }
        if (r == 3 & s%%2 == 0 & k < s ) {
            s1 <- s/2
            alpha <- matrix(0, nrow = k, ncol = r)
            alpha[2, 2] <- 1
            alpha[2, 3] <- s1
            for (i in 3:k) {
                alpha[i, 2] <- alpha[i - 1, 2] + 1
                alpha[i, 3] <- alpha[i - 2, 3] + 1
            }
            serie <- "III"
        }
         if (r == 4 & s%%2 != 0 & s%%3 != 0 & k<=s) {
            s2 <- (s + 1)/2
            alpha <- matrix(0, nrow = k, ncol = r)
            alpha[2, 2] <- 1
            alpha[2, 3] <- s - 1
            alpha[2, 4] <- s2
            for (i in 3:k) {
                alpha[i, 2] <- alpha[i - 1, 2] + 1
                alpha[i, 3] <- alpha[i - 1, 3] - 1
                alpha[i, 4] <- alpha[i - 2, 4] + 1
            }
            serie <- "IV"
        }
        if (serie == "") {
            cat("\nhelp(design.alpha): to see the series of alpha generators\n")
        }
        else {
            nf <- nrow(alpha)
            nc <- ncol(alpha)
            cc <- rep(alpha[, 1], s)
            for (i in 2:r) {
                cc <- c(cc, rep(alpha[, i], s))
            }
            dim(cc) <- c(nf, s, r)
            for (m in 1:r) cc[, 1, m] <- alpha[, m]
            for (i in 2:s) {
                for (j in 1:nf) {
                  for (m in 1:r) {
                    cc[j, i, m] <- cc[j, i - 1, m] + 1
                    if (cc[j, i, m] >= s)
                      cc[j, i, m] <- 0
                  }
                }
            }
            for (j in 1:nf) {
                cc[j, , ] <- cc[j, , ] + (j - 1) * s
            }
            intermediate <-cc
            cat("\nalpha design (0,1) - Serie ", serie, "\n")
            E <- (ntr - 1) * (r - 1)/((ntr - 1) * (r - 1) + r *
                (s - 1))
            cat("\nParameters Alpha design\n=======================")
            cat("\ntreatmeans :", ntr)
            cat("\nBlock size :", k)
            cat("\nBlocks     :", s)
            cat("\nReplication:", r, "\n")
            cat("\nEfficiency factor\n(E )", E, "\n\n<<< Book >>>\n")
            for (m in 1:r) {
                for (j in 1:s) {
                  aleatorio <- sample(1:k, k)
                  cc[, j, m] <- cc[aleatorio, j, m]  # randomize block in rep
                }
            }
            for (m in 1:r) {
                aleatorio <- sample(1:s, s)
                cc[, , m] <- cc[, aleatorio, m]  # randomize col in rep
            }
            cc<-cc+1
            block <- gl(s, k)
            md <- as.numeric(cc[, , 1])
            bp <- sample(1:ntr, ntr)    # 
            trt <- trt[bp]              # randomize treatments
            mtr <- trt[md]              # assign to plot
            book <- data.frame(block = as.factor(block), trt = as.factor(mtr),
                replication = 1)
            for (i in 2:r) {
                md <- as.numeric(cc[, , i])
                mtr <- trt[md]
                book1 <- data.frame(block = as.factor(block),
                  trt = as.factor(mtr), replication = i)
                book <- rbind(book, book1)
            }
            plots <- number + 1:(s * k * r) - 1
            cols <- as.numeric(rep(gl(k, 1), s * r))
            book <- data.frame(plots = plots, cols = cols, book)
            book <- data.frame(row.names = NULL, book)
            book$block <- gl(s * r, k)
            names(book)[4] <- name.trt
            tr<-as.character(book[,4])
            dim(tr)<-c(k,s,r)
            if ( r == 2) design<-list(rep1=t(tr[,,1]),rep2=t(tr[,,2]))
            if ( r == 3) design<-list(rep1=t(tr[,,1]),rep2=t(tr[,,2]),rep3=t(tr[,,3]))
            if ( r == 4) design<-list(rep1=t(tr[,,1]),rep2=t(tr[,,2]),rep3=t(tr[,,3]),rep4=t(tr[,,4]))
            return(list(book=book, alpha=alpha, intermediate=intermediate, design=design))
        }
    }
}

