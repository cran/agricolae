\name{NEWS}
\title{News for Package \pkg{agricolae}}

\section{Changes in version 1-3.7 (Oct 22, 2023)}{
  \itemize{
    \item CRAN checks (status ERROR). Changes in AMMI plot function, only biplot is considered.
  }
}

\section{Changes in version 1-3.6 (Jun 30, 2023)}{
  \itemize{
    \item CRAN checks (status NOTE), change name in documentation object ‘intervals.freq’ with ‘inter.freq’.
    \item Improvements in the DAU.test function. Update the standard error.
    \item Improvements in the audps function, includes when dates are not equidistant.
    \item Improvements in the bar.err and group.plot functions, Update the standard error.
    \item Improvements in the group.plot function, includes the 'decreasing' option for the decreasing order of the mean.
    \item Improvements in the sp.plot and ssp.plot functions, include the p-value in the random factor block.
    \item update PBIB.test when PBIB.test() is called from another function.
  }
}

\section{Changes in version 1-3.5 (Jun 5, 2021)}{
  \itemize{
    \item Improvements in the DAU.test function. controls can have different repetitions, at least two, do not use missing data.
    \item Update design.graeco() by new version R-4.0 and higher
    \item Improvements in the functions sp.plot, ssp.plot and strip.plot for the analysis of split plot and strip plot design, the mixed models with random blocks and fixed factors.
  }
}

\section{Changes in version 1-3.3 (June 5, 2020)}{
  \itemize{
    \item New function design.mat. Generate the design matrix from the fieldbook.
    \item Update plot.group() by new version R-4.0
  }
}

\section{Changes in version 1-3.2 (January 18, 2020)}{
  \itemize{
    \item The examples were edited.
    \item A control is set on the label limit for the functions order.group and orderPvalue.
    \item The plot.AMMI function uses only type = 1 and type = 2 options.
  }
}

\section{Changes in version 1-3.1 (April 4, 2019)}{
  \itemize{
    \item Improvement in the formation of Duncan test groups and the confidence limits of Scheffe.test.
    \item Shannon index on base log 2.
    \item Update documentation on multiple comparison tests.
  }
}

\section{Changes in version 1-2.9 (January 4, 2019)}{
  \itemize{
    \item Improvement in the formation of Duncan test groups and the confidence limits of Scheffe.test.
    \item Shannon index on base log 2.
    \item Update documentation on multiple comparison tests.
  }
}

\section{Changes in version 1-2.8 (September 12, 2017)}{
  \itemize{
    \item The order.group function  is again in agricolae.
    \item It is equivalent to the orderPvalue function in functional terms.
  }
}

\section{Changes in version 1-2.7 (August 30, 2017)}{
  \itemize{
    \item In the post.hoc tests, the grouping of treatments are formed according to 
the probability of the difference between treatments and the alpha level.
    \item The affected functions were BIB.test, DAU.test, duncan.test, durbin.test, 
friedman, HSD.test, kruskal, LSD.test, Median.test, PBIB.test, REGW.test, 
scheffe.test, SNK. Test, waller.test and waerden.test. Now there is good 
correspondence between the grouping and the pvalue.
    \item A new function (plot.group) is included in agricolae for the graphs of 
treatment groups and their variation by range, interquartil range, 
Standard deviation and standard error.
    \item The RANN package of suggestions was removed.
    \item Updated documentation.
  }
}

\section{Changes in version 1-2.6 (August 4, 2017)}{
  \itemize{
    \item Documentation check.
  }
}

\section{Changes in version 1-2.5 (July 20, 2017)}{
  \itemize{
    \item Add model object in output PBIB.test function.
    \item procedure duncan.test is better, the limitations in convergence were corrected.
    \item The influence in AMMI (type=3) is relative neighbor graph as a sub-graph.
    \item The post hoc nonparametrics tests (kruskal, friedman, durbin and waerden) 
are using the criterium Fisher's least significant difference (LSD)
  }
}

\section{Changes in version 1-2.4 (June 12, 2016)}{
  \itemize{
    \item Add suggests packages: RANN and rgeos to plot AMMI
    \item Concordance index in correlation function(), additional arg (method="lin").
    \item New function orderPvalue(). Grouping the treatments in a comparison with p.value minimum value (alpha)
    \item Test LSD.test and kruskal the adjust P.value (holm, hommel, hochberg, bonferroni, BH, BY, fdr). The comparison in pairs and groups give similar results.
  }
}

\section{Changes in version 1-2.3 (October 6, 2015)}{
  \itemize{
    \item REGW.test(). New function for multiple comparisons of treatments. 
(Ryan, Einot and Gabriel and Welsch)
    \item Concordance index in correlation function(), additional arg (method="lin").
    \item New function orderPvalue(). Grouping the treatments in a comparison with p.value minimum value (alpha)
    \item diffograph(). New function: Mean-mean scatter plot, test: Bonferroni, Fisher, Duncan, Student-Newman-Keul, Tukey, Kruskal-Wallis, Friedman and Waerden test.
    \item Changes in all comparison means, add parameters to facility function diffograph.
    \item Added randomization parameter (TRUE or FALSE) in all design function.
    \item Update Tutorial.
  }
}

\section{Changes in version 1-2.2 (August 12, 2015)}{
  \itemize{
    \item  Now in the frequency table shows the relative frequency as a percentage, the function is table.freq or summary( graph.freq or hist object)
    \item The histogram class is added to graph.freq and it can use the package HistogramTools
    \item The function design.bib create optimal design, use function optBlock(algDesign)
    \item sketch option in design: rbcd, lsd, graeco, youden, bib.
  }
}

\section{Changes in version 1-2.1 (August 25, 2014)}{
  \itemize{
    \item Move packages from Suggests to Imports.
    \item AUDPS. The Area Under the Disease Progress Stairs.
    \item AMMI stability value (ASV) and Yield stability index (YSI)
    \item Design youden.
    \item Now the PBIB.test function uses missing values.
  }
}

\section{Changes in version 1-2.0 (June 30, 2014)}{
  \itemize{
    \item AMMI: aditional parameters PC=FALSE or TRUE, output principal components, check error equal cero.
    \item plot.AMMI: graphic aditional parameters lwd = 1.8, length = 0.1 to arrow function.
    \item simulation.model: aditional parameter console=FALSE or TRUE, output in console.
    \item resampling.model: aditional parameter console=FALSE or TRUE, output in console.
    \item stability.par: aditional parameter console=FALSE or TRUE, output in console.
    \item stability.nonpar: aditional parameter console=FALSE or TRUE, output in console.
  }
}

\section{Changes in version 1-1.9 (June 17, 2014)}{
  \itemize{
    \item PBIB new parameter: group=TRUE. PBIB.test(block,trt,replication,y,k, method=c("REML","ML","VC"), test = c("lsd","tukey"), alpha=0.05, console=FALSE, group=TRUE). When you have many treatments to use group=FALSE.
    \item design.rcbd(..., continue=FALSE). Continue=TRUE or FALSE, continuous numbering of plot.
    \item Median.test. New function for multiple comparisons of treatments with Median.
    \item Now, AMMI function checks the minimum number of environments and genotypes. Now use console=TRUE or FALSE to output in screen. the graphs are produced by 
the plot function.
    \item plot.AMMI() or plot() functions generate plot of the AMMI with others principal components. type=1 (biplot), type=2 (triplot) and type=3 (influence genotype)
    \item Changed parameters by default "first = TRUE" in designs: rcbd, ab, split and lsd.
    \item Now vignettes in agricolae.
    \item change name ogive.freq by ojiva.freq, the parameters are same.
    \item AUDPC the evaluation parameter  now can be numeric vector. To see help(audpc)
  }
}

\section{Changes in version 1-1.8 (February 21, 2014)}{
  \itemize{
    \item zigzag(outdesign). The new function applied to designs: rcbd, lsd, graeco, split, strip, ab, alpha, bib, cyclic, lattice, dau. The outdesign is the output book the function design.###(). The function zigzag change the order number plots in serpentine form.
    \item Now, all design functions have two output objects: parameters and book, the parameters contain initial values that will allow reproduce the design and book contain field book.
    \item Now, all design functions have two output objects: parameters and book, the parameters contain initial values that will allow reproduce the design and book contain field book.
    \item The alpha and lattice designs have additionally two objects: statistics and field sketches.
    \item BIB have the statistics
  }
}
