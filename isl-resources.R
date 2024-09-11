#### READ IN LIBRARIES AND CUSTOM FNs

library(ggplot2)
library(tidyr)
library(tidyverse)
#library(janitor)
library(corrplot)
library(lattice)
library(lme4)
#library(eeptools)
library(kableExtra)
library(car)
library(igraph)
#library(magick)
library(mclogit)
library(stargazer)
library(apaTables)
library(sjPlot)
library(gridExtra)
library(xtable)
library(ggpubr)
library(XML)
library(ordinal)

##########################################################################################
#### custom functions

"bootstrap"<- function(x,nboot,theta,...,func=NULL) {
  call <- match.call()
  
  n <- length(x)
  bootsam<- matrix(sample(x,size=n*nboot,replace=TRUE),nrow=nboot)
  thetastar <- apply(bootsam,1,theta,...)
  func.thetastar <- NULL; jack.boot.val <- NULL; jack.boot.se <- NULL;
  if(!is.null(func)){
    match1 <- function(bootx,x){
      duplicated(c(bootx,x))[(length(x)+1) : (2*length(x))]
    } 
    matchs <- t(apply(bootsam,1,match1,x))
    func.thetastar <- func(thetastar)
    jack.boot <- function(inout,thetastar,func){
      func(thetastar[!inout])
    }
    jack.boot.val <- apply(matchs,2,jack.boot,thetastar,func)
    
    if(sum(is.na(jack.boot.val)>0)) {
      cat("At least one jackknife influence value for func(theta) is   undefined", 
          fill=TRUE)
      cat(" Increase nboot and try again",fill=TRUE)
      return()
    }
    
    if( sum(is.na(jack.boot.val))==0) {
      jack.boot.se <- sqrt( ((n-1)/n)*sum( (jack.boot.val-mean(jack.boot.val))^2 )  )
      
    }
  }
  
  return(list(thetastar=thetastar, func.thetastar=func.thetastar,
              jack.boot.val=jack.boot.val, jack.boot.se=jack.boot.se,
              call=call))
}

# Bootstrapping confidence intervals 
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}
na.mean <- function(x) {mean(x,na.rm=T)}
na.sum <- function(x) {sum(x,na.rm=T)}
na.min <- function(x) {min(x, na.rm=T)}
na.max <- function(x) {max(x, na.rm=T)}
na.med <- function(x) {median(x, na.rm=T)}
na.sd <- function(x) {sd(x, na.rm=T)}
################################################################################
### 
reportPercent <- function(variable){
  paste(round(100*na.mean(variable), 0), "%", sep="")
}

################################################################################
### 
asPercent <- function(value){
  paste(round(100*value, 0), "%", sep="")
}

################################################################################
### Format odds ratios for reporting in .Rmd text
reportORs <- function(table, variable){
  paste("OR=",
        sprintf("%.2f", table[variable, 1]),
        " [",
        paste(sprintf("%.2f", table[variable, 2]),
              sprintf("%.2f", table[variable, 3]),
              sep=", "), 
        "]", sep="")
}

################################################################################
### Format coefficients for reporting in .Rmd text
reportCoefs <- function(table, variable){
  paste("b=",
        sprintf("%.2f", table[variable, 1]),
        " [",
        paste(sprintf("%.2f", table[variable, 2]),
              sprintf("%.2f", table[variable, 3]),
              sep=", "), 
        "]", sep="")
}

################################################################################
### Format p value for reporting in .Rmd text
reportP <- function(pvalue) {
  if(pvalue>=.05) {
    return(paste("p=", sprintf("%.3f", pvalue), sep=""))
  }
  if(pvalue<.001) {
    return("p<.001")
  }
  if(pvalue<.01) {
    return("p<.01")
  }
  if(pvalue<.05) {
    return("p<.05")
  }
}

################################################################################
### Format anova model comparison results for reporting in .Rmd text
reportanova <- function(model1, model2){
  reportP <- function(pvalue) {
    if(pvalue>.05) {
      return(paste("p=", sprintf("%.3f", pvalue), sep=""))
    }
    if(pvalue<.001) {
      return("p<.001")
    }
    if(pvalue<.01) {
      return("p<.01")
    }
    if(pvalue<.05) {
      return("p<.05")
    }
  }
  anova_sum <- anova(model1, model2, test="Chisq")
  chisquared <- "\\chi^2"
  stat <- round(anova_sum[2, 'Chisq'], 2)
  df <- anova_sum[2, 'Df']
  p <- reportP(anova_sum[2, 'Pr(>Chisq)'])
  return(paste(chisquared, "(", df, ")=", stat, ", ", p, sep=""))
}

################################################################################
### Format Anova significance test for reporting in .Rmd text
reportAnova <- function(model, variable){
  reportP <- function(pvalue) {
    if(pvalue>=.05) {
      return(paste("p=", sprintf("%.3f", pvalue), sep=""))
    }
    if(pvalue<.001) {
      return("p<.001")
    }
    if(pvalue<.01) {
      return("p<.01")
    }
    if(pvalue<.05) {
      return("p<.05")
    }
  }
  Anova_sum <- car::Anova(model)
  walds <- "Wald "
  chisquared <- "\\chi^2"
  stat <- round(Anova_sum[variable, 1], 2)
  df <- Anova_sum[variable, 'Df']
  p <- reportP(Anova_sum[variable, 'Pr(>Chisq)'])
  return(paste(walds, chisquared, "(", df, ")=", stat, ", ", p, sep=""))
}

################################################################################
### Format confidence interval for reporting in .Rmd text
reportCIs <- function(summary_table_subset){
  CI.LOW <- summary_table_subset$ci.lo
  CI.HIGH <- summary_table_subset$ci.hi
  return(paste("[", 
               CI.LOW, 
               ", ",
               CI.HIGH,
               "]",
               sep=""))
}

################################################################################
### Format mean for reporting in .Rmd text
reportMean <- function(summary_table_subset){
  M <- summary_table_subset$mean
  return(paste("M=", 
               M,
               sep=""))
}

################################################################################
### Format range for reporting in .Rmd text
reportRange <- function(summary_table_subset){
  MIN <- summary_table_subset$min
  MAX <- summary_table_subset$max
  return(paste("range: ",
               MIN, 
               "-",
               MAX,
               sep=""))
}

################################################################################
### Format standard deviation for reporting in .Rmd text
reportSD <- function(summary_table_subset){
  SD <- summary_table_subset$sd
  return(paste("SD=", SD, sep=""))
}

################################################################################
### Format N for reporting in .Rmd text
reportn <- function(summary_table_subset){
  N <- as.numeric(summary_table_subset$n)
  return(paste("n=", N, sep=""))
}

################################################################################
### Format t-test results for reporting in .Rmd text
reportTTest <- function(vector1, vector2){
  reportP <- function(pvalue) {
    if(pvalue>=.05) {
      return(paste("p=", sprintf("%.3f", pvalue), sep=""))
    }
    if(pvalue<.001) {
      return("p<.001")
    }
    if(pvalue<.01) {
      return("p<.01")
    }
    if(pvalue<.05) {
      return("p<.05")
    }
  }
  ttest_summary <- t.test(vector1, vector2) 
  df <- sprintf("%.2f", ttest_summary$parameter)
  stat <- sprintf("%.2f", ttest_summary$statistic)
  p <- reportP(ttest_summary$p.value)
  return(paste("t(", df, ")=", stat, ", ", p, sep=""))
}
  
meanAssociationsByStd <- function(subset, variable, var_cols){
    subset %>%
      filter(!is.na(variable)) %>%
      dplyr::select(c("language", all_of(var_cols), standard)) %>%
      gather(., association, selection, -language, -standard) %>%
      group_by(association, language, standard) %>%
      summarize(mean=na.mean(selection)) %>%
      mutate(
      mean=cell_spec(mean, color=ifelse(mean>0.5, "green", "white"))
      ) %>%
      spread(standard, mean)
  }

tablifyAssociationsByStd <- function(subset, variable, var_cols){
  subset %>%
    filter(!is.na(variable)) %>%
    dplyr::select(c("language", all_of(var_cols), standard)) %>%
    gather(., association, selection, -language, -standard) %>%
    group_by(language, association, standard) %>%
    summarize(n = n(),
              mean=na.mean(selection),
              cilo = mean-ci.low(selection),
              cihi = mean+ci.high(selection)) %>%
    group_by(language, association, standard) %>%
    summarize(#n=n,
              M=paste(asPercent(mean), 
                      " [", 
                      asPercent(cilo),
                      ", ",
                      asPercent(cihi),
                      "]", 
                      sep="")) %>%
    spread(., standard, M)
}

tablifyAssociationsByLanguage <- function(subset, 
                                          variable, 
                                          var_cols, 
                                          var_labels) {
  interim_table <- subset %>%
    filter(!is.na(variable)) %>%
    dplyr::select(c("language", all_of(var_cols))) %>%
    gather(., association, selection, -language) %>%
    group_by(language, association) %>%
    summarize(n = na.sum(selection==1),
              mean=na.mean(selection),
              cilo = mean-ci.low(selection),
              cihi = mean+ci.high(selection)) 
    interim_table$association <- factor(interim_table$association,
                                        levels=var_cols, labels=var_labels)
    interim_table %>%
    group_by(association, language) %>%
    summarize(n=n,
              M=paste(asPercent(mean), 
                      " [", 
                      asPercent(cilo),
                      ", ",
                      asPercent(cihi),
                      "]", 
                      sep=""))
}

tablifyAssociationsByLanguage <- function(subset, 
                                          variable, 
                                          var_cols, 
                                          var_labels) {
  interim_table <- subset %>%
    filter(!is.na(variable)) %>%
    dplyr::select(c("language", all_of(var_cols))) %>%
    gather(., association, selection, -language) %>%
    group_by(language, association) %>%
    summarize(n = na.sum(selection==1),
              mean=na.mean(selection),
              cilo = mean-ci.low(selection),
              cihi = mean+ci.high(selection)) 
  interim_table$association <- factor(interim_table$association,
                                      levels=var_cols, labels=var_labels)
  interim_table %>%
    group_by(association, language) %>%
    summarize(n=n,
              M=(round(mean, 2)),
              `95% CI`= (paste(
                      " [", 
                      round(cilo, 2),
                      ", ",
                      round(cihi, 2),
                      "]", 
                      sep="")))
}

tablifyAssociationsByStd <- function(subset, variable, var_cols, var_labels){
  std3tab <- tablifyAssociationsByLanguage(subset[subset$standard=="3",],
                                           variable=variable, var_cols=var_cols,
                                           var_labels=var_labels)
  std5tab <- tablifyAssociationsByLanguage(subset[subset$standard=="5",],
                                           variable=variable, var_cols=var_cols,
                                           var_labels=var_labels)
  std7tab <- tablifyAssociationsByLanguage(subset[subset$standard=="7",],
                                           variable=variable, var_cols=var_cols,
                                           var_labels=var_labels)
  kable(cbind(std3tab, std5tab[,3:5], std7tab[,3:5])) %>%
    collapse_rows(1) %>%
    add_header_above(c(" "," ", "3rd" = 3, "5th" = 3, "7th" = 3))
}

tablifyFacesByLanguage <- function(subset, var_cols, var_labels) {
  interim_table <- subset %>%
    filter(!is.na(language)) %>%
    ungroup() %>%
    dplyr::select(c("language", all_of(var_cols))) %>%
    gather(., face, selection, -language) %>%
    group_by(language, face) %>%
    summarize(n = na.sum(selection>0),
              mean=na.mean(selection),
              cilo = mean-ci.low(selection),
              cihi = mean+ci.high(selection)) 
  interim_table$face <- factor(interim_table$face,
                                      levels=var_cols, labels=var_labels)
  interim_table %>%
    group_by(face, language) %>%
    reframe(n=n,
              M=(round(mean,2)),
            `95% CI`= paste(
              " [", 
              round(cilo, 2),
              ", ",
              round(cihi, 2),
              "]", 
              sep=""))
}

tablifyFacesByStd <- function(subset, var_cols, var_labels){
  std3tab <- tablifyFacesByLanguage(subset[subset$standard=="3",],
                                           var_cols=var_cols,
                                           var_labels=var_labels)
  std5tab <- tablifyFacesByLanguage(subset[subset$standard=="5",],
                                           var_cols=var_cols,
                                           var_labels=var_labels)
  std7tab <- tablifyFacesByLanguage(subset[subset$standard=="7",],
                                           var_cols=var_cols,
                                           var_labels=var_labels)
  kable(cbind(std3tab, std5tab[,3:5], std7tab[,3:5])) %>%
    collapse_rows(1) %>%
    add_header_above(c(" "," ", "3rd" = 3, "5th" = 3, "7th" = 3))
}

################################################################################
order_languages <- function(x) {
  factor(x, levels=language_labels)
}

## make standardized barplot of response rates
make_overall_barplot <- function(summary_table, fill_values, legend_title){
  ggplot(summary_table, aes(x=var_labeled, y=mean, fill=var_labeled)) +
    geom_bar(stat="identity")  + 
    geom_errorbar(aes(ymin=cilo, ymax=cihi), width=0, 
                  color=line_color) +
    facet_wrap(~language, ncol=2) +
    scale_fill_manual(values = fill_values, name=legend_title) +
    ylim(0, 1) +
    sans_theme +
    theme(legend.title = element_text(
      size=10, colour=line_color, family="sans", face="italic")) +
    ylab("Mean Proportion") 
}

make_lineplot_by_std <- function(
    summary_table_by_std, 
    color_values, shape_values, lty_values, legend_title="") {
  ggplot(summary_table_by_std, 
         aes(x=standard_num, y=mean, color=var_labeled)) +
    geom_line(aes(lty=var_labeled), linewidth=1.1) + 
    geom_point(aes(shape=var_labeled)) +
    geom_errorbar(aes(ymin=cilo, ymax=cihi), width=0) +
    facet_wrap(~language, ncol=4) +
    scale_color_manual(values=color_values, legend_title) + 
    scale_shape_manual(values=shape_values, legend_title) +
    scale_linetype_manual(values=lty_values, legend_title) +
    ylim(0, 1) +
    scale_x_continuous(breaks=c(3, 5, 7), label=std_axis_labels) +
    sans_axes_theme +
    theme(legend.position = "bottom") +
    xlab("Grade") +
    ylab("Mean Proportion")
}

make_barplot_by_std <- function(summary_table_by_std, 
                                fill_values, 
                                legend_title="") {
  ggplot(summary_table_by_std, 
    aes(x=var_labeled, y=mean, fill=var_labeled)) +
    geom_bar(stat="identity") + 
    geom_errorbar(aes(ymin=cilo, ymax=cihi), width=0, 
                  color=line_color) + 
    facet_wrap(~language + standard, ncol=3) +
    scale_fill_manual(values = fill_values, legend_title) +
    ylim(0, 1) +
    xlab("Grade") +
    ylab("Mean Proportion") +
    sans_axes_theme +
    theme(legend.position = "bottom",
          legend.key=element_rect(color=NA),
          legend.margin = margin(-5,0,0,0))
}

make_age_lm_plot <- function(data, 
                             color_values, lty_values, 
                             legend_title=""){
  ggplot(data, 
         aes(x=child_age_centered, 
             y=selection, color=var_labeled, 
             lty=var_labeled, fill=var_labeled)) +
    stat_smooth(aes(x=child_age_centered, y=selection), 
                method="lm", level=.75, fullrange=T) +
    facet_wrap(~language, ncol=4) +
    scale_color_manual(values = color_values, legend_title) +
    scale_linetype_manual(values=lty_values, legend_title) +
    scale_fill_manual(values=color_values, legend_title) +
    ylim(0, 1) +
    ylab("Mean Proportion") +
    xlab("Child Age (yrs)") +
    sans_axes_theme
}

save_plot <- function(plot=last_plot(), 
                      plot_class=c(
                        "barplot_overall", "barplot_std", "line_std",
                        "age_lm"
                        ), 
                      which_data){
  if (plot_class=="barplot_overall"){
    filename <- paste(which_data, ".pdf", sep="")
    ggsave(filename=filename, plot=plot, device="pdf", 
           path="plots/barplots_overall",
           scale=1, width=7, height=6, units="in")
    print(filename)
  }
  if (plot_class=="barplot_std"){
    filename <- paste(which_data, "_std_barplot.pdf", sep="")
    ggsave(filename=filename, plot=plot, device="pdf", 
           path="plots/by_standard/barplots",
           scale=2, width=7, height=9.5, units="in")
    print(filename)
  }
  if (plot_class=="line_std"){
    filename <- paste(which_data, "_std_line.pdf", sep="")
    ggsave(filename=filename, plot=plot, device="pdf", 
           path="plots/by_standard/line_plots",
           scale=1, width=7, height=5, units="in")
    print(filename)
  }
  if (plot_class=="age_lm"){
    filename <- paste(which_data, "_age_lm.pdf", sep="")
    ggsave(filename=filename, plot=plot, device="pdf", 
           path="plots/by_age",
           scale=1, width=7, height=5, units="in")
    print(filename)
  }
  
}

make_multinomial_html <- function(model, 
                                 prefix=c("geo", "rel", "w", "fa", "fl", "fp"), 
    model_class=c("primary", "familiarity", "gender")) {
  if (model_class=="primary"){
    filename <- paste(prefix, "_mod_tab.html", sep="")
    file_path <- paste("tables/models/primary/", filename, sep="")
    model <- model
    sjPlot::tab_model(
      model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, 
      p.style = "numeric_stars", emph.p = TRUE, 
      file = file_path)
  }
  if (model_class=="familiarity"){
    filename <- paste(prefix, "_fam_mod_tab.html", sep="")
    file_path <- paste("tables/models/familiarity/", filename, sep="")
    sjPlot::tab_model(
      model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, 
      p.style = "numeric_stars", emph.p = TRUE, 
      file = file_path)
  }
  if (model_class=="gender"){
    filename <- paste(prefix, "_sex_tab.html", sep="")
    file_path <- paste("tables/models/gender/", filename, sep="")
    sjPlot::tab_model(
      model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, 
      p.style = "numeric_stars", emph.p = TRUE, 
      file = file_path)
  }
}

make_forest_plot <- function(model){
  sjPlot::plot_model(model, axis.lim=c(0.01, 100), 
                     #show.values=T,
                     #show.p=T,
                     dot.size=1) 
}

get_point_sizes <- function(df_for_plots, reference_level) {
  df_for_plots %>% 
  filter(var_labeled!=reference_level) %>%
    distinct(var_labeled, language, id) %>%
    group_by(var_labeled, language) %>%
    summarize(n=n()) %>%
    mutate(facet=var_labeled) %>%
    mutate(term=language) %>%
    ungroup() %>%
    dplyr::select(term, facet, n)
}

edit_forest_plot_data <- function(plot, n_tab, 
                                  ordered_facet_levels, 
                                  terms=language_labels) {
  #terms = c(language_labels, "age")
  terms=terms
  plot$data_temp <- plot$data 
  plot$data_temp$term <- gsub("language", "", plot$data$term)
  #plot$data_temp$term <- gsub("child_", "", plot$data_temp$term)
  #plot$data_temp$term <- gsub("_centered", "", plot$data_temp$term)
  plot$data_temp$term <- factor(plot$data_temp$term, levels=terms) %>%
    fct_rev(.)
  plot$data <- merge(plot$data_temp, n_tab, by=c("facet", "term")) %>% 
    filter(term %in% terms)
  #plot$data$n[plot$data$term=="age"] <- NA
  plot$data$facet <- factor(plot$data$facet, levels=ordered_facet_levels)
  #plot$data$facet <- paste(plot$data$facet, "\n", "")
  return(plot)
}

update_forest_plot <- function(plot, title){
  plot + 
    geom_hline(yintercept=1, lty=2, color="#000080", linewidth=1.05) + 
    geom_pointrange(shape=18, size=1, aes(ymin=xmin, ymax=xmax)) +
    geom_point(aes(size=n),  color="gray40", shape=1) +
    # geom_text(aes(label=p.stars), nudge_x=0.25) +
    scale_size(name="n children", breaks=c(20, 40, 60, 80, 100), 
               range = c(2, 11)) +
    theme(panel.background = element_rect(fill=NA, color=line_color),
          panel.grid = element_line(color="gray80", linetype="dotted"),
          panel.spacing.x=unit(.75, "lines"),
          strip.background = element_rect(fill="white", color=line_color, 
                                          linewidth = .75),
          strip.text = element_text(size=12, 
                                    color=line_color, face="bold.italic"),
          title=element_text(color=line_color, face="bold.italic"),
          legend.key = element_rect(linewidth=0, color=NA, fill=NA),
          legend.text = element_text(color=line_color, size=11),
          legend.box.margin = margin(5,0,0,-7),
          axis.text.y = element_text(color=line_color, size=11, 
                                     face="bold"),
          axis.title=element_text(color=line_color, size=11, face="bold"),
          axis.title.x=element_text(margin=margin(7,0,0,0)),
          #axis.title.y=element_text(margin=margin(0,-11,0,0))
          ) +
        scale_color_manual(values=c("#ff7f00", "#009000")) + 
        labs(title=title) +
    xlab("Language (audio)")
}

save_forest_plot_svg <- function(plot, filename) {
  ggsave(file=filename, plot=plot, device="pdf", path="plots/forest_plots/", 
         scale = 1, width = 7, height = 6)
}

save_forest_plot_png <- function(plot, filename) {
  ggsave(file=filename, plot=plot, device="png", path="plots/forest_plots/", 
         scale = 1, width = 6.75, height = 6, units="in")
}

save_combined_plot_svg <- function(arrangement, filename, 
                                   path="plots/combined/") {
  ggsave(file=filename, plot=arrangement, path=path,
         width = 190, scale=2, units ='mm')
}

relevel_languages <- function(factor) {
  factor(factor, levels=language_labels)
}
################################################################################
### Format F-test results for reporting in .Rmd text
reportFTest <- function(model, parameter){
  reportP <- function(pvalue) {
    if(pvalue>.05) {
      return(paste("p=", sprintf("%.3f", pvalue), sep=""))
    }
    if(pvalue<.001) {
      return("p<.001")
    }
    if(pvalue<.01) {
      return("p<.01")
    }
    if(pvalue<.05) {
      return("p<.05")
    }
  }
  ftest_summary <- Anova(model) 
  df1 <- ftest_summary[parameter, "Df"]
  df2 <- ftest_summary["Residuals", "Df"]
  stat <- sprintf("%.2f", ftest_summary[parameter, "F value"])
  p <- reportP(ftest_summary[parameter, "Pr(>F)"])
  return(paste("F(", df1, ", ", df2, ")=", stat, ", ", p, sep=""))
}

##########################################################################################
#### ggplot variables for figures
## standard (grade) axis labels
std_axis_labels <- c(" 3rd", "5th", "7th ")

language_vars <- c("gujarati", "hindi", "urdu", "marathi", "tamil", 
                   "english_american", "english_indian", "chinese")
## by language 
language_colors <- c(
  "gujarati" = "#ff7f00", # dark orange
  "marathi" = "#fdbf6f", # light orange
  "hindi" = "#6a3d9a", # dark purple
  "urdu" = "#cab2d6", # light purple
  "tamil" = "#e31a1c", # red
  "english_indian" = "#33a02c", # dark green
  "english_american" = "#b2df8a", # light green
  "chinese" = "#f781bf"# pink
)

language_labels <- c("Gujarati", "Hindi", "Urdu", "Marathi", "Tamil", 
                     "English (India)", "English (U.S.)", "Mandarin")
language_colors <- c(
  "Gujarati" = "#ff7f00", # dark orange
  "Marathi" = "#fdbf6f", # light orange
  "Hindi" = "#6a3d9a", # dark purple
  "Urdu" = "#cab2d6", # light purple
  "Tamil" = "#e31a1c", # red
  "English (India)" = "#33a02c", # dark green
  "English (U.S.)" = "#b2df8a", # light green
  "Mandarin" = "#f781bf"# pink
)
face_colors <- c(
  "hindu_face" = "#1b9e77", # teal
  "muslim_face" = "#7570b3", # purple
  "dravidian_face" = "#e7298a", # magenta
  "asian_face" = "#d95f02", # burnt orange
  "white_face" = "#e6ab02", # goldenrod 
  "no_opinion_face" = "gray65"
)


language_name_vars <- c("gujarati", "hindi", "urdu", "marathi", "tamil", 
                        "english", "chinese")

language_name_labels <- c('"Gujarati"', '"Hindi"', '"Urdu"', '"Marathi"', 
                          '"Tamil"', '"English"', '"Chinese"')

language_name_colors <- c(
  '"Gujarati"' = "#ff7f00", # dark orange
  '"Hindi"' = "#6a3d9a", # dark purple
  '"Urdu"' = "#cab2d6", # light purple
  '"Marathi"' = "#fdbf6f", # light orange
  '"Tamil"' = "#e31a1c", # red
  #"English" = "#33a02c", # dark green
  '"English"' = "#b2df8a", # light green
  '"Chinese"' = "#f781bf"# pink
)

sharedlang_labels <- c("Gujarati", "Hindi", "Urdu", 
                       "Marathi", "Tamil", "Mandarin")

sharedlang_colors <- c(
  '"Gujarati"' = "#ff7f00", # dark orange
  '"Marathi"' = "#fdbf6f", # light orange
  '"Hindi"' = "#6a3d9a", # dark purple
  '"Urdu"' = "#cab2d6", # light purple
  '"Tamil"' = "#e31a1c", # red
  '"Chinese"' = "#f781bf"# pink
)

language_shapes <- c(
  "Gujarati" = 0, # dark orange
  "Marathi" = 1, # light orange
  "Hindi" = 2, # dark purple
  "Urdu" = 5, # light purple
  "Tamil" = 6, # red
  "English (India)" = 4, # dark green
  "English (U.S.)" = 11, # light green
  "Mandarin" = 8
)
language_ltys <- c(
  "Gujarati" = "solid",
  "Marathi" = "longdash",
  "Hindi" = "dotdash", 
  "Urdu" = "dashed", 
  "Tamil"  = "twodash",
  "English (India)" = "dotted",
  "English (U.S.)" = "dotted",
  "Mandarin" = "longdash"
)

## by face
face_vars <- c("hindu_face", "muslim_face", "dravidian_face", 
                  "white_face", "asian_face", "no_opinion_face")

face_labels <- c("Hindu", "Muslim", "Dravidian",  "White", 
                "East Asian", "No Opinion")
face_labels <- c("HINDU", "MUSLIM", "DRAVIDIAN",  "WHITE", 
                 "ASIAN", "No Opinion")

face_colors <- c(
  "hindu_face" = "#1b9e77", # teal
  "muslim_face" = "#7570b3", # purple
  "dravidian_face" = "#e7298a", # magenta
  "asian_face" = "#d95f02", # burnt orange
  "white_face" = "#e6ab02", # goldenrod 
  "no_opinion_face" = "gray65"
)

face_colors <- c(
  "Hindu" = "#1b9e77", # teal
  "Muslim" = "#7570b3", # purple
  "Dravidian" = "#e7298a", # magenta
  "East Asian" = "#d95f02", # burnt orange
  "White" = "#e6ab02", # goldenrod 
  "No Opinion" = "gray65"
)

face_colors <- c(
  "HINDU" = "#1b9e77", # teal
  "MUSLIM" = "#7570b3", # purple
  "DRAVIDIAN" = "#e7298a", # magenta
  "ASIAN" = "#d95f02", # burnt orange
  "WHITE" = "#e6ab02", # goldenrod 
  "No Opinion" = "gray65" #grey
)

## likert questions
likert_levels <- c(
  "tell_a_lot", "tell_where_from", "tell_education", 
  "french", "ancestors", "indian"
)
likert_labels <- c(
  "Tell a lot about", "Tell where from", "Tell education", 
  "Learn French", "Ancestor's Language", "Learn Indian Language"
)
likert_colors <- c(
  "Tell a lot about" = "#66a61e",
  "Tell where from" = "#e6ab02",
  "Tell education" = "#7570b3", 
  "Learn French" = "#e7298a", 
  "Ancestor's Language" = "#1b9e77",
  "Learn Indian Language" = "#d95f02"
)
likert_legend_title <- "Likert Question"
likert_shapes <- c(
  "Tell a lot about" = 0,
  "Tell where from" = 1,
  "Tell education" = 2,
  "Learn French" = 5,
  "Ancestor's Language" = 6, 
  "Learn Indian Language" = 4
)
likert_ltys <- c(
  "Tell a lot about" = "solid",
  "Tell where from" = "longdash",
  "Tell education" = "dotdash", 
  "Learn French" = "dashed", 
  "Ancestor's Language" = "twodash",
  "Learn Indian Language" = "dotted"
)

face_shapes<- c(
  "Hindu" = 0, # teal
  "Muslim" = 1, # purple
  "Dravidian" = 2, # magenta
  "East Asian" = 5, # burnt orange
  "White" = 6, # goldenrod 
  "No Opinion" = 4
)

face_shapes<- c(
  "HINDU" = 0, # teal
  "MUSLIM" = 1, # purple
  "DRAVIDIAN" = 2, # magenta
  "ASIAN" = 5, # burnt orange
  "WHITE" = 6, # goldenrod 
  "No Opinion" = 4
)

face_ltys<- c(
  "Hindu" = "solid",
  "Muslim" = "longdash",
  "Dravidian" = "dotdash",
  "East Asian" = "dashed", 
  "White" = "twodash",
  "No Opinion" = "dotted"
)

face_ltys<- c(
  "HINDU" = "solid",
  "MUSLIM" = "longdash",
  "DRAVIDIAN" = "dotdash",
  "ASIAN" = "dashed", 
  "WHITE" = "twodash",
  "No Opinion" = "dotted"
)

learning_face_colors <- c(
  "hindu" = "#1b9e77", # teal
  "white" = "#e6ab02" # goldenrod 
)

line_color <- "gray20"
  
face_legend_title1 <- "Face"
face_legend_title <- ""
  
## by origin
origin_vars <- c("gujarat", 
                 "india", 
                 "foreign", 
                 "no_opinion_geo")

origin_labels <- c("Gujarat (Same State)", 
                   "India (Different State)", 
                   "Foreign (Outside India)",
                   "No Opinion")

origin_colors <- c(
  "Gujarat (Same State)" = "#ff7f00", # dark orange
  "India (Different State)" = "#33a02c", # dark green
  "Foreign (Outside India)" = "#6a3d9a",   # dark purple
  "No Opinion" = "gray65"
)

origin_shapes<- c(
  "Gujarat (Same State)" = 0, 
  "India (Different State)" = 1, 
  "Foreign (Outside India)" = 2,   
  "No Opinion" = 4 #"x"
)

origin_ltys<- c(
  "Gujarat (Same State)" = "solid", # 
  "India (Different State)" = "longdash", # dark purple
  "Foreign (Outside India)" = "dotdash",   # dark green
  "No Opinion" = "dotted"
)

origin_legend_title1 <- "This speaker is from..."
origin_legend_title <- ""

## by religion
religion_vars <- c("hindu", 
                 "muslim", 
                 "jain", 
                 "christian",
                 "buddhist",
                 "no_opinion_religion")

religion_labels <- c("Hindu", "Muslim", "Jain", "Christian", 
                     "Buddhist", "No Opinion")

religion_colors <- c(
  "Hindu" = "#f9a602", # deep saffron
  "Muslim" = "#009000", # "islamic green"
  "Jain" = "#000080", # navy blue
  "Christian" = "#e31a1c", # red
  "Buddhist" = "#0000ff", # high blue
  "No Opinion" = "gray65"
)

religion_fills <- c(
  "hindu" = "#f9a602", # deep saffron
  "muslim" = "#009000", # "islamic green"
  "jain" = "#000080", # navy blue
  "christian" = "#e31a1c", # red
  "buddhist" = "#0000ff", # high blue
  "no opinion" = "gray65"
)

religion_shapes<- c(
  "Hindu" = 0,
  "Muslim" = 1,
  "Jain" = 2,
  "Christian" = 5, # red
  "Buddhist" = 6, # high blue
  "No Opinion" = 4
)

religion_ltys<- c(
  "Hindu" = "solid",
  "Muslim" = "longdash",
  "Jain" = "dotdash",
  "Christian" = "dashed", 
  "Buddhist" = "twodash",
  "No Opinion" = "dotted"
)

religion_legend_title1 <- "This speaker's religion is..."
religion_legend_title <- ""

##### by wealth 
wealth_vars <- c("poorer", "same", "richer", "no_opinion_wealth")

wealth_labels1 <- c("Less Wealthy", "As Wealthy", 
                    "More Wealthy", "No Opinion")

wealth_labels <- c("Less Money", "As Much Money", 
                   "More Money", "No Opinion")

wealth_colors1 <- c(
  "Less Wealthy" = "#56614a", # 
  "As Wealthy" = "#99ba6a", # 
  "More Wealthy" = "#a5d48f", # 
  "No Opinion" = "gray85"
)

wealth_colors <- c(
  "Less Money" = "#56614a", # 
  "As Much Money" = "#99ba6a", # 
  "More Money" = "#a5d48f", # 
  "No Opinion" = "gray65"
)

wealth_colors <- c(
  "Less Money" = "#a1d99b", # 
  "As Much Money" =  "#31a354", 
  "More Money" =  "#006d2c",   
  "No Opinion" = "gray65"
)
#"#74c476",
#"#238b45", #
wealth_shapes<- c(
  "Less Money" = 0, # 
  "As Much Money" = 1, # 
  "More Money" = 2, # 
  "No Opinion" = 4
)

wealth_ltys<- c(
  "Less Money" = "dotdash", # 
  "As Much Money" = "longdash", # 
  "More Money" = "solid", # 
  "No Opinion" = "dotted"
)

wealth_legend_title1 <- "This speaker has..."
wealth_legend_title <- ""

#### themes
face_theme <- theme(
  #axis.title.x = element_text(size=12, colour=line_color, family="mono"),
  #axis.title.y = element_text(size=12, colour=line_color, family="mono"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  #axis.text.x = element_text(size=12, colour=line_color, family="mono", angle=60, hjust=.8),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  #axis.title.y = element_text(size=12, colour="gray42", family="mono"),
  #axis.text.y = element_text(size=12, colour="gray42", family="mono"),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  panel.background = element_rect(colour="gray80", fill="white"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  #plot.title = element_blank(),
  plot.title = element_text(size=12, colour=line_color, family="mono"),
  plot.subtitle = element_text(size=12, colour=line_color, family="mono"),
  legend.position="right",
  legend.title = element_text(size=12, colour=line_color, family="mono"),
  legend.text  = element_text(size=10, colour=line_color, family="mono"),
  strip.background = element_rect(
    color="gray90", fill="gray95"),
  strip.text.x=element_text(size=12, colour=line_color, family="mono")
)

face_axes_theme <- theme(
  axis.title.x = element_text(size=10, colour=line_color, family="mono"),
  axis.title.y = element_text(size=10, colour=line_color, family="mono"),
  #axis.title.x = element_blank(),
  #axis.title.y = element_blank(),
  #axis.text.x = element_text(size=12, colour="gray42", family="mono", angle=60, hjust=.8),
  axis.text.x = element_text(size=8, colour=line_color, family="mono"),
  axis.ticks.x = element_blank(),
  #axis.title.y = element_text(size=12, colour="gray42", family="mono"),
  #axis.text.y = element_text(size=12, colour="gray42", family="mono"),
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(size=8, colour=line_color, family="mono"),
  panel.background = element_rect(colour="gray80", fill="white"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  #plot.title = element_blank(),
  plot.title = element_text(size=10, colour=line_color, family="mono"),
  plot.subtitle = element_text(size=10, colour=line_color, family="mono"),
  legend.position="right",
  legend.title = element_text(size=8, colour=line_color, family="mono"),
  legend.text  = element_text(size=8, colour=line_color, family="mono"),
  strip.background = element_rect(
    color="gray90", fill="gray95"),
  strip.text.x=element_text(size=10, colour=line_color, family="mono"),
  legend.key = element_rect(fill=NA)
)

## switching to sans serif for manuscript
### theme without axis titles/labels
sans_theme <- theme(
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_text(size=8, colour=line_color, family="sans"),
  axis.title.y = element_text(size=10, colour=line_color, 
                              family="sans", face="bold"),
  panel.background = element_rect(colour="gray50", fill="white"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(size=10, colour=line_color, family="sans"),
  plot.subtitle = element_text(size=10, colour=line_color, family="sans"),
  legend.position="right",
  legend.title = element_text(size=10, colour=line_color, family="sans"),
  legend.text  = element_text(size=10, colour=line_color, family="sans"),
  strip.background = element_rect(
    color="gray50", fill=NA),
  strip.text.x=element_text(size=10, colour=line_color, family="sans", face="bold"),
  legend.key = element_rect(fill=NA)
)

### theme with axis titles/labels
sans_axes_theme <- theme(
  axis.title.y = element_text(size=10, colour=line_color, 
                              family="sans", face="bold"),
  axis.title.x = element_text(size=10, colour=line_color, 
                             family="sans", face="bold",
                             margin=margin(5,0,0,0)),
  #axis.title.y = element_text(size=10, colour=line_color, family="sans"),
  #axis.title.x = element_blank(),
  #axis.title.y = element_blank(),
  #axis.text.x = element_text(size=12, colour="gray42", family="sans", angle=60, hjust=.8),
  axis.text.x = element_text(size=8, colour=line_color, family="sans"),
  axis.ticks = element_line(colour=line_color),
  #axis.title.y = element_text(size=12, colour="gray42", family="sans"),
  #axis.text.y = element_text(size=12, colour="gray42", family="sans"),
  #axis.ticks.y = element_blank(),
  axis.text.y = element_text(size=8, colour=line_color, family="sans"),
  panel.background = element_rect(colour="gray50", fill="white"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  #plot.title = element_blank(),
  plot.title = element_text(size=10, colour=line_color, family="sans"),
  plot.subtitle = element_text(size=10, colour=line_color, family="sans"),
  legend.position="right",
  legend.title = element_text(size=10, colour=line_color, family="sans",  face="bold"),
  legend.text  = element_text(size=10, colour=line_color, family="sans"),
  #strip.background = element_rect(color="gray90", fill="gray95"),
  strip.background = element_rect(
    color="gray50", fill=NA),
  strip.text.x=element_text(size=10, colour=line_color, family="sans", face="bold"),
  legend.key = element_rect(fill=NA)
)

### theme with axis titles/labels
sans_axes_theme_bigger <- theme(
  axis.title.y = element_text(size=11, colour=line_color, 
                              family="sans", face="bold"),
  axis.title.x = element_text(size=11, colour=line_color, 
                              family="sans", face="bold",
                              margin=margin(5,0,0,0)),
  #axis.title.y = element_text(size=10, colour=line_color, family="sans"),
  #axis.title.x = element_blank(),
  #axis.title.y = element_blank(),
  #axis.text.x = element_text(size=12, colour="gray42", family="sans", angle=60, hjust=.8),
  axis.text.x = element_text(size=11, colour=line_color, family="sans"),
  axis.ticks = element_line(colour=line_color),
  #axis.title.y = element_text(size=12, colour="gray42", family="sans"),
  #axis.text.y = element_text(size=12, colour="gray42", family="sans"),
  #axis.ticks.y = element_blank(),
  axis.text.y = element_text(size=10, colour=line_color, family="sans"),
  panel.background = element_rect(colour=line_color, fill="white"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  #plot.title = element_blank(),
  plot.title = element_text(size=12, colour=line_color, family="sans"),
  plot.subtitle = element_text(size=12, colour=line_color, family="sans"),
  legend.position="right",
  legend.title = element_text(size=12, colour=line_color, family="sans", face="bold"),
  legend.text  = element_text(size=12, colour=line_color, family="sans"),
  #strip.background = element_rect(color="gray90", fill="gray95"),
  strip.background = element_rect(
    color=line_color, fill=NA, linewidth = .75),
  strip.text.x=element_text(size=12, colour=line_color, family="sans", face="bold"),
  legend.key = element_rect(fill=NA)
)

options(digits = 2)
