#' Handling missing values
#'
#' @param data a data frame
#'
#' @return a data frame which had no missing value
#' @export
#'
#' @examples
#' na_process(data=iris)
na_process <- function(data){
  null <- c()
  for (i in 1:ncol(data)) {
    null <- c(null,sum(is.na(data[,i])))
  }
  null
  number <- which(null!=0)

  for (j in number) {
    if(is.numeric(data[,j])==TRUE){
      data[,j][is.na(data[,j])] <- mean(data[,j],na.rm=TRUE)
    }else{
      data[,j][is.na(data[,j])] <- names(sort(table(na.omit(data[,j])),
                                              decreasing = T)[1])
    }
  }
  data
}

#' Outlier processing for data without missing values
#'
#' @param data a data frame with no missing value, which consist of character or numerical variables
#'
#' @return a data frame without outlier, especially for numeric column
#' @export
#'
#' @examples iris$Species <- as.character(iris$Species); outlier_process(data=iris)
outlier_process <- function(data){

  num <- data[,which(!sapply(data, is.character))]
  char <- data[,which(sapply(data, is.character))]

  outlier <- function(x,time=1.5){
    out.lower <- quantile(x,probs=0.25)-IQR(x)*time
    out.higher <- quantile(x,probs=0.75)+IQR(x)*time
    x[x>out.higher | x<out.lower] <- NA
    x
  }

  for (i in 1:ncol(num)) {
    num[,i] <- num[,i] %>%
      map_dbl(~outlier(.x))
  }

  data <- cbind(num,char)
}

#' Box plot of numeric variables
#'
#' @param num a data frame consist of numeric variables
#' @param group A factor with duplicated character or factor, which must correspond to the obs in num
#' @param width width of boxplot and error bar
#' @param xlab title of the x axis
#' @param ylab title of the y axis
#' @param grouplab title of the group legend
#' @param color color vector
#'
#' @return a boxplot according to \code{num} and grouped by \code{group}.
#' @export
#'
#' @examples
#'num_plot(num=iris[,1:3],group=iris[,4])
num_plot <- function(num,
                     group,
                     width=0.5,
                     xlab = "variable",
                     ylab = "",
                     grouplab = "Group",
                     color = c("#2874C5","#f87669","#e6b707","#868686","#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
                                        "#FFD92F", "#E5C494", "#B3B3B3")){
  if(!requireNamespace("ggplot2",quietly = TRUE)){
    stop("ggplot2 for the this function to work. Please install it",
         call. = FALSE)
}

  if(!requireNamespace("plyr",quietly = TRUE)){
    stop("plyr for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("purrr",quietly = TRUE)){
    stop("purrr for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("readxl",quietly = TRUE)){
    stop("readxl for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("tibble",quietly = TRUE)){
    stop("tibble for the this function to work. Please install it",
         call. = FALSE)
  }

  res <- apply(num, 2, function(x){
    ks.test(x,"pnorm")
  })

  tmp_norm <- data.frame(group = group)
  tmp_nonorm <- data.frame(group = group)
  for (i in 1:length(res)) {
    if(res[[i]]$p.value>0.05){
      tmp_norm <- cbind(tmp_norm,num[,i])
      colnames(tmp_norm)[i+1] <- names(res[i])
    }else{
      tmp_nonorm <- cbind(tmp_nonorm,num[,i])
      colnames(tmp_nonorm)[i+1] <- names(res[i])
    }

  }

  rownames(tmp_norm) <- paste0("sample","_",1:nrow(tmp_norm))
  rownames(tmp_nonorm) <- paste0("sample","_",1:nrow(tmp_nonorm))

  tmp_norm = rownames_to_column(tmp_norm,
                                var = "sample")
  tmp_norm = tidyr::gather(tmp_norm,"rows","value",
                           -sample,-group)

  tmp_nonorm = rownames_to_column(tmp_nonorm,
                                  var = "sample")
  tmp_nonorm = tidyr::gather(tmp_nonorm,"rows","value",
                             -sample,-group)

  col = color[1:length(levels(group))]

  if("rows" %in% colnames(tmp_norm)){
    p_norm <- ggplot(tmp_norm,
                     aes(rows,value,fill = group))+
      geom_boxplot(width = width)+
      theme_bw()+
      theme(legend.position = "top")+
      labs(fill = grouplab,
           x = xlab,
           y = ylab)+
      scale_fill_manual(values = col)

    if(length(unique(group))==2){
      p_norm = p_norm +
        ggpubr::stat_compare_means(aes(group = group,
                                       label = ggplot2::after_stat(p.signif)),
                                   method = "t.test")

    }else if(length(unique(group))>2){
      p_norm = p_norm +
        ggpubr::stat_compare_means(aes(group = group,
                                       label = ggplot2::after_stat(p.signif)),
                                   method = "aov")
    }
  }else{
    message("The columns of the data need to contain 'rows'.")
  }

  if("rows" %in% colnames(tmp_nonorm)){
    p_nonorm <- ggplot(tmp_nonorm,
                       aes(rows,value,fill = group))+
      geom_boxplot( width = width)+
      theme_bw()+
      theme(legend.position = "top")+
      labs(fill = grouplab,
           x = xlab,
           y = ylab)+
      scale_fill_manual(values = col)

    if(length(unique(group))==2){
      p_nonorm = p_nonorm +
        ggpubr::stat_compare_means(aes(group = group,
                                       label = ggplot2::after_stat(p.signif)),
                                   method = "wilcox.test")

    }else if(length(unique(group))>2){
      p_nonorm = p_nonorm +
        ggpubr::stat_compare_means(aes(group = group,
                                       label = ggplot2::after_stat(p.signif)),
                                   method = "kruskal.test")
    }
  }else{
    message("The columns of the data need to contain 'rows'.")
  }

  if(exists("p_norm") & exists("p_nonorm")){
    p <- wrap_plots(p_norm,p_nonorm,
                    nrow = 2,heights = c(1,11))
  }else if(exists("p_norm") & !exists("p_nonorm")){
    p <- p_norm
  }else{
    p <- p_nonorm
  }

  return(p)
}

#' Bar plot of categorical variables
#'
#' @param vec a data frame consist of categorical variables
#' @param group A factor with duplicated character or factor, which must correspond to the obs in vec
#' @param color color vector
#'
#' @return a barplot according to \code{vec} and grouped by \code{group}.
#' @export
#'
#' @examples
#'vec_plot(vec=mtcars[,1:10],group=mtcars[,10])
vec_plot <- function(vec,group,
                     color = c("#2874C5","#f87669","#e6b707","#868686",
                               "#66C2A5","#FC8D62","#8DA0CB","#E78AC3",
                               "#A6D854","#FFD92F","#E5C494","#B3B3B3")){

  if(!requireNamespace("ggplot2",quietly = TRUE)){
    stop("ggplot2 for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("plyr",quietly = TRUE)){
    stop("plyr for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("purrr",quietly = TRUE)){
    stop("purrr for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("patchwork",quietly = TRUE)){
    stop("patchwork for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("tibble",quietly = TRUE)){
    stop("tibble for the this function to work. Please install it",
         call. = FALSE)
  }

        vec$group <- group
        p <- list()
        for (i in 1:(ncol(vec)-1)) {
        a <- data.frame(table(vec[,i],vec[,ncol(vec)]))
        b <- table(vec[,i],vec[,ncol(vec)])
        a <- plyr::ddply(a,.(Var2),transform,percent=Freq/sum(Freq)*100)
        a$label = paste0(sprintf("%.1f", a$percent), "%")

        pvalue <- chisq.test(b)$p.value
        xsquared <- chisq.test(b)$statistic

p[[i]] <- ggplot(a,aes(Var2,percent,fill=Var1))+
      geom_bar(stat="identity",position = position_stack())+
      scale_fill_manual(values = color[1:length(levels(a$Var1))])+
      scale_y_continuous(labels = scales::percent_format(scale = 1))+ #百分比y轴
      labs(x="Number of transfusion",
      y="Percent Weidght",
      fill=colnames(vec)[i])+
      annotate(geom = "text",
     cex=5,
     x=1.5, y=105,
    label=paste0("P ", ifelse(pvalue<2.2e-16, "<2.2e-16", paste0("= ",round(pvalue,4))),
    'x-squared =',paste0(round(xsquared,1))),
    color="black")+
    theme_classic()+
    theme(legend.position = "top",
    legend.text = element_text(size=12),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12))
   }

   if(ncol(vec)<=8){
   p <- patchwork::wrap_plots(p,ncol = 4)
   ggsave(p,device = "png",
   units = "cm",
   width = 16,
   dpi = 600,
   file="./test.png")
   }else if(ncol(vec)>=9 &ncol(vec)<=25){

   p <- patchwork::wrap_plots(p,ncol = 5)
   ggsave(p,device = "png",
   units = "cm",
   width = 100,
   height = 100,
   dpi = 600,
   file="./test.png")
      }else{

   p <- patchwork::wrap_plots(p,ncol = 6)
   ggsave(p,device = "png",
   units = "cm",
   width = 150,
   height = 150,
   dpi = 600,
   file="./test.png")
 }

}



