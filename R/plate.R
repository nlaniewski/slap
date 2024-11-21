#' @title Generate a box/plate based layout
#'
#' @description
#' Generate a box/plate based layout for use with organizing sample storage (cryobox) or experimental culture plates (96-well).
#'
#'
#' @param rows numeric; the number of rows
#' @param cols numeric; the number of columns
#'
#' @return a `data.frame` representing a box/plate based layout with both 'alphanumeric' and 'row_col' labels.
#' @export
#'
#' @examples
#' plate.layout(9,9)
#' plate.layout(8,12)
#'
plate.layout<-function(rows,cols){
  n<-rows*cols
  plate<-data.frame(
    row=rep(seq(rows),each=n/rows),
    col=rep(seq(cols),times=n/cols),
    value=0
  )
  plate$alpahnumeric<-paste0(LETTERS[plate$row],plate$col)
  plate$row_col<-paste(plate$row,plate$col,sep="_")
  return(plate)
}
#' @title Plot a box/plate based layout
#'
#' @description
#' Plot a basic box/plate based layout; if metadata is added to a \link{plate.layout}, those additional columns can be used as additional plot aesthetics.
#'
#'
#' @param plate A `data.frame` as returned from \link{plate.layout}.
#' @param row.label Character string; row-labeling style:
#' * 'alpha' (default) for letter-based row-labeling; 96-well plate style.
#' * 'numeric' for number-based row-labeling
#'
#' @return a \link[ggplot2]{ggplot2} object
#' @export
#'
#' @examples
#' ##a standard 9x9 storage box
#' plate<-plate.layout(9,9)
#' plate
#'
#' ##skip every third spot
#' skip<-c(3,6,9)
#' for(i in seq((9-1))){
#' skip<-c(skip,c(3,6,9)+(i*9))
#' }
#'
#' ##'subject.id'
#' plate$subject.id<-NA
#' for(i in seq(skip)){
#' plate.index<-c(skip[i]-2,skip[i]-1)
#' plate$subject.id[plate.index]<-"Subject"
#' }
#' subject.ids<-paste0(na.omit(plate)[['subject.id']],na.omit(plate)[['row']])
#' plate[!is.na(plate$subject.id),'subject.id']<-subject.ids
#'
#' ##'visit.id'
#' plate$visit.id<-NA
#' for(i in unique(na.omit(plate$subject.id))){
#' visit.index<-grep(i,plate$subject.id)
#' plate[visit.index[1:2],'visit.id']<-"V1"
#' plate[visit.index[3:4],'visit.id']<-"V2"
#' plate[visit.index[5:6],'visit.id']<-"V3"
#' }
#'
#' plate
#'
#' ##basic plot; empty box
#' plate.plot(plate,row.label = "n")
#'
#' ##plot with added geom using plate metadata
#' plate.plot(plate,row.label = "n") +
#' ggplot2::geom_point(
#' data=na.omit(plate),
#' mapping=ggplot2::aes(
#' col,
#' row,
#' color=subject.id,
#' shape=visit.id),
#' size=10)
#'
plate.plot <- function(plate,row.label=c('alpha','numeric')){
  ##
  .cols<-seq(max(plate$col))
  .rows<-seq(max(plate$row))
  ##
  row.label<-match.arg(row.label)
  if(row.label=='alpha'){
    .row.label<-LETTERS[.rows]
  }
  if(row.label=='numeric'){
    .row.label<-.rows
  }
  ##
  p<-ggplot2::ggplot(
  ) +
    ggforce::geom_circle(
      data = plate,
      mapping = ggplot2::aes(x0=col,y0=row,r = 0.45)
    ) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(
      breaks = .cols,
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      breaks = .rows,
      labels = .row.label,
      expand = ggplot2::expansion(mult = c(0.02, 0.02)),
      trans = scales::reverse_trans()
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = NULL
    )
  ##
  return(p)
}
