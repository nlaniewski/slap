#' @title Parse a Cellometer Counts File
#'
#' @param counts.csv.path Character string; file path to a Cellometer counts file (.csv).
#' @param convert.date.time Logical; default `TRUE`; splits the 'Date Time' column and converts to class \link[data.table]{as.IDate} and \link[data.table]{as.ITime}.
#' @param return.assay.type Character string; default 'PBMC'; returns matching counts.  Other assay types might include: '1.6% fixed'.
#' @param make.wide Logical; default `TRUE`; make a wide-format (single-row per sample) data.table.
#'
#' @return a \link[data.table]{data.table} of cell counts
#' @note
#' As used in a streamlined workflow, the 'sample.name' column will have a 'batch order': a sequence of numbers 1:n number of samples; the batch order, along with 'date.time', should produce a unique subset for merging with other workflow metadata.
#' @export
#'
#' @examples
#'
#' counts.csv.path<-list.files(
#' system.file("extdata",package="slap"),
#' pattern="cellometer_counts.csv", full.names = TRUE
#' )
#'
#' cellometer.counts.parse(counts.csv.path,make.wide=FALSE)
#' counts<-cellometer.counts.parse(counts.csv.path,make.wide=TRUE)
#'
#' ##samples in a total volume of 2.2 mL
#' counts[,volume.ml := 2.2]
#'
#' ##total number of live cells
#' counts[,total.live:=volume.ml*concentration.live]
#'
#' ##extract batch order
#' counts[,batch.order:=sub("^.*_ON_","",sample.name)]
#'
#' ##merge with some metadata
#' mdat<-data.table::data.table(
#' subject.id = c(paste0("Subject",rep(1:6,each=3)),"Control1"),
#' visit.id = c(paste0("V",rep(1:3,times=6)),"Vctrl")
#' )
#' mdat[,sample.id:=paste(subject.id,visit.id,sep="_")]
#' mdat[,batch.order:=sprintf("%02d",seq(.N))]
#'
#' mdat[counts,on='batch.order']
cellometer.counts.parse<-function(counts.csv.path,convert.date.time=TRUE,return.assay.type="PBMC",make.wide=TRUE){
  ##cellometer counts file; .csv; read in as is but replace "" strings with NA
  counts<-utils::read.csv(counts.csv.path,strip.white = T,na.strings = "",check.names = F)
  ##index/drop NA lines; line breaks
  na.lines<-which(apply(counts,1,function(x) all(is.na(x))))
  counts<-counts[-na.lines,]
  ##drop 'User|Result Type|Result' columns
  counts<-counts[,-grep('User|Result Type|Result',colnames(counts))]
  ##back-fill 'Date.Time'
  date.time.lines<-which(!is.na(counts[['Date Time']]))
  for(i in date.time.lines){
    date.time<-counts[['Date Time']][i]
    while(is.na(counts[['Date Time']][i+1])){
      counts[['Date Time']][i+1]<-date.time
      i<-i+1
      if(i==nrow(counts)){
        break
      }
    }
  }
  ##convert to data.table
  counts<-data.table::setDT(counts)
  ##split by 'Date.Time
  counts.split<-split(counts,by='Date Time')
  ##back-fill
  counts.split<-lapply(counts.split,function(dt){
    back.fill<-names(which(sapply(dt,function(j) any(is.na(j)))))
    for(j in back.fill){
      val.unique<-unique(dt[[j]])
      val.unique<-val.unique[!is.na(val.unique)]
      if(length(val.unique)==1){
        data.table::set(dt,j=j,value = val.unique)
      }
    }
    ##back-fill 'Assay Type'
    dt[is.na(`Cell Type`),`Cell Type`:= `Assay Type`]
    return(dt)
  })
  ##rbind the list
  counts<-data.table::rbindlist(counts.split)
  ##function argument: convert.date.time
  ##convert from "%m/%d/%Y" to "%Y-%m-%d"
  if(convert.date.time){
    counts[,c('Date','Time'):=data.table::tstrsplit(`Date Time`," ")]
    counts[,Date:=data.table::as.IDate(Date,format="%m/%d/%Y")]
    counts[,Time:=data.table::as.ITime(Time)]
  }
  ##modify names
  data.table::setnames(counts,names(counts),gsub(" ",".",tolower(names(counts))))
  counts[grep("AO",cell.type),cell.type:="live_AO"]
  counts[grep("PI",cell.type),cell.type:="dead_PI"]
  counts[grep("AO|PI",cell.type,invert = T),cell.type:="total"]
  ##add attribute to data.table; 'instrument'
  .instrument<-counts[,unique(instrument)]
  if(length(.instrument)==1){
    counts[,instrument:=NULL]
    data.table::setattr(counts,"instrument",.instrument)
  }
  ##function argument: return.assay.type
  if(!is.null(return.assay.type)){
    if(make.wide&return.assay.type=="PBMC"){
      counts<-data.table::rbindlist(
        lapply(split(counts,by='date.time'),function(dt){
          cols.conserved<-names(which(dt[,sapply(.SD,function(j)length(unique(j)))]==1))
          dt<-cbind(
            unique(dt[,..cols.conserved]),
            data.table::data.table(
              N.total = dt[count.type=='Total',cell.count],
              N.live = dt[count.type=='F1',cell.count],
              N.dead = dt[count.type=='F2',cell.count],
              concentration.total = dt[count.type=='Total',concentration],
              concentration.live = dt[count.type=='F1',concentration],
              concentration.dead = dt[count.type=='F2',concentration]
            )
          )
          dt[,viability:=round(concentration.live/concentration.total*100,1)]
        })
      )
    }
    return(counts[assay.type==return.assay.type])
  }else{
    return(counts)
  }
}
