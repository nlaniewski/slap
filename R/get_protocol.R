#' @title Generate a printable (.html) protocol for flow cytometery staining
#'
#' @param panel.master.path file path to a conformant panel master (.xlsx).
#' @param fix.type Character string -- default `'eBio'`; fixation type used for intracellular/intranuclear staining.
#' @param test.number Numeric; the number of individual samples (plate wells) to be stained
#' @param overage Numeric -- default `.2`; percent overage as decimal.
#' @param stain.volume Numeric -- default `50`; total uL (microliter) stain volume per well.
#' @param browse Logical -- default `TRUE`; opens the .html document in a browser window for printing.
#'
#' @returns Opens a .html (.Rmd) document using the default browser.
#' @export
#'
#' @examples
#' panel.master.path <- system.file("extdata/panels_CD4pheno.xlsx",package = 'slap')
get.protocol<-function(
    panel.master.path,
    fix.type=c('eBio','BD'),
    test.number,
    overage = .2,
    stain.volume=50,
    browse = TRUE
)
{
  ##
  panel<-get.panel.from.master(panel.master.path)
  ##
  protocol.path<-list.files(
    system.file("protocols",package="slap"),
    pattern="staining",
    full.names = TRUE
  )
  protocol.hash.body<-tools::md5sum(protocol.path)
  protocol.hash.panel<-digest::digest(panel[,sapply(.SD,digest::digest)])
  hashes<-lapply(list(body=protocol.hash.body,panel=protocol.hash.panel),function(i){
    paste(substr(i,1,4),substr(i,nchar(i)-3,nchar(i)),sep="...")
  })
  hashes.fn<-sapply(hashes,sub,pattern="\\...",replacement="")
  ##
  title.vec<-panel[,sapply(.SD,unique),.SDcols = c('study.id','panel.id')]
  title<-paste(
    paste(paste0("**",title.vec[['study.id']],"**"),"Study Protocol:"),
    "Staining for Full Spectrum Cytometry",
    paste("Panel:",paste0("**",title.vec[['panel.id']],"**")),
    sep = "<br>"
  )
  author<-panel[,unique(user.id)]
  ##
  test.number <- test.number + test.number * overage
  .panel.tables<-panel.tables(panel,test.number,stain.volume)
  .panel.kables<-panel.kables(.panel.tables)
  ##
  .buffers <- get.buffers(permabilization.type = fix.type)
  .reagents <- get.reagents(reagents.pattern = "FBS|PBS|Universal Nuclease|UltraPure Distilled Water|eBio|Filter|Brilliant Stain Buffer Plus")
  ##
  fn<-paste(
    title.vec[['study.id']],
    'staining',
    title.vec[['panel.id']],
    paste0("BODY",hashes.fn[['body']]),
    paste0("PANEL",hashes.fn[['panel']]),
    sep = "_")
  ##
  rmarkdown::render(
    input = protocol.path,
    params = list(
      protocol_title = title,
      protocol_author = author,
      protocol.hash.body = hashes$body,
      protocol.hash.panel = hashes$panel,
      reagents = .reagents,
      buffers = .buffers,
      panel.buffers = .panel.kables$buffer,
      panel.mixes = .panel.kables$mix,
      tests.number = test.number
    ),
    output_dir = tempdir(),
    output_file = fn
  )
  ##
  if(browse){
    utils::browseURL(file.path(tempdir(),paste0(fn,'.html')))
  }
}
