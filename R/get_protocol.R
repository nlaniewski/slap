get.protocol<-function(panel.master.path,fix.type='eBio',test.number,stain.volume=50,browse = TRUE){
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
  .panel.tables<-panel.tables(panel,test.number,stain.volume)
  .panel.kables<-panel.kables(.panel.tables)
  ##
  .buffers <- get.buffers('eBio')
  .reagents <- get.reagents(reagents.pattern = "FBS|PBS|Universal Nuclease|UltraPure Distilled Water|eBio")
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
