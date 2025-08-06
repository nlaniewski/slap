get.buffers<-function(permabilization.type=c('BD','eBio')){
  staining.buffers.alias <- c(
    c("SB","SBn","VB"),
    switch(
      match.arg(permabilization.type),
      BD = c('CF/CP','bdPB'),
      eBio = c('F/P','ePB')
    )
  )
  buffers.table <- kableExtra::kable(
    x = buffers[`Buffer Alias` %in% staining.buffers.alias],
    format = "html",
    escape = FALSE
  )
  buffers.table <- kableExtra::kable_styling(
    kable_input = buffers.table,
    bootstrap_options = c("hover", "condensed", "striped")
  )
  return(buffers.table)
}

get.reagents<-function(reagents.c=NULL,reagents.pattern=NULL,...){
  reagents.table<-data.table::copy(reagents)
  reagents.table<-(
    reagents.table[
      if(!is.null(reagents.c)){
        Reagent %in% reagents.c
      }else if(!is.null(reagents.pattern)){
        grep(reagents.pattern,Reagent,...)
      }
    ]
    [,Reagent := kableExtra::cell_spec(Reagent,"html",link=href)]
    [,href:=NULL]
  )
  reagents.table <- kableExtra::kable(
    x = reagents.table,
    format = "html",
    escape = FALSE
  )
  reagents.table <- kableExtra::kable_styling(
    kable_input = reagents.table,
    bootstrap_options = c("hover", "condensed", "striped")
  )
  return(reagents.table)
}
