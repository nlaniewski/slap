get.buffers<-function(permabilization.type=c('BD','eBio')){
  staining.buffers.alias <- c(
    c("SB","SBn","VB","BSB+"),
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
  reagents.table<-data.table::copy(reagents_materials)
  reagents.table<-(
    reagents.table[
      if(!is.null(reagents.c)){
        `Reagent|Material` %in% reagents.c
      }else if(!is.null(reagents.pattern)){
        grep(reagents.pattern,`Reagent|Material`,...)
      }
    ]
    [,`Reagent|Material` := kableExtra::cell_spec(`Reagent|Material`,"html",link=href)]
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
