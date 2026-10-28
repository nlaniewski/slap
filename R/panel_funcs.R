get.panel.from.master <- function(
    panel.master.path,
    study.id = NULL,
    panel.id = NULL,
    drop.unused.detectors = TRUE,
    drop.unspecified.location = TRUE
)
{
  ##
  file.extension<-sub("^.*\\.","",panel.master.path)
  if(!file.extension %in% c('csv','xlsx')){
    stop("Need a panel.master with a file extension of .csv or .xlsx")
  }
  func.call <- "panel.master.path |> openxlsx::read.xlsx() |> data.table::data.table()"
  if(file.extension %in% 'csv'){
    func.call<-sub("|> openxlsx::read.xlsx() ","",func.call, fixed = TRUE)
  }
  panel<-eval(parse(text=func.call))
  ##
  if(!is.null(study.id)){
    panel<-panel[study.id %in% study.id]
  }
  if(!is.null(panel.id)){
    panel<-panel[panel.id %in% panel.id]
  }
  ##
  if(drop.unused.detectors){
    panel<-panel[!is.na(marker)]
  }
  if(drop.unspecified.location){
    panel <- panel[!is.na(location)]
  }
  panel[]
}
panel.tables <- function(
    panel,
    test.number = 10,
    stain.volume = 50
)
{
  ##
  if(panel[,!is.numeric(volume.test)]){
    stop("Volume-based mix calculations are dependent on numerical values stored in the 'volume.test' column of the panel; check the panel master file and confirm that this column is encoded as numeric.")
  }
  ##
  panel[
    ,
    location := factor(
      location,
      levels = c('viability', 'surface', 'intracellular')
    )
  ]
  ##
  panels.mix <- data.table::rbindlist(
    lapply(split(panel, by = 'location', drop = T), function(
    panel.split,
    cols.keep = c(
      'panel.id', 'location', 'marker', 'fluor', 'volume.mix.uL', 'volume.mix.note'
    ),
    total.volume = test.number * stain.volume
    )
    {
      if(unique(panel.split[['location']]) == 'viability'){
        ## viability dyes are aliquoted at 2 uL (Scheible Lab)
        panel.split[
          ,
          j = c('volume.mix.uL', 'volume.mix.note') := {
            vol.dye <- ceiling(volume.test * test.number)
            total.buffer <- (stain.volume * vol.dye) / volume.test
            ##
            list(vol.dye, sprintf("+ %d uL VB", total.buffer))
          }
        ]
      }else{
        # panel.split[
        #   grep("test/total volume dilution", titration.note),
        #   volume.mix.uL := titration * total.volume
        # ]

        panel.split[
          i = dilution.factor > test.number,
          volume.mix.note := sprintf("of a 1:%d", dilution.factor)
        ]

        panel.split[
          i = !is.na(volume.mix.note),
          j = volume.mix.uL := test.number
        ]
      }

      panel.split[
        i = is.na(volume.mix.uL),
        j = volume.mix.uL := volume.test * test.number
      ]
      panel.split[
        i = is.na(volume.mix.note),
        j = volume.mix.note := 'neat'
      ]

      panel.split <- panel.split[, .SD, .SDcols = cols.keep]

      # panel.split[, volume.mix.uL := round(volume.mix.uL, 1)]

      droplevels(panel.split)[]
    }
    ),
    fill = TRUE
  )
  ##
  if("volume.mix.note" %in% names(panels.mix)){
    locations <- panels.mix[!is.na(volume.mix.note), location]
    for(.location in locations){
      panels.mix[
        i = location == .location & is.na(volume.mix.note),
        j = volume.mix.note := ""
      ]
    }
  }
  ##
  panels.buffer <- data.table::rbindlist(
    lapply(
      split(panels.mix, by = c('panel.id', 'location'), drop = T),
      function(
    panel.split,
    total.volume=(test.number*stain.volume),
    bsb.volume=test.number*10
      )
      {
        ##
        vols<-unique(panel.split[,.(panel.id,location)])
        ##
        vols<-if(vols$location == 'viability'){
          total.volume <- panel.split[
            ,
            as.numeric(gsub("\\D", "", volume.mix.note))
          ]
          cbind(
            vols,
            data.table::data.table(
              volume.uL = total.volume,
              buffer.type = "VB"
            )
          )
        }else if(vols$location == 'surface'){
          cbind(
            vols,
            data.table::data.table(
              volume.uL = c(
                total.volume-(panel.split[,sum(volume.mix.uL)])-bsb.volume,
                bsb.volume
              ),
              buffer.type = c("SBn","BSB+")
            )
          )
        }else if(vols$location == 'intracellular'){
          cbind(
            vols,
            data.table::data.table(
              volume.uL = c(
                total.volume-(panel.split[,sum(volume.mix.uL)])-bsb.volume,
                bsb.volume
              ),
              buffer.type = c("ePB","BSB+")
            )
          )
        }
      }
    )
  )
  list(
    mix=panels.mix,
    buffer=panels.buffer
  )
}
panel.kables<-function(panel.tables){
  ##
  kable.func<-function(dt){
    ## base HTML table
    tbl <- kableExtra::kbl(dt)
    ## themed table with bootstrap options
    # tbl <- kableExtra::kable_classic_2(tbl, full_width = FALSE, lightable_options = "striped")
    ## footnote
    tbl <- kableExtra::footnote(
      tbl,
      general_title = "Volume(s) uL:",
      general = "Add the indicated volume(s) to a Protein LoBind 1.5 mL tube",
      footnote_as_chunk = TRUE
    )
    ## bootstrap options
    # tbl <- kableExtra::kable_styling(
    #   tbl,
    #   bootstrap_options = c("striped", "hover", "condensed"),
    #   full_width = FALSE
    # )
    tbl <- kableExtra::kable_styling(
      tbl,
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center",
      html_font = "Calibri"
    )
  }
  ##
  mixes <- lapply(
    split(panel.tables$mix, by = c('panel.id', 'location')), function(mix){
      if(
        unique(mix[,location]) == 'viability' |
        mix[, all(is.na(volume.mix.note))]
      ) mix[, volume.mix.note := NULL]

      kable.func(mix)
    }
  )
  ##
  buffers<-lapply(split(panel.tables$buffer,by=c('panel.id','location')),function(buffer){
    # drop.cols<-names(which(sapply(mix,function(j) all(is.na(j)))))
    kable.func(buffer)
  })
  ##
  list(
    mix=mixes,
    buffer=buffers
  )
}
