.datatable.aware = TRUE
#for NSE R CMD check; data.table vars
utils::globalVariables(
  c("."
    ,"..cols.conserved"
    ,".SD"
    ,":="
    ,"Assay Type"
    ,"assay.type"
    ,"cell.count"
    ,"Cell Type"
    ,"cell.type"
    ,"concentration"
    ,"concentration.live"
    ,"concentration.total"
    ,"count.type"
    ,"Date"
    ,"Date Time"
    ,"instrument"
    ,"Time"
    ,"viability"
  )
)
