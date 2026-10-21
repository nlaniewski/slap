.datatable.aware = TRUE
##to silence NSE R CMD check notes; "no visible biding for global variable..."; data.table vars
# dput(unlist(strsplit(trimws(utils::readClipboard())," ")))
utils::globalVariables(
  c(".", "..cols.conserved", ".SD", ":=", "Alias", "Assay Type",
    "assay.type", "Buffer", "Buffer Alias", "Cell Type", "cell.count", "cell.type",
    "concentration", "concentration.live", "concentration.total",
    "count.type", "Date", "Date Time", "denominator", "href", "instrument", "location",
    "marker", "panel.id", "Reagent", "Reagent|Material", "Time", "titration",
    "titration.note", "user.id", "viability", "volume.mix.note", "volume.mix.uL")
)
