buffers<-rbind(
  c(
    Buffer ="Staining Buffer",
    'Buffer Alias' = "SB",
    Constituents = "1x PBS + 2% FBS (490 mL PBS + 10 mL FBS); 0.2 um filter sterilized"
  ),
  c(
    Buffer ="Staining Buffer + Universal Nuclease",
    'Buffer Alias' = "SBn",
    Constituents = "SB + 1:10,000 Universal Nuclease"
  ),
  c(
    Buffer ="Viability Buffer",
    'Buffer Alias' = "VB",
    Constituents = "1 part SBn + 3 parts 1x PBS; 0.5% FBS final (0.8 mL SBn + 2.4 mL 1x PBS)"
  ),
  c(
    Buffer ="BD Cytofix/CytoPerm",
    'Buffer Alias' = "CF/CP",
    Constituents = "BD Cytofix/CytoPerm; use as provided (1x solution)"
  ),
  c(
    Buffer ="eBio Fix/Perm",
    'Buffer Alias' = "F/P",
    Constituents = "1 part eBIO Fix/Perm Concentrate + 3 parts eBIO Fix/Perm Diluent"
  ),
  c(
    Buffer ="BD Permeabilization Buffer (1x)",
    'Buffer Alias' = "bdPB",
    Constituents = "10x BD Permeabilization Buffer diluted to 1x in UltraPure Distilled Water"
  ),
  c(
    Buffer ="eBio Permeabilization Buffer (1x)",
    'Buffer Alias' = "ePB",
    Constituents = "10x eBIO Permeabilization Buffer diluted to 1x in UltraPure Distilled Water"
  ),
  c(
    Buffer ="RPMI 1640 + FBS + antibiotics",
    'Buffer Alias' = "R10",
    Constituents = "RPMI 1640 + 10% FBS + 1x Penicillin-Streptomycin (445 mL + 50 mL + 5 mL); 0.2 um filter sterilized"
  ),
  c(
    Buffer ="BD Horizon Brilliant Stain Buffer Plus",
    'Buffer Alias' = "BSB+",
    Constituents = "use as provided."
  )
)
##
# c(
#   Buffer ="",
#   'Buffer Alias' = "",
#   Constituents = ""
# )
##
buffers<-data.table::data.table(buffers)
##
