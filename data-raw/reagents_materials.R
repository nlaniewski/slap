reagents_materials<-rbind(
  c(
    `Reagent|Material` ="FBS",
    Manufacturer = "Avantor Seradigm",
    Catalog = "97068-085",
    href = "https://export.vwr.com/store/product?keyword=97068-085",
    Note = "Fetal Bovine Serum; heat-inactivated; previously aliquoted"
  ),
  c(
    `Reagent|Material` ="PBS",
    Manufacturer = "Gibco",
    Catalog = "10010023",
    href = "https://www.thermofisher.com/order/catalog/product/10010023",
    Note = "Phosphate Buffered Saline"
  ),
  c(
    `Reagent|Material` ="Universal Nuclease",
    Manufacturer = "Pierce",
    Catalog = "88702",
    href = "https://www.thermofisher.com/order/catalog/product/88702",
    Note = ""
  ),
  c(
    `Reagent|Material` ="UltraPure Distilled Water",
    Manufacturer = "Invitrogen",
    Catalog = "10977015",
    href = "https://www.thermofisher.com/order/catalog/product/10977015",
    Note = ""
  ),
  c(
    `Reagent|Material` ="BD Cytofix/Cytoperm",
    Manufacturer = "BD Biosciences",
    Catalog = "554722",
    href = "https://www.bdbiosciences.com/en-us/products/reagents/flow-cytometry-reagents/research-reagents/single-color-antibodies-ruo/fixation-and-permeabilization-solution.554722?tab=product_details",
    Note = ""
  ),
  c(
    `Reagent|Material` ="BD Perm/Wash Buffer",
    Manufacturer = "BD Biosciences",
    Catalog = "554723",
    href = "https://www.bdbiosciences.com/en-us/products/reagents/cell-preparation-separation-reagents/blood-lysis/staining-and-cell-preparation/perm-wash-buffer.554723?tab=product_details",
    Note = "10X solution; diluted 1:10 using UltraPure Distilled Water"
  ),
  c(
    `Reagent|Material` ="eBioscience Fixation/Permeabilization Concentrate",
    Manufacturer = "eBioscience",
    Catalog = "00-5123-43",
    href = "https://www.thermofisher.com/order/catalog/product/00-5123-43",
    Note = "4x solution; diluted 1 part to 3 parts using eBioscience Fixation/Permeabilization Diluent"
  ),
  c(
    `Reagent|Material` ="eBioscience Fixation/Permeabilization Diluent",
    Manufacturer = "eBioscience",
    Catalog = "00-5223-56",
    href = "https://www.thermofisher.com/order/catalog/product/00-5223-56",
    Note = "Used in combination with eBioscience Fixation/Permeabilization Concentrate"
  ),
  c(
    `Reagent|Material` ="eBioscience Permeabilization Buffer (10x)",
    Manufacturer = "eBioscience",
    Catalog = "00-8333-56",
    href = "https://www.thermofisher.com/order/catalog/product/00-8333-56",
    Note = "10X solution; diluted 1:10 using UltraPure Distilled Water"
  ),
  c(
    `Reagent|Material` ="RPMI 1640",
    Manufacturer = "Corning",
    Catalog = "10-040-CV",
    href = "https://ecatalog.corning.com/life-sciences/b2c/US/en/Media%2C-Sera%2C-and-Reagents/Classical-Media/RPMI-1640/Corning%C2%AE-RPMI-1640/p/10-040-CV",
    Note = ""
  ),
  c(
    `Reagent|Material` ="Ultrafree-MC Centrifugal Filter",
    Manufacturer = "Millipore",
    Catalog = "UFC30VV",
    href = "https://www.sigmaaldrich.com/US/en/product/mm/ufc30vv",
    Note = "Pore size 0.1 uM, PVDF membrane; for filtering antibody mixes"
  ),
  c(
    `Reagent|Material` ="BD Horizon Brilliant Stain Buffer Plus",
    Manufacturer = "BD Biosciences",
    Catalog = "566385",
    href = "https://www.bdbiosciences.com/en-us/products/reagents/flow-cytometry-reagents/research-reagents/buffers-and-supporting-reagents-ruo/brilliant-stain-buffer-plus.566385?tab=product_details",
    Note = ""
  )
)
##
# c(
#   `Reagent|Material` ="",
#   Manufacturer = "",
#   Catalog = "",
#   href = "",
#   Note = ""
# )
##
reagents_materials<-data.table::data.table(reagents_materials)
##
