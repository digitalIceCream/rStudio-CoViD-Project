RKI_imptran <- function(x){
  read_csv(x, col_types = list(
    Altersgruppe = col_factor(),
    Geschlecht = col_factor(),
    IstErkrankungsbeginn = col_logical()
  )) %>%
    rename(geo_ID = IdLandkreis,age_group = Altersgruppe,
           sex = Geschlecht, rep_date = Meldedatum,
           ref_date = Refdatum, ref_rep_date = IstErkrankungsbeginn,
           amount_case = AnzahlFall, amount_death = AnzahlTodesfall,
           amount_recovery = AnzahlGenesen, new_case = NeuerFall,
           new_death = NeuerTodesfall, new_recovery = NeuGenesen)
}