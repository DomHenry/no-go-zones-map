draw_gt <- function(x){

  x %>%
    gt() %>%
    tab_header(
      title = md("Some relevant header here **testing**"),
      subtitle = "Some useful subtitle *here*"
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = cells_body(
        columns = vars(Species)
      )
    )

}


draw_gt_property <- function(x, prop_type){

  x %>%
    gt() %>%
    tab_header(
      title = md("Some relevant header here **testing**"),
      subtitle = "Some useful subtitle *here*"
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = cells_body(
        columns = vars({{prop_type}})
      )
    )

}

compile_species_table <- function(x){

  x %>%
    st_drop_geometry() %>%
    group_by(SENSFEA, THEME, SENSITI) %>%
    tally() %>%
    rename(Species = SENSFEA, 'EST Theme' = THEME,
           'EST sensistivity' = SENSITI, 'Polygon count' = n) %>%
    mutate(Class = str_split(Species, pattern = "-")[[1]][1],
           Species = str_split(Species, pattern = "-")[[1]][2]) %>%
    select(Species, Class, everything()) %>%
    ungroup()


}

compile_property_table <- function(x, prop_type){

  prop_df <- x %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    select(-GID) %>%
    select(PRCL_KEY, PRCL_TYPE, ID, PROVINCE, MAJ_REGION, MAJ_CODE, PARCEL_NO, PORTION) %>%
    mutate_all(as.character)

  prop_df %>%
    mutate(prop = str_c("property_", 1:nrow(.))) %>%
    pivot_longer(cols =-prop,
                 names_to = prop_type,
                 values_to = "value") %>%
    pivot_wider(names_from = prop,
                values_from = value)

}
