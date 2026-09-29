is_drivable_highway <- function(x) {
  excluded <- c(
    "footway", "cycleway", "pedestrian", "steps", "bridleway", "path",
    "corridor", "platform", "construction", "proposed", "raceway"
  )
  !is.na(x) & nzchar(trimws(x)) & !(tolower(trimws(x)) %in% excluded)
}

road_class_priority <- function(x) {
  levels <- c(
    motorway = 1L, trunk = 2L, primary = 3L, secondary = 4L,
    tertiary = 5L, unclassified = 6L, residential = 7L,
    living_street = 8L, service = 9L, track = 10L
  )
  answer <- unname(levels[tolower(x)])
  answer[is.na(answer)] <- 20L
  answer
}

normalise_corridor_text <- function(x) {
  x <- toupper(trimws(ifelse(is.na(x), "", x)))
  gsub("[^[:alnum:]]+", "", x)
}

match_accidents_to_roads <- function(accidents, roads,
                                     high_confidence_m = 30,
                                     maximum_m = 50,
                                     ambiguity_tolerance_m = 5) {
  if (!inherits(accidents, "sf") || !inherits(roads, "sf")) {
    stop("accidents and roads must be sf objects")
  }
  if (sf::st_crs(accidents) != sf::st_crs(roads)) stop("accidents and roads must use the same CRS")
  if (!all(c("osm_id", "highway") %in% names(roads))) stop("roads require osm_id and highway fields")

  usable <- roads[is_drivable_highway(roads$highway), , drop = FALSE]
  if (!nrow(usable)) stop("No drivable roads were supplied")
  distance_matrix <- units::drop_units(sf::st_distance(accidents, usable))
  if (is.null(dim(distance_matrix))) distance_matrix <- matrix(distance_matrix, nrow = nrow(accidents))

  selected <- vector("list", nrow(accidents))
  for (i in seq_len(nrow(accidents))) {
    distances <- distance_matrix[i, ]
    minimum <- min(distances)
    candidates <- which(distances <= minimum + ambiguity_tolerance_m)
    # Only candidates inside the explicit ambiguity tolerance compete on road
    # class; a road outside that band cannot override the materially nearer way.
    chosen <- candidates[order(
      road_class_priority(usable$highway[candidates]),
      distances[candidates],
      as.character(usable$osm_id[candidates])
    )][1]
    ambiguous <- length(candidates) > 1L
    within_limit <- minimum <= maximum_m

    row <- sf::st_drop_geometry(accidents[i, , drop = FALSE])
    road_values <- sf::st_drop_geometry(usable[chosen, , drop = FALSE])
    road_values[] <- lapply(road_values, function(x) if (within_limit) x else x[NA_integer_])
    selected[[i]] <- cbind(
      row,
      road_values,
      match_distance_m = minimum,
      match_confidence = if (!within_limit) "Unmatched" else if (minimum <= high_confidence_m && !ambiguous) "High" else "Lower",
      ambiguity_reason = if (!within_limit) "No drivable road within maximum distance" else if (ambiguous) "Competing road within ambiguity tolerance" else NA_character_,
      stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(selected)
}

summarise_road_burden <- function(matches, roads) {
  matched <- matches[!is.na(matches$osm_id), , drop = FALSE]
  if (!nrow(matched)) return(data.frame())

  road_attributes <- sf::st_drop_geometry(roads)
  # OSM extracts may omit some naming fields. Keep the burden summary
  # structurally stable by creating missing optional fields as NA.
  for (field in c("ref", "name", "name_bn")) {
    if (!field %in% names(road_attributes)) road_attributes[[field]] <- NA_character_
  }
  if (!"length_km" %in% names(road_attributes)) {
    road_attributes$length_km <- as.numeric(sf::st_length(roads)) / 1000
  }
  road_attributes <- road_attributes |>
    dplyr::mutate(
      corridor_id = dplyr::case_when(
        !is.na(ref) & nzchar(trimws(ref)) ~ paste0("ref:", normalise_corridor_text(ref)),
        !is.na(name) & nzchar(trimws(name)) ~ paste0("name:", normalise_corridor_text(name)),
        TRUE ~ paste0("osm:", osm_id)
      )
    )

  event_counts <- matched |>
    dplyr::left_join(road_attributes[, c("osm_id", "corridor_id")], by = "osm_id") |>
    dplyr::group_by(corridor_id) |>
    dplyr::summarise(
      recorded_accidents = dplyr::n(),
      high_confidence_accidents = sum(match_confidence == "High"),
      lower_confidence_accidents = sum(match_confidence == "Lower"),
      .groups = "drop"
    )

  corridor_attributes <- road_attributes |>
    dplyr::group_by(corridor_id) |>
    dplyr::summarise(
      road_label = dplyr::first(stats::na.omit(c(ref, name, name_bn, osm_id))),
      ref = dplyr::first(ref),
      name = dplyr::first(name),
      highway = paste(sort(unique(highway)), collapse = "; "),
      corridor_length_km = sum(length_km, na.rm = TRUE),
      osm_ids = paste(sort(unique(osm_id)), collapse = "; "),
      .groups = "drop"
    )

  event_counts |>
    dplyr::left_join(corridor_attributes, by = "corridor_id") |>
    dplyr::mutate(
      share_of_matched_pct = 100 * recorded_accidents / sum(recorded_accidents),
      accidents_per_km = dplyr::if_else(corridor_length_km > 0, recorded_accidents / corridor_length_km, NA_real_)
    ) |>
    dplyr::arrange(dplyr::desc(recorded_accidents), dplyr::desc(accidents_per_km), road_label)
}
