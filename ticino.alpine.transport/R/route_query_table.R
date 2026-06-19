#' @param station_data Data frame returned by read_station_data().
#' @param group_id_value Group ID to filter, for example 4.
#' @param query_date Selected query date.
#' @param query_times Selected query times.
#'
#' @return A data frame with route queries.
#' @export
route_query_table <- function(query_date = "2025-06-20",
                                    query_times = c("07:30", "09:00", "12:00", "17:00", "19:00")) {

  station_data <- read_csv_data()
  origin <- station_data[station_data$is_origin == TRUE, ]
  destinations <- station_data[station_data$is_origin == FALSE, ]
  
  query_table <- data.frame(
    group_id = destinations$group_id,
    region = destinations$region,
    from_city = origin$city,
    to_city = destinations$city,
    from_station_id = origin$station_id,
    to_station_id = destinations$station_id
  )
  
  query_table <- merge(
    query_table,
    data.frame(query_date = query_date, query_time = query_times)
  )
  
  return(query_table.tibble::as_tibble())
}


route_query_table()
  
