#' @title Compute waiting-time indicators
#' @param connections A tidy data frame of parsed connections, one row per
#'   connection, with at least these columns:
#'   from_city, to_city, query_date, query_time, departure.
#'   \code{query_time} is the requested time ("HH:MM") and \code{departure}
#'   is the connection's departure timestamp (POSIXct or parseable string).
#' @description
#' For each origin-destination-date-time query, computes the waiting time
#' (in minutes) between the query time and each connection's departure,
#' then keeps only the connection with the smallest non-negative waiting
#' time. This is the per-query waiting-time indicator.
#'
#' @returns A data frame with one row per query, adding a \code{wait_min}
#'   column (waiting time in minutes).
#' @export
compute_waiting <- function(connections) {
  
  df <- connections
  
  # Build full POSIXct timestamps for query and departure
  df$query_ts     <- as.POSIXct(paste(df$query_date, df$query_time),
                                format = "%Y-%m-%d %H:%M", tz = "UTC")
  df$departure_ts <- as.POSIXct(df$departure, tz = "UTC")
  
  # Waiting time in minutes
  df$wait_min <- as.numeric(
    difftime(df$departure_ts, df$query_ts, units = "mins")
  )
  
  # Keep only non-negative waits (connection hasn't left yet)
  df <- df[!is.na(df$wait_min) & df$wait_min >= 0, ]
  
  # For each query (origin-destination-date-time), keep the smallest wait
  df <- df[order(df$from_city, df$to_city, df$query_date,
                 df$query_time, df$wait_min), ]
  key <- paste(df$from_city, df$to_city, df$query_date, df$query_time)
  df  <- df[!duplicated(key), ]
  
  return(df)
}


#' @title Summarise waiting time by destination
#' @param waiting Output of \code{compute_waiting()}.
#' @description
#' Summarises the per-query waiting times into one row per destination,
#' reporting the median (and mean) waiting time across query times.
#'
#' @returns A data frame with one row per destination city.
#' @export
summarise_waiting <- function(waiting) {
  
  agg <- aggregate(
    wait_min ~ to_city,
    data = waiting,
    FUN = function(x) c(median = median(x), mean = mean(x), n = length(x))
  )
  
  # aggregate() returns a matrix column; flatten it
  out <- data.frame(
    to_city     = agg$to_city,
    median_wait = agg$wait_min[, "median"],
    mean_wait   = agg$wait_min[, "mean"],
    n_queries   = agg$wait_min[, "n"]
  )
  
  return(out)
}


# --- Hardcoded demo data (remove once real parsed data is wired in) ----
# Mimics the parsed connection table: several connections per query.
demo_connections <- data.frame(
  from_city  = "Bellinzona",
  to_city    = c("Lugano","Lugano","Lugano",  "Locarno","Locarno",  "Mendrisio"),
  query_date = "2026-06-19",
  query_time = c("08:00","08:00","08:00",     "08:00","08:00",      "08:00"),
  departure  = c("2026-06-19 08:12:00","2026-06-19 08:27:00","2026-06-19 08:42:00",
                 "2026-06-19 08:18:00","2026-06-19 08:48:00",
                 "2026-06-19 08:25:00"),
  stringsAsFactors = FALSE
)

# Run this to preview:
#   w <- compute_waiting(demo_connections)
#   summarise_waiting(w)