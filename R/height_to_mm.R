#' Pin Height to mm
#'
#' turn any pin heights into mm, in a column named pin_height
#'
#' @param data a data frame; must have a column named either `height_cm` or `height_mm`
#'
#' @return the original data frame, but if there was a `height_cm` or `height_mm` column, it is now named `pin_height`. If original readings were in cm, they have been transformed into mm.
#' @export
#'
#' @examples
#' df <- data.frame(site = c("SET1", "SET2"), height_cm = c(15, 18))
#' height_to_mm(df)
#'
#' df <- data.frame(site = c("SET1", "SET2"), height_mm = c(156, 182))
#' height_to_mm(df)
height_to_mm <- function(data){
    if(exists('height_cm', data)) {
        data <- data %>%
            dplyr::mutate(pin_height = .data$height_cm * 10) %>%
            dplyr::select(-.data$height_cm)
    }
    if(exists('height_mm', data)){
        data <- data %>%
            dplyr::mutate(pin_height = .data$height_mm) %>%
            dplyr::select(-.data$height_mm)
    }
    return(data)
}
