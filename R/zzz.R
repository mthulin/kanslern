# On load:
.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

#' Vektor med kanslerns farger
#'
#' 21 farger:
#' nr. 1-3 uka:s huvudsakliga profilfarger
#' nr. 4-6 sekundarfarger
#' rod, gron och gra ar komplementfarger
#' @export
uka_farg_vect <- c(gul1 = "#ffab2e",
                   gul2 = "#fec367",
                   gul3 = "#ffd490",
                   gul4 = "#c68529",
                   gul5 = "#d7a560",
                   gul6 = "#e1bc87",
                   bla1 = "#016cbc",
                   bla2 = "#4290ce",
                   bla3 = "#76acdb",
                   bla4 = "#084d98",
                   bla5 = "#4a78b3",
                   bla6 = "#7a99c5",
                   lila1 = "#632896",
                   lila2 = "#8c63b2",
                   lila3 = "#a98dc7",
                   lila4 = "#4d2470",
                   lila5 = "#775a92",
                   lila6 = "#9883ac",
                   rod = "#c63527",
                   gron = "#7a9a01",
                   gra = "#857874")


#' Funktion som extraherar profilfarger
#'
#' Funktion som ger tillgang till uka:s profilfarger, som de ar lagrade i
#' vektorn uka_farg_vect.
#'
#' @param ... char-vektor som hamtar specifika farger
#'
#' @export
uka_farg <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (uka_farg_vect)

  uka_farg_vect[cols]
}



#' Funktion som skapar paletter baserade pa fargerna i vektorn
#'
#' @return uka_prim:         UKA:s primara farger (9 st)
#' @return uka_sek:          UKA:s sekundara farg (9 st)
#' @return uka_gul/bla/lila: UKA:s gula/bla/lila farger (6 var)
#' @return uka_1:            Forslag pa standardtema (12 st)
#'
#' @export
uka_paletter <- list("uka_prim" =  as.character(c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "1")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "2")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "3")])),
                     "uka_sek"  =  as.character(c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "4")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "5")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "6")])),
                     "uka_gul"  =    as.character(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "gul")]),
                     "uka_bla"  =    as.character(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "bla")]),
                     "uka_lila" =    as.character(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "lila")]),
                     "uka_1"    =  as.character(c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "4")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "1")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "3")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "6")])),
                     "uka_komp" =  as.character(c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "rod")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "gron")],
                                     uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "gra")]))
)

# Temainstallningar for ggplot:
uka_tema <- function(size = 10, angle = "a0")  {

  if (angle == "a0") {a = 0; h = 0.5} else if (angle == "a90") {a = 90; h <- 1}

  ggplot2::theme(legend.position = "bottom",
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 legend.title  = ggplot2::element_blank(),
                 axis.ticks.y.left = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 text = ggplot2::element_text(colour = "black", size = size),
                 axis.text.x = ggplot2::element_text(angle = a, hjust = h),
                 plot.background = ggplot2::element_rect(fill = "white"),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 legend.background = ggplot2::element_rect(fill = "white"),
                 #legend.margin = ggplot2::element_rect(fill = "white"),
                 legend.key = ggplot2::element_rect(fill = "white"),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color = "#9d9d9c", size = 0.1))
}
rlang::on_load(ggplot2::theme_set(uka_tema()))

# Ladda UKA-paletten som forvalda farger i ggplot:
rlang::on_load(
  options(ggplot2.discrete.colour = uka_paletter$uka_1,
          ggplot2.discrete.fill = uka_paletter$uka_1)
)

#' Svensk procent
#'
#' @param x tal som ska anges som procent
#' @param ggr100 boolean; TRUE: x ar i decimalform (och multipliceras med 100). Standard.
#'                        FALSE: x ar inte i decimalform (multpliceras inte med 100).
#' @param n antal decimaler (nsmall). default = 0
#'
#' @export
Svensk_procent <- function(x, ggr100 = TRUE, n = 0) {
  paste0(format(round(x*(ggr100*100)), nsmall = n, decimal.mark =","), " %")
}





#' Svensk antal
#'
#' @param x tal som ska fa "ratt" formatering - " " som tusentalsavg., "," som decimal.
#' @param n antal decimaler (nsmall). default = 0
#'
#' @export
Svensk_antal <- function(x, n = 0) {
  paste0(format((x), big.mark = " ", nsmall = n, decimal.mark = ","))
}

