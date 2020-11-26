

questions_barplot = function(data, column, main, colour, levels, flip = FALSE) {
  col <- enquo(column)
  n <- data %>% select(!!col) %>% summarise(n=n()) %>% .$n
  breaks <- seq(from = 0, to = n, by = 2)
  data %>%
    ggplot2::ggplot(aes(!!col), 
                    show.legend = FALSE) +
    ggplot2::geom_bar(fill = colours[colour], color = "black") +
    ggplot2::ggtitle(main) +
    ggplot2::scale_x_discrete(name = "Level", 
                              labels = stringr::str_wrap(levels, width = 10)) +
    ggplot2::scale_y_continuous(name = "",
                                breaks = breaks,
                                limits = range(breaks)) -> p
    if (flip) {
      p <- p + ggplot2::coord_flip()
    }
    
    # theme_tufte(base_size = 12) + theme(axis.ticks.x = element_blank())
    p + theme_wsj(base_size = 12) + theme(plot.title = element_text(size=10))
  
}

criteria_barplot = function(data, column, main, colour) {
  col <- enquo(column)
  breaks <- seq(from = 0, to = 8, by = 2)
  data %>%
    ggplot2::ggplot(aes(!!col), 
                    show.legend = FALSE) +
    ggplot2::geom_bar(fill = colours[colour], color = "black") +
    ggplot2::ggtitle(main) +
    # https://community.rstudio.com/t/na-rm-argument-to-geom-bar-not-working/43275/2
    ggplot2::scale_x_discrete(name = "Level", na.translate = TRUE) +
    ggplot2::scale_y_continuous(name = "",
                                breaks = breaks,
                                limits = range(breaks)) +
  
    theme_wsj(base_size = 12) + theme(plot.title = element_text(size=10))

}
