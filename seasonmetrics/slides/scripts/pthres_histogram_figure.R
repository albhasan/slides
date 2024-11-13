library(dplyr)
library(ggplot2)

data_df <- tibble::tibble(
        Month = factor(
                x = month.abb,
                levels = month.abb
        ),
        Intensity = c(0, 1, 6, 3, 2, 2, 7, 11, 5, 3, 1, 0),
        histlabels = c("", "", "", "", "", "", "2nd", "Peak", "3rd", "4th", "", "")
)

data_df <- data_df %>%
        dplyr::mutate(color = dplyr::if_else(histlabels != "", "red", "white"))

plot_hist <- ggplot2::ggplot(data_df) +
        ggplot2::geom_col(ggplot2::aes(
                x = Month, y = Intensity,
                fill = color
        )) +
        ggplot2::geom_text(
                ggplot2::aes(
                        x = Month,
                        y = Intensity,
                        label = histlabels
                ),
                vjust = -1, position = position_dodge(0.9), size = 3
        ) +
        ggplot2::theme(legend.position = "none")

page_size <- c(210, 297) / 2
out_file <- file.path(
        "~/Documents/github/slides/seasonmetrics/slides/images",
        "peak_thres_hist.png"
)

ggplot2::ggsave(
        plot = plot_hist,
        filename = out_file,
        width = max(page_size),
        height = min(page_size),
        units = "mm"
)
