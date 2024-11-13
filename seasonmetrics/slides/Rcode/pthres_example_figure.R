library(dplyr)
library(ggplot2)

data_df <- tibble::tibble(
        Month = factor(x = 1:12),
        GC = c(380, 60, 78, 96, 30, 22, 56, 32, 112, 220, 500, 450),
        histlabels = c("3rd", "", "", "", "", "", "", "", "", "", "Peak", "2nd")
)

data_df <- data_df %>%
        dplyr::mutate(color = dplyr::if_else(histlabels != "", "red", "white"))

plot_hist <- ggplot2::ggplot(data_df) +
        ggplot2::geom_col(ggplot2::aes(
                x = Month, y = GC,
                fill = color
        )) +
        ggplot2::geom_text(
                ggplot2::aes(
                        x = Month,
                        y = GC,
                        label = histlabels
                ),
                vjust = -1, position = position_dodge(0.9), size = 3
        ) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_y_continuous(limits = c(0, 550))

page_size <- c(105, 297) / 2
out_file <- file.path(
        "~/Documents/github/slides/seasonmetrics/slides/images",
        "peak_thres_example.png"
)

ggplot2::ggsave(
        plot = plot_hist,
        filename = out_file,
        width = max(page_size),
        height = min(page_size),
        units = "mm"
)
