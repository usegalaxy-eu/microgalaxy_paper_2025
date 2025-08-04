
# Load libraries -----------------------------------------

library(data.table)
library(stringr)

library(tidytext)

library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggh4x)

library(shadowtext)
library(extrafont)
library(colorspace)

library(patchwork)

# Input data -----------------------------------------

all_papers       <- fread("https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/results/citations/all_papers.csv")
microbial_papers <- fread("https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/results/citations/microbial_papers.csv")

all_papers       <- all_papers[which(year >= 2006)]
microbial_papers <- microbial_papers[which(year >= 2006)]

# Barplots ---------------------------------------------------

gr_df <- microbial_papers |> 
    subset(select = c("year", "Targeted organisms", "Technical target", "Methods")) |>
    melt(id.vars = "year", variable.factor = FALSE, value.factor = FALSE)

gr_df <- gr_df |> tidyr::separate_rows("value", sep = "\\,") |> setDT()

gr_df$value <- gr_df$value |> str_squish()

gr_df <- gr_df |> subset(subset = value != "")
gr_df <- gr_df[, by = .(variable, value), .N]

gr_df[, by = variable, Freq := N / sum(N)]

gr <- gr_df |> 
    ggplot(aes(reorder_within(value, -Freq, variable), Freq)) +
    geom_point(fill = "#4E79A7" |> lighten(.25), color = "grey25", shape = 21, size = 5.3+.4, stroke = .25) +
    geom_col(width = .75, fill = "#4E79A7" |> lighten(.25), color = "grey25", linewidth = .15) +
    geom_point(color = "#4E79A7" |> lighten(.25), size = 4.63+.4) +
    scale_x_reordered() +
    scale_y_continuous(labels = scales::percent, limits = c(0, .4), expand = c(0, 0)) +
    facet_grid2(cols = vars(variable), scales = "free_x", space = "free_x", axes = "all") + 
    theme_minimal(base_family = "Calibri") +
    theme(        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dotted", lineend = "round"),

        axis.line  = element_line(lineend = "round"),
        axis.ticks = element_line(lineend = "round"),

        axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_markdown(margin = margin(r = 10)),

        strip.text = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    ) +

    labs(x = "Target", y = "**Percentage in the microbial related papers**<br>citing Galaxy papers")


# Save plots -------------------------------------

outfolder <- "../docs/extended/"

dir.create(outfolder, showWarnings = FALSE)

save_plot <- function(plot, filename, w, h) {
    
    ggsave(
        plot = plot, filename = paste0(filename, ".png"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = plot, filename = paste0(filename, ".svg"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = plot, filename = paste0(filename, ".pdf"),
        width = w, height = h, units = "in", device = cairo_pdf
    )
    
}

save_plot(gr, "../docs/extended/supplementary_figure_1", 8, 6)

