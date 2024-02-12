

rm(list = ls())
gc()

# load libraries -------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggdensity)
library(ggtext)
library(colorspace)
library(extrafont)
library(ggh4x)

library(ggrepel)

# reading input data ------------------------------

df = "input/tools_microGalaxy.tsv" |> fread()

# Column to exclude ----------------

df$`Galaxy tool ids` = NULL
df$Description       = NULL

df$`bio.tool id`          = NULL
df$`bio.tool name`        = NULL
df$`bio.tool description` = NULL    

df$Source            = NULL

df$`Conda id`      = NULL
df$`Conda version` = NULL

df$`Galaxy wrapper owner`   = NULL
df$`Galaxy wrapper source`  = NULL
df$`Galaxy wrapper version` = NULL

df$`https://usegalaxy.eu`     = NULL
df$`https://usegalaxy.org`    = NULL
df$`https://usegalaxy.org.au` = NULL

df = df[which(`To keep`)]

# EDAM operation plot ----------------------

df2 = df |>
    tidyr::separate_rows("EDAM operation", sep = ",") |>
    setDT()

df2$`EDAM operation` = df2$`EDAM operation` |> str_squish()

# df2 = df2[which(`EDAM operation` != "")]

df2$`EDAM operation` = ifelse(df2$`EDAM operation` == "", "No Operation", df2$`EDAM operation`)

st = df2[, by = `EDAM operation`, .(N = `Galaxy wrapper id` |> unique() |> length())]
st = st[order(-N)]
st = st[seq_len(11)]

df2$cluster = ifelse(
    df2$`EDAM operation` %in% st$`EDAM operation`,
    df2$`EDAM operation`, "Other Operations"
)

df2 = df2[, c(
    "Galaxy wrapper id", 
    "Total tool usage (usegalaxy.eu)", 
    "No. of tool users (2022-2023) (usegalaxy.eu)", 
    "cluster"
)] |> unique()

df2 = df2[, by = cluster, N := `Galaxy wrapper id` |> unique() |> length()]

df2$cluster = df2$cluster |> factor(levels = c(st$`EDAM operation`[-1], "Other Operations", "No Operation"))

df2 = df2[order(cluster)]

df2$strip = paste0("**", df2$cluster, "** (", df2$N, " tools)")
df2$strip = df2$strip |> factor(levels = df2$strip |> unique())

h0 = rbind(
    df2[, by = strip, .SD[which.max(`Total tool usage (usegalaxy.eu)`)]],
    df2[, by = strip, .SD[which.max(`No. of tool users (2022-2023) (usegalaxy.eu)`)]]
) |>
    unique()


gr1 = ggplot(df2, aes(`Total tool usage (usegalaxy.eu)`, `No. of tool users (2022-2023) (usegalaxy.eu)`)) +
    
    # geom_hdr_lines(linewidth = .55, color = darken("#2F509E", .25)) +
    
    geom_point(shape = 21, size = 2, stroke = .2, color = "grey96", fill = "#2E2A2B") +
    
    geom_text_repel(
        data = h0, inherit.aes = FALSE, 
        mapping = aes(
            `Total tool usage (usegalaxy.eu)`, 
            `No. of tool users (2022-2023) (usegalaxy.eu)`, 
            label = `Galaxy wrapper id`
        ),
        bg.color = alpha("white", .5), family = "Calibri", box.padding = .5,
        segment.size = .3
    ) +
    
    scale_x_continuous(
        trans = "log10",  expand = c(0, 0), limits = c(1, 10000000), 
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    
    scale_y_continuous(
        trans = "log10", limits = c(1, 10000),
        labels = scales::comma, expand = c(0, 0), 
        breaks = c(1, 10, 100, 1000, 10000)
    ) +
    
    guides(
        alpha = guide_legend(
            title = "Perc. of observations (tools)",
            title.position = "top", 
            title.theme = element_text(family = "Calibri")
        )
    ) +
    
    facet_wrap2(vars(strip), nrow = 3, axes = "all") +
    
    coord_cartesian() +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        strip.text = element_markdown(),
        
        axis.title.x = element_markdown(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        axis.ticks = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey75"),
        
        panel.border = element_rect(linewidth = .3, fill = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "**Total tool usage** (usegalaxy.eu)",
        y = "**No. of tool users 2022-2023** (usegalaxy.eu)"
    )
    

ggsave(
    plot = gr1, filename = "EDAM_operation.jpeg",
    width = 12, height = 9, units = "in", dpi = 600
)    

# EDAM topic plot ----------------------

df2 = df |>
    tidyr::separate_rows("EDAM topic", sep = ",") |>
    setDT()

df2$`EDAM topic` = df2$`EDAM topic` |> str_squish()

df2$`EDAM topic` = ifelse(df2$`EDAM topic` == "", "No Topic", df2$`EDAM topic`)

st = df2[, by = `EDAM topic`, .(N = `Galaxy wrapper id` |> unique() |> length())]
st = st[order(-N)]
st = st[seq_len(11)]

df2$cluster = ifelse(
    df2$`EDAM topic` %in% st$`EDAM topic`,
    df2$`EDAM topic`, "Other Topics"
)

df2 = df2[, c(
    "Galaxy wrapper id", 
    "Total tool usage (usegalaxy.eu)", 
    "No. of tool users (2022-2023) (usegalaxy.eu)", 
    "cluster"
)] |> unique()

df2 = df2[, by = cluster, N := `Galaxy wrapper id` |> unique() |> length()]

df2$cluster = df2$cluster |> factor(levels = c(st$`EDAM topic`[-1], "Other Topics", "No Topic"))

df2 = df2[order(cluster)]

df2$strip = paste0("**", df2$cluster, "** (", df2$N, " tools)")
df2$strip = df2$strip |> factor(levels = df2$strip |> unique())


h0 = rbind(
    df2[, by = strip, .SD[which.max(`Total tool usage (usegalaxy.eu)`)]],
    df2[, by = strip, .SD[which.max(`No. of tool users (2022-2023) (usegalaxy.eu)`)]]
) |>
    unique()

gr2 = ggplot(df2, aes(`Total tool usage (usegalaxy.eu)`, `No. of tool users (2022-2023) (usegalaxy.eu)`)) +
    
    # geom_hdr_lines(linewidth = .55, color = darken("#2F509E", .25)) +
    
    geom_point(shape = 21, size = 2, stroke = .2, color = "grey96", fill = "#2E2A2B") +
    
    geom_text_repel(
        data = h0, inherit.aes = FALSE, 
        mapping = aes(
            `Total tool usage (usegalaxy.eu)`, 
            `No. of tool users (2022-2023) (usegalaxy.eu)`, 
            label = `Galaxy wrapper id`
        ),
        bg.color = alpha("white", .5), family = "Calibri", box.padding = .5,
        segment.size = .3
    ) +
    
    scale_x_continuous(
        trans = "log10",  expand = c(0, 0), limits = c(1, 10000000),
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    
    scale_y_continuous(
        trans = "log10", limits = c(1, 10000),
        labels = scales::comma, expand = c(0, 0), 
        breaks = c(1, 10, 100, 1000, 10000)
    ) +
    
    guides(
        alpha = guide_legend(
            title = "Perc. of observations (tools)",
            title.position = "top", 
            title.theme = element_text(family = "Calibri")
        )
    ) +
    
    facet_wrap2(vars(strip), nrow = 3, axes = "all") +
    
    coord_cartesian() +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        strip.text = element_markdown(),
        
        axis.title.x = element_markdown(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        axis.ticks = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey75"),
        
        panel.border = element_rect(linewidth = .3, fill = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "**Total tool usage** (usegalaxy.eu)",
        y = "**No. of tool users 2022-2023** (usegalaxy.eu)"
    )


ggsave(
    plot = gr2, filename = "EDAM_topic.jpeg",
    width = 12, height = 9, units = "in", dpi = 600
)



# patchwork -------------------

# library(patchwork)
# 
# 
# gr1 / gr2 + 
#     plot_layout(guides = "collect") &
#     theme(
#         legend.position = "bottom",
#         legend.justification = "left"
#     )













