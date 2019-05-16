blend_calc_df <- function(df, x){
  require(dplyr)

  df <- df %>%
        mutate(blended.estimate = x * `full adjustment` + (1 - x) * f.tpvs)

  return(df)
}

blended_calc <- function(df, x, tpvs){
  result <- abs((x * df$`full adjustment` + (1 - x) * df$f.tpvs) - tpvs)

  return(result)
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

df.1 <- read_csv("./data/2018 Fundamentals Results.csv") %>%
        mutate(state = state.abb[match(state, state.name)],
               vs.res = 100*vs.res,
               f.tpvs = vs.res - (100-vs.res))
df.2 <- read_csv("./data/2018 Polls Only Results.csv") %>%
        filter(Seat == "Class I",
               `Poll Count` < 5)
names(df.2) <- tolower(names(df.2))

df <- left_join(df.2, df.1, by = "state")

x.vals <- seq(0, 1, 0.001)
res.table <- t(do.call(rbind, lapply(x.vals, FUN = function(x) blended_calc(df, x, df$`2 party margin`))))
plot.res <- as.numeric(t(colMeans(res.table)))

final <- as.data.frame(cbind(x.vals, plot.res)) %>%
         rename(percent.fund = x.vals, absolute.error = plot.res) %>%
         mutate(absolute.error = absolute.error/100)

p <- ggplot(aes(x = percent.fund, y = absolute.error), data = final) +
     geom_point(size = 1) +
     geom_vline(xintercept = 0.888, linetype = "dashed", size = 1.1) +
     geom_hline(yintercept = 0.032, color = "darkolivegreen3", linetype = "dashed", size = 1.1) +
     geom_hline(yintercept = 0.037, color = "red", linetype = "dashed", size = 1.1) +
     theme_bw() +
     theme(panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           axis.text = element_text(size = 14),
           axis.title = element_text(size = 16),
           plot.title = element_text(size = 20),
           plot.caption = element_text(size = 12)) +
     scale_x_continuous(labels = scales::percent,
                        breaks = seq(.6, 1, .1),
                        limits = c(.6, 1),
                        expand = c(0.01,0.01)) +
     scale_y_continuous(labels = scales::percent,
                        breaks = seq(0, .07, .01),
                        limits = c(0, 0.07),
                        expand = c(0.01,0.01)) +
      labs(caption = "Races with less than 5 polls") +
      xlab("Percent Polls Model") +
      ylab("Average Absolute Error") +
      ggtitle("2018 Blended Model Results")



full.result <- blend_calc_df(df, .888) %>%
               select(state, `2 party margin`, `full adjustment`, f.tpvs, blended.estimate)
