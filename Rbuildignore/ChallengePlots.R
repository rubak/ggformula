require(RColorBrewer)
require(ggformula)
load("/Users/rpruim/projects/github/talks/LessVolume/Data/likertdata.Rdata")
colors1 <- brewer.pal(5, "Spectral")
colorsB <- colors1[c(2,3,4,5,1)]
baseplot <- ggplot(mapping = aes(x=citystate, y=Freq, fill = Response, order=Response)) +
  facet_wrap(~year, nrow=3) +
  geom_bar(data = trial2$neg, stat = "identity") +
  scale_fill_manual(
    breaks=c("Not at all satisfied", "2", "3", "4", "Extremely satisfied"),
    values=colorsB,
    name="Response"
  ) +
  geom_bar(data = trial2$pos, stat = "identity") +
  coord_flip() +
  ggtitle("Community satisfaction") +
  xlab("") +
  ylab("") +
  scale_y_continuous(
    limits=c(-0.5, 1),
    breaks=seq(from=-0.5, to=0.75, by=0.25),
    labels=c("50%", "25%", "0", "25%", "50%", "75%")
  ) +
  theme(
    legend.text=element_text(size=14),
    legend.title=element_text(size=16),
    axis.text=element_text(size=14),
    strip.text=element_text(size=14))
baseplot

reprex::reprex(
  {
    require(RColorBrewer)
    require(ggformula)
    load("/Users/rpruim/projects/github/talks/LessVolume/Data/likertdata.Rdata")
    colors1 <- brewer.pal(5, "Spectral")
    colorsB <- colors1[c(2,3,4,5,1)]
    gf_col(Freq ~ citystate, fill = ~ Response, order = ~ Response, data = trial2$neg) %>%
      gf_col(data = trial2$pos) %>%
      gf_facet_wrap( ~ year, nrow = 3) %>%
      gf_refine(
        scale_fill_manual(
          breaks = c("Not at all satisfied", "2", "3", "4", "Extremely satisfied"),
          values = colorsB,
          name = "Response"),
        scale_y_continuous(
          limits=c(-0.5, 1),
          breaks=seq(from=-0.5, to=0.75, by=0.25),
          labels=c("50%", "25%", "0", "25%", "50%", "75%")
        ),
        coord_flip()
      ) %>%
      gf_labs(
        title = "Community satisfaction", x = "", y = "") %>%
      gf_theme(
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        axis.text=element_text(size=14),
        strip.text=element_text(size=14))
  }
)

reprex::reprex(
  {
    require(RColorBrewer)
    require(ggformula)
    load("/Users/rpruim/projects/github/talks/LessVolume/Data/likertdata.Rdata")
    colors1 <- brewer.pal(5, "Spectral")
    colorsB <- colors1[c(2,3,4,5,1)]
    responses1 <- c("Not at all satisfied", "2", "3", "4", "Extremely satisfied")
    responses2 <- responses1[c(1,5,2,4,3)]
    gf_col(Freq ~ citystate, fill = ~ factor(Response, levels = responses2), data = trial2$neg) %>%
      gf_col(data = trial2$pos, fill = ~ factor(Response, levels = responses2)) %>%
      gf_facet_wrap( ~ year, nrow = 3) %>%
      gf_refine(
        scale_fill_manual(
          breaks = responses1,
          values = colorsB,
          name = "Response"),
        scale_y_continuous(
          limits=c(-0.5, 1),
          breaks=seq(from=-0.5, to=0.75, by=0.25),
          labels=c("50%", "25%", "0", "25%", "50%", "75%")
        ),
        coord_flip()
      ) %>%
      gf_labs(
        title = "Community satisfaction", x = "", y = "") %>%
      gf_theme(
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        axis.text=element_text(size=6, angle = 30),
        strip.text=element_text(size=11))
  }
)
