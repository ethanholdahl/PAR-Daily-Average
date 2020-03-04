water.height <- seq(0, 2, 1)
y <- seq(0, 1500, length.out = 3)
z <- c(0, 5, 10)
df <- data.frame(water.height, y, z)

sky = c("#000000", "#4395f9")
ground = c("#000000", "#32cd3f")

x <- seq(0, 5, 1)
z <- seq(0, 1500, length.out = 6)
y <- rnorm(6, 10, 1)
df <- data.frame(x, y, z)
x
y
cols = blues9
grad_by_val <- function(x, y, cols) {
  require(grid)
  ys <- y
  cols <- colorRamp(cols)(ys) / 256
  cols
  colnames(cols) <- c("red", "green", "blue")
  cols <- apply(cols, 1, function(z)
    do.call(rgb, as.list(z)))
  mat <- matrix(cols, ncol = length(x))
  rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
}



test = time_ele_PAR %>%
  mutate(ele = sin(elevation * pi / 180)) %>%
  select(time, ele, linear_ratio, wang_ratio)

test$ele[test$ele < -.2] = -.2



ggplot(data = test,
       aes(
         x = time,
         y = ele,
         color = ele,
         fill = ele
       )) +
  annotation_custom(
    grob = grad_by_val(test$time, (test$ele + .2) / 1.2, col = sky),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_point(size = 5,
             shape = 21) +
  scale_color_viridis_c(option = "C",
                        rescaler =  function(x, to = NULL, from = NULL) {
                          ifelse(x<.2, 
                                 scales::rescale(x, from = c(0,.2), to = c(.45,.85)),
                                 scales::rescale(x, from = c(.2,1), to = c(.85,1)))
                        }) +
  scale_fill_viridis_c(option = "C",
                       rescaler =  function(x, to = NULL, from = NULL) {
                         ifelse(x<.2, 
                                scales::rescale(x, from = c(0,.2), to = c(.45,.85)),
                                scales::rescale(x, from = c(.2,1), to = c(.85,1)))
                       }
  ) +
  annotation_custom(
    grob = grad_by_val(test$time, (test$ele + .2) / 1.2, col = ground),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = 0
  ) +
  scale_y_continuous(limits = c(-.2,1))

