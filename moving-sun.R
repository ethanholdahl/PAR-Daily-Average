water.height <- seq(0, 2, 1)
y <- seq(0, 1500, length.out = 3)
z <- c(0,5,10)
df <- data.frame(water.height, y, z)

grad_by_val <- function(elevation, ratio, cols) {
  require(grid)
  cols <- colorRamp(cols, interpolate = "linear", bias = .1)(elevation) / (256)
  colnames(cols) <- c("red", "green", "blue")
  cols <- apply(cols, 1, function(z) do.call(rgb, as.list(z)))
  mat <- matrix(cols, ncol = length(x))
  rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
}
sky = c("#000000", "#4395f9")
ground = c("#152e17", "#32cd3f")


  ggplot(data = test, aes(x = time, y = ele))+
    annotation_custom(
      grob = grad_by_val(test$time, (test$ele+.25)/1.25, col = sky),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) +
    geom_point(
      size = 5,
      color = "#fffb29",
      fill = "#fffb29",
      shape = 21
    ) +
    annotation_custom(
      grob = grad_by_val(test$time, (test$ele+.25)/1.25, col = ground),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = 0
    )
  
  x <- seq(0, 5, 1)
  z <- seq(0, 1500, length.out = 6)
  y <- rnorm(6, 10, 1)
  df <- data.frame(x, y, z)
  x
  y
  cols = blues9
  grad_by_val <- function(x, y, cols = blues9) {
    require(grid)
    ys <- y
    cols <- colorRamp(cols)(ys)/ 256
    cols
    colnames(cols) <- c("red", "green", "blue")
    cols <- apply(cols, 1, function(z) do.call(rgb, as.list(z)))
    mat <- matrix(cols, ncol = length(x))
    rasterGrob(
      image = mat,
      width = unit(1, "npc"),
      height = unit(1, "npc"),
      interpolate = TRUE
    )
  }
  
  
  
test = time_ele_PAR %>%
  mutate(ele = sin(elevation*pi/180)) %>%
  select(time, ele, linear_ratio, wang_ratio)
  
test$ele[test$ele < -.25] = -.25
