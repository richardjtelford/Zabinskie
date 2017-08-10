ggscreeplot <-
function (x, bstick = FALSE, npcs = min(10, if (is.null(x$CCA) || x$CCA$rank == 0) x$CA$rank else x$CCA$rank), 
          xlab = "Component", ylab = "Inertia", title = "",  ...) {
  if (is.null(x$CCA) || x$CCA$rank == 0) 
    eig.vals <- x$CA$eig
  else eig.vals <- x$CCA$eig
  ncomps <- length(eig.vals)
  if (npcs > ncomps) 
    npcs <- ncomps
  comps <- seq(len = npcs)

  df <- data.frame(Inertia = eig.vals[comps], names = reorder(factor(names(eig.vals[comps])), comps))
  ymax <- df$Inertia[1]
  if (bstick && !is.null(x$CCA) && x$CCA$rank > 0) {
    warning("'bstick' unavailable for constrained ordination")
    bstick <- FALSE
  } else {
    df <- cbind(df, bstick = bstick(x)[comps])
    ymax <-max(ymax, df$bstick[1])
  }
  g <- ggplot(df, aes(x = names)) + 
                geom_col(aes(y = Inertia), fill = "grey50") + 
                labs(x = xlab, y = ylab, title = title) +
                scale_y_continuous(limits = c(0, ymax * 1.04), expand = c(0, 0))
  
  if (bstick) {
                g <- g + geom_point(aes(y = bstick), colour = "red")
                g <- g + geom_line(aes(y = bstick, group =1), colour = "red")
              }
  g
}