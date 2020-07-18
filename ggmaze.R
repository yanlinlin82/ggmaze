library(tidyverse)
library(animation)

set.seed(20200718)

plot_maze <- function(a, answer = FALSE, polar = FALSE) {
  g <- a %>%
    mutate(x0 = x + 0.5,
           y0 = y + 0.5) %>%
    ggplot(aes(x, y)) +
    geom_segment(aes(xend = x + hline, yend = y)) +
    geom_segment(aes(xend = x, yend = y + vline)) +
    scale_x_continuous(limits = c(0, max(a$x))) +
    scale_y_continuous(limits = c(ifelse(polar, -1, 0), max(a$y))) +
    guides(color = FALSE) +
    theme_void()
  if (!polar) {
    g <- g +
      geom_text(aes(x = x0, y = y0, label = text), size = 10)
  }
  if (answer) {
    g <- g +
      geom_segment(aes(x = x0, y = y0, xend = x0 - 0.5 * track_left, yend = y0), linetype = 2) +
      geom_segment(aes(x = x0, y = y0, xend = x0 + 0.5 * track_right, yend = y0), linetype = 2) +
      geom_segment(aes(x = x0, y = y0, xend = x0, yend = y0 - 0.5 * track_down), linetype = 2) +
      geom_segment(aes(x = x0, y = y0, xend = x0, yend = y0 + 0.5 * track_up), linetype = 2) +
      geom_point(aes(x = x0, y = y0 + 0.5, color = digging_up), shape = 9) +
      geom_point(aes(x = x0 + 0.5, y = y0, color = digging_right), shape = 9) +
      scale_color_manual(values = c(NA, "black"))
  }
  g
}

create_maze <- function(w, h,
                        start = NULL, end = NULL,
                        plot = FALSE, interact = FALSE) {
  if (missing(start)) {
    start <- sample(w, 1)
  }
  if (missing(end)) {
    end <- sample(w, 1)
  }
  cat("start =", start, "\n")
  cat("end =", end, "\n")

  a <- expand.grid(x = 0:w,
                   y = 0:h) %>%
    as_tibble() %>%
    mutate(hline = (x < w),
           vline = (y < h),
           text = "",
           visited = 0,
           sealed = FALSE,
           digging_right = FALSE,
           digging_up = FALSE,
           track_left = FALSE,
           track_right = FALSE,
           track_down = FALSE,
           track_up = FALSE)

  a$hline[a$x == start & a$y == 0] <- FALSE
  a$text[a$x == start & a$y == 0] <- "S"

  a$hline[a$x == end & a$y == h] <- FALSE
  a$text[a$x == end & a$y == h - 1] <- "E"

  visited <- 1
  a$visited[a$x == start & a$y == 0] <- visited

  if (plot) {
    print(plot_maze(a, answer = TRUE))
  }
  if (interact) {
    readLines(n = 1)
  }

  while (sum((a$visited > 0) & !a$sealed) > 0) {
    i <- which((a$visited > 0) & !a$sealed)
    print(i)
    if (length(i) > 1) {
      if (runif(1) < 0.1) {
        i <- sample(i, 1)
      } else {
        i <- i[which.max(a$visited[i])]
      }
    }
    print(i)
    x <- a$x[[i]]
    y <- a$y[[i]]
    d <- NULL
    if (x > 0) {
      if (a$visited[a$x == x - 1 & a$y == y] == 0) {
        d <- c(d, "left")
      }
    }
    if (x < w - 1) {
      if (a$visited[a$x == x + 1 & a$y == y] == 0) {
        d <- c(d, "right")
      }
    }
    if (y > 0) {
      if (a$visited[a$x == x & a$y == y - 1] == 0) {
        d <- c(d, "down")
      }
    }
    if (y < h - 1) {
      if (a$visited[a$x == x & a$y == y + 1] == 0) {
        d <- c(d, "up")
      }
    }
    cat("x =", x, ", y =", y, "\n")
    print(d)
    if (length(d) == 0) {
      a$sealed[a$x == x & a$y == y] <- TRUE
      cat("> sealed\n")
    } else {
      if (length(d) > 1) {
        d <- sample(d, 1)
      }
      cat("> select: ", d, "\n")
      if (d == "left") {
        a$vline[a$x == x & a$y == y] <- FALSE
        visited <- visited + 1
        a$visited[a$x == x - 1 & a$y == y] <- visited
        a$digging_right[a$x == x - 1 & a$y == y] <- TRUE
        a$track_left[a$x == x & a$y == y] <- TRUE
        a$track_right[a$x == x - 1 & a$y == y] <- TRUE
      } else if (d == "right") {
        a$vline[a$x == x + 1 & a$y == y] <- FALSE
        visited <- visited + 1
        a$visited[a$x == x + 1 & a$y == y] <- visited
        a$digging_right[a$x == x & a$y == y] <- TRUE
        a$track_right[a$x == x & a$y == y] <- TRUE
        a$track_left[a$x == x + 1 & a$y == y] <- TRUE
      } else if (d == "down") {
        a$hline[a$x == x & a$y == y] <- FALSE
        visited <- visited + 1
        a$visited[a$x == x & a$y == y - 1] <- visited
        a$digging_up[a$x == x & a$y == y - 1] <- TRUE
        a$track_down[a$x == x & a$y == y] <- TRUE
        a$track_up[a$x == x & a$y == y - 1] <- TRUE
      } else if (d == "up") {
        a$hline[a$x == x & a$y == y + 1] <- FALSE
        visited <- visited + 1
        a$visited[a$x == x & a$y == y + 1] <- visited
        a$digging_up[a$x == x & a$y == y] <- TRUE
        a$track_up[a$x == x & a$y == y] <- TRUE
        a$track_down[a$x == x & a$y == y + 1] <- TRUE
      }

      if (plot) {
        print(plot_maze(a, answer = TRUE))
      }
      if (interact) {
        readLines(n = 1)
      }
    }
  }
  print(a)
  a
}

# generate GIF
saveGIF(create_maze(8, 5, 1, 6, plot = TRUE),
        movie.name = "maze.gif",
        ani.width = 480, ani.height = 280)

# generate a bigger maze
a <- create_maze(50, 35)
g <- plot_maze(a, FALSE)
print(g)
ggsave(g, filename = "big.png", width = 20, height = 13)
