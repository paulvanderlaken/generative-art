# Metropolis: Generative city visualisations

library(ggplot2)
options(scipen = 999)

generate_metropolis = function(seed,
                               n = 5e4,
                               max_trials = 250,
                               max_exceeds = 50,
                               r = 0.6,
                               angle = 2*pi/180,
                               p_branch = 0.1,
                               width = 80,
                               height = 80) {
  
  # Parameters plotting
  line_size = 2
  bg_col = 'white'
  margin_cm = 2
  
  # Initialise data frames
  points <- data.frame(x = numeric(n), y = numeric(n), dir = numeric(n), level = integer(n))
  edges <-  data.frame(x = numeric(n), y = numeric(n), xend = numeric(n), yend = numeric(n), level = integer(n))
  
  # Set initial point in center
  points[['x']][1] = width / 2
  points[['y']][1] = height / 2
  
  # Make reproducible
  set.seed(seed)
  
  # Main loop
  i <- 2 # start at second point
  n_exceeds = n_trials = trial = 0
  while (i <= n & n_exceeds < max_exceeds) {
    if (n_trials > max_trials) {
      n_exceeds = n_exceeds + 1
    }
    valid <- FALSE
    n_trials = 0
    while (!valid) {
      if (trial > max_trials / 10 | i < 500) {
        # Pick a point at random
        options = seq(1:(i-1))
        random_point <- points[sample(options, 1), ]
      } else {
        # Pick a point among last x
        last = (1 + trial) * 20
        options = max(c(i-last, 1)):i
        random_point <- points[sample(options, 1), ]
      }
      branch <- ifelse(runif(1, 0, 1) <= p_branch, TRUE, FALSE)
      alpha <- random_point$dir[1] + runif(1, -angle, angle) + (branch * (ifelse(runif(1, 0, 1) < 0.5, -1, 1) * pi/2))
      v <- c(cos(alpha), sin(alpha)) * r * (1 + 1 / ifelse(branch, random_point$level[1]+1, random_point$level[1])) # Create directional vector
      xj <- random_point$x[1] + v[1]
      yj <- random_point$y[1] + v[2]
      lvl <- random_point$level[1]
      lvl_new <- ifelse(branch, lvl+1, lvl)
      if(xj < 0 | xj > width | yj < 0 | yj > height) {
        trial = trial + 1
        next
      }
      d = sqrt((xj - points[['x']])^2 + (yj - points[['y']])^2)
      if (min(d) >= 1 * r) {
        points[i, ] <- c(xj, yj, alpha, lvl_new)
        edges[i, ] <- c(xj, yj, random_point$x[1], random_point$y[1], lvl_new)
        valid <- TRUE
        trial = 0
      } else {
        trial = trial + 1
      }
      n_trials = max(c(n_trials, trial))
    }
    i <- i + 1
    cat('\r', i, '/', n, 
        paste0('(', formatC(i/n*100, digits = 1, format = 'f'), '%', ')'), '-',
        'trials:', n_trials, '-', 
        'exceeds:', n_exceeds, '/', max_exceeds, 
        paste0('(', formatC(n_exceeds/max_exceeds*100, digits = 0, format = 'f'), '%', ')'),
        '\t\t\t')
  }
  
  
  # Create plot
  
  edges_selection <- edges[edges$level > 0, ]
  
  p <- ggplot(data = edges_selection) +
    geom_segment(aes(x, y, xend = xend, yend = yend, size = -level), lineend = "round") +
    lims(x = c(0, width), y = c(0, height)) +
    coord_equal() +
    scale_size_continuous(range = c(line_size, line_size)) +
    theme(plot.caption = element_text(color = "green", size = 24),
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          legend.position = "none",
          plot.background = element_rect(fill = bg_col, colour = bg_col),
          panel.background = element_rect(fill = bg_col, colour = bg_col), 
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          panel.spacing=unit(c(0,0,0,0), "null"),
          plot.margin = unit(c(0,0,0,0), "null"),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    # labs(caption = paste(paste0('seed:', seed), 
    #                      paste0('r:', r),
    #                      paste0('angle:', round(angle, 3)),
    #                      paste0('p_branch:', p_branch)
    # )) +
    NULL
  
  # Save plot
  save_plot = function (p) {
    dir = 'output/metropolis'
    if (!dir.exists(dir)) {
      dir.create(dit)
    }
    files = list.files(dir)
    nrs = gsub('\\s?\\..+', '', files)
    nrs = gsub('[^0-9]', '', files)
    nrs = as.numeric(nrs)
    nr = length(files)
    while (nr %in% nrs) {
      nr = nr + 1
    }
    cat('\n')
    cat(paste0('Plot nr', nr))
    # ggsave(paste0(dir, '/', nr, '.png'), p, width = width, height = height , units = "cm", dpi = 300)
    ggsave(paste0(dir, '/', nr, '.pdf'), p, width = width , height = height, units = "cm", dpi = 600)
  }
  
  save_plot(p)
}

generate_metropolis(seed = 20191108, r = 0.62, angle = 0.03, p_branch = 0.1)


