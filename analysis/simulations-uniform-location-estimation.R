# Setup libraries --------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(latex2exp)
library(tikzDevice)

# Source our main utils functions ----------------------------------------------
source(here::here("R", "utils-uniform-location-estimation.R"))

# Define GLOBAL variables ------------------------------------------------------
# base::set.seed(seed = 242663)
base::set.seed(seed = 724449)
ANALYSIS_REF <- "analysis"

# Derived PATH variables -------------------------------------------------------
PLOT_DIR <- here::here(ANALYSIS_REF, "plots")

# Some intersting 2D examples --------------------------------------------------
# Convex nonagon - with 20 samples
conv_nonagon <- gen_uniform_2D_data_rev(rad = 5, M = 9,
                                             centroid = c(0, 0), closed = TRUE,
                                             n_samps = 20,
                                             v_star = c(4, 5), sigma_star = 1,
                                             K_verts = NULL)
conv_nonagon_plt <- gen_uniform_2D_plot_rev(uniform_2D_data = conv_nonagon)
# conv_nonagon_plt$p1

# Convex nonagon - with 20 samples, v = (10, 5)
conv_nonagon_p10 <- gen_uniform_2D_data_rev(
    rad = 5,
    M = 9,
    centroid = c(0, 0),
    closed = TRUE,
    n_samps = 10,
    v_star = c(20, 3),
    sigma_star = 1,
    K_verts = NULL
)
conv_nonagon_p10_plt <- gen_uniform_2D_plot_rev(
    uniform_2D_data = conv_nonagon_p10
)

# Revised conv_nonagon_p10_plt_rev plot
# This version uses the raw data to create a customized plot directly, rather
# than modifying the output of our plotting function.
# POLY_COL <- "purple"
POLY_COL <- "#D0021C"
POINT_COL <- "#D0021C"
K_COL <- "darkgreen"
conv_nonagon_p10_plt_rev <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = conv_nonagon_p10$K_star, color = "darkgreen",
                     fill = NA, linetype = "dashed", linewidth = 1) +
    ggplot2::geom_sf(data = sf::st_centroid(conv_nonagon_p10$K_star),
                     color = "darkgreen",
                     fill = "darkgreen", linewidth = 1.5) +
    ggplot2::geom_sf(data = conv_nonagon_p10$K_star_t_s, fill = NA,
                     linetype = "dashed", color = "#D0021C", linewidth = 1) +
    ggplot2::geom_sf(data = conv_nonagon_p10$out_intersection,
                     color = "green", fill = "green") +
    ggplot2::geom_hline(yintercept = 0,
                        color = K_COL,
                        linetype = "dotted",
                        linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 0,
                        color = K_COL,
                        linetype = "dotted",
                        linewidth = 0.5) +
    ggplot2::geom_sf(data =
                         sf::st_point(x = conv_nonagon_p10$v_star),
                     color = POINT_COL, fill = POINT_COL,
                     linewidth = 1.5) +
    ggplot2::geom_sf(data = conv_nonagon_p10$Y_i_unif_samples,
                     color = POINT_COL, linewidth = 1) +
    ggplot2::geom_sf(data = conv_nonagon_p10$unif_samps_K_s_polys,
                     color = POLY_COL,
                     fill = POLY_COL,
                     linetype = "dashed",
                     alpha = 0.02,
                     linewidth = 0.1) +
    ggplot2::theme_bw() +
    # ggplot2::theme_minimal() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
    # ggthemes::theme_base() +
    NULL
conv_nonagon_p10_plt_rev

# Produce a png of the plot
cowplot::ggsave2(plot = conv_nonagon_p10_plt_rev,
                 filename = "conv_nonagon_p10_plt_rev.png",
                 width = 15,
                 height = 10,
                 units = "cm",
                 dpi = 900,
                 path = PLOT_DIR)

# Produce a more lightweight TikZ representation of plot
gen_tikz_plot(width = 5, 
              height = 8, 
              plt = conv_nonagon_p10_plt_rev, 
              plt_outdir = PLOT_DIR, 
              plt_outname = "conv_nonagon_p10_plt_rev")
            
# Square with centroid c(2, 0) - with 20 samples
# Demonstrates that we can always re-center our 2D polygon by shifting to (0, 0)
# centroid and applying the estimator
# square_poly_noncen <- gen_uniform_2D_data_rev(rad = NULL, M = NULL,
#                                                    centroid = NULL, closed = NULL,
#                                                    n_samps = 20,
#                                                    v_star = c(4, 5), sigma_star = 1,
#                                                    K_verts = rbind(c(1,-1), c(3,-1),
#                                                                    c(3,1), c(1,1), c(1, -1)))
# square_poly_noncen_plt <- gen_uniform_2D_plot_rev(uniform_2D_data = square_poly_noncen)
# square_poly_noncen_plt$p1

# 2D examples with varying parameters ------------------------------------------
# Let's vary sample size
base::set.seed(1678)
# N_SAMPS_VALS <- base::seq.int(from = 10, to = 120, by = 20)
N_SAMPS_VALS <- c(5L, 10L, 20L, 30L, 50L, 100L)
M_VAL        <- 9 # regular convex polygon of these many sides
V_STAR       <- c(4, 5) # location shift vector
SIGMA_STAR   <- 1

# Generate the underlying Data
conv_poly_n_dat <- N_SAMPS_VALS %>%
    purrr::map(.x = ., .f =
                   ~gen_uniform_2D_data_rev(rad = 5,
                                                 M = 9,
                                                 centroid = c(0, 0), closed = TRUE,
                                                 n_samps = .x,
                                                 v_star = V_STAR,
                                                 sigma_star = SIGMA_STAR,
                                                 K_verts = NULL))
conv_poly_n_plt <- conv_poly_n_dat %>%
    purrr::map(.x = ., .f =
                   ~gen_uniform_2D_plot_rev(uniform_2D_data = .x)$p1)

comb_conv_poly_n_plt_rev <- patchwork::wrap_plots(x = conv_poly_n_plt, nrow = 2)
comb_conv_poly_n_plt_rev

# Produce a png of the plot
cowplot::ggsave2(plot = comb_conv_poly_n_plt_rev,
                 filename = "comb_conv_poly_n_plt_rev.png",
                 width = 15,
                 height = 15,
                 units = "cm",
                 dpi = 900,
                 path = PLOT_DIR)

# Produce a more lightweight TikZ representation of plot
dev.off()
source(here::here("R", "utils-uniform-location-estimation.R"))
gen_tikz_plot(width = 5, 
              height = 8, 
              plt = comb_conv_poly_n_plt_rev, 
              plt_outdir = PLOT_DIR, 
              plt_outname = "comb_conv_poly_n_plt_rev")

# Plot metrics for varying number of replications and n sample values ----------
base::set.seed(965956)

reps <- 1:100
n <- base::seq.int(from = 10, to = 200, by = 10)
all_rep_n_combs <- base::mget(x = c("reps", "n"))
all_rep_n_combs_df <- purrr::cross_df(.l = all_rep_n_combs)

all_rep_n_combs_df <-
    all_rep_n_combs_df %>%
    dplyr::mutate(sim_dat =
                      purrr::map(.x = n, .f =
                                     ~gen_uniform_2D_data_rev(
                                         rad = 5,
                                         M = 9,
                                         centroid = c(0, 0), closed = TRUE,
                                         n_samps = .x,
                                         v_star = V_STAR,
                                         sigma_star = SIGMA_STAR,
                                         K_verts = NULL)),
                  # sim_plot =
                  #     purrr::map(.x = sim_dat, .f =
                  #                    ~gen_uniform_2D_plot_rev(
                  #                        uniform_2D_data = .x)$p1),
                  v_hat_naive = purrr::map(.x = sim_dat, .f = ~.x$v_hat_naive),
                  v_hat_proj = purrr::map(.x = sim_dat, .f = ~.x$v_hat_proj),
                  v_hat_proj_rand = purrr::map(.x = sim_dat,
                                               .f = ~.x$v_hat_proj_rand),
                  v_hat_pitman = purrr::map(.x = sim_dat, .f = ~.x$v_hat_pitman),
                  naive_2_norm = purrr::map(.x = v_hat_naive,
                                            .f = ~base::norm(.x - V_STAR, type="2")),
                  proj_2_norm = purrr::map(.x = v_hat_proj,
                                           .f = ~base::norm(.x - V_STAR, type="2")),
                  proj_rand_2_norm = purrr::map(.x = v_hat_proj_rand,
                                                .f = ~base::norm(.x - V_STAR, type="2")),
                  pitman_2_norm = purrr::map(.x = v_hat_pitman,
                                             .f = ~base::norm(.x - V_STAR, type="2")))

all_rep_n_combs_plt_rev <-
    all_rep_n_combs_df %>%
    dplyr::select(reps, n, naive_2_norm,
                  proj_2_norm, proj_rand_2_norm,
                  pitman_2_norm) %>%
    # dplyr::mutate(n = base::as.factor(n)) %>%
    tidyr::unnest(data = ., cols = c(naive_2_norm,
                                     proj_2_norm,
                                     proj_rand_2_norm,
                                     pitman_2_norm)) %>%
    dplyr::rename(naive  = naive_2_norm,
                  proj   = proj_2_norm,
                  proj_rand   = proj_rand_2_norm,
                  pitman = pitman_2_norm) %>%
    tidyr::pivot_longer(data = ., cols = c(naive, proj, proj_rand, pitman),
                        names_to = "est_type") %>%
    # TODO: Remove proj_rand altogether
    dplyr::filter(est_type != "proj_rand") %>%
    dplyr::select(n, est_type, reps, value) %>%
    dplyr::group_by(n, est_type) %>%
    dplyr::summarize(.data = .,
                     q25 = quantile(value, probs = 0.25),
                     mean_val = mean(x = value),
                     q75 = quantile(value, probs = 0.75)) %>%
    dplyr::rename("Estimator" = "est_type") %>%
    ggplot2::ggplot(data = ., aes(x = n, y = mean_val, color = Estimator)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(aes(ymin=q25, ymax=q75), width=2, size = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom",
                   text = element_text(size=12)) +
    # ggplot2::labs(title =
    #                   latex2exp::TeX("Estimation error in ${L_{2}}$ norm"),
    #               fill = "as",
    #               y = latex2exp::TeX("Estimation error - ${L_{2}}$ norm"),
    #               x = latex2exp::TeX("Sample size $(n)$")) +
    ggplot2::labs(
        fill = "as",
        # title ="Estimation error in $L_{2}$ norm",
        y = "Estimation error",
        x = "Sample size $(n)$") +
    NULL
all_rep_n_combs_plt_rev

# Produce a png of the plot
cowplot::ggsave2(plot = all_rep_n_combs_plt_rev,
                 filename = "all_rep_n_combs_plt_rev.png",
                 width = 15,
                 height = 10,
                 units = "cm",
                 dpi = 900,
                 path = PLOT_DIR)

# Produce a more lightweight TikZ representation of plot
# dev.off()
source(here::here("R", "utils-uniform-location-estimation.R"))
gen_tikz_plot(width = 5, 
              height = 1, 
              plt = all_rep_n_combs_plt_rev, 
              plt_outdir = PLOT_DIR,
              plt_outname = "all_rep_n_combs_plt_rev")
