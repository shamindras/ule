# Generate vertices of regular 2D M-gon (convex), inscribed in a unit circle
# Adapted from: \url{https://stackoverflow.com/questions/7198144/how-to-draw-a-n-sided-regular-polygon-in-cartesian-coordinates}
#' Generate a regular 2D M-gon inscribed in a circle of specified radius
#'
#' @param M (integer) : The specified number of vertices (must be positive)
#' @param rad (numberic) : The radius of the circle in which the M-gon
#' is insribed
#' @param centroid (vector) : The centroid of the polygon to be constructed,
#' currently taken to be \code{c(0, 0)} as the origin. The resulting polygon
#' will be shifted by this centroid
#' @param closed (logical) : Will ensure that resulting M-gon constructed has
#' the same starting an end point i.e. will generate \code{M + 1} points with
#' the first point and \code{M + 1}th point coinciding. This is because the
#' \code{sf} package only accepts polygons being closed. Default value is
#' \code{TRUE}.
#'
#' @return (matrix) : Returns matrix of 2D coordinates
#' @export
gen_verts_m_gon_cvx <- function(M, rad, centroid = c(0, 0), closed = TRUE) {
    assertthat::assert_that(is.numeric(M))
    assertthat::assert_that(M > 0)
    assertthat::assert_that(is.numeric(rad))
    assertthat::assert_that(length(centroid) == 2,
                            msg = "Centroid must be of length 2 i.e. 2D centroid")
    
    vert_idcs <- 1:M
    x_verts <- rad * cos(2 * pi * vert_idcs / M) + centroid[1]
    y_verts <- rad * sin(2 * pi * vert_idcs / M) + centroid[2]
    
    if (closed) {
        x_verts_out <- c(x_verts, x_verts[1])
        y_verts_out <- c(y_verts, y_verts[1])
    } else {
        x_verts_out <- x_verts
        y_verts_out <- y_verts
    }
    out_verts <- cbind(x_verts_out, y_verts_out)
    
    base::return(out_verts)
}

#' Shifts a specied polygon by a constant vector i.e. affine shift
#'
#' @param sf_poly (sf) : A 2D \code{sf} polygon
#' @param shft_vec (numeric) : A numeric vector of the same dimension as the
#' specified polygon
#'
#' @return (sf) : A 2D polygon which is the original polygon affine shifted by
#' the given vector
#' @export
get_shifted_sf_polygon <- function(sf_poly, shft_vec){
    assertthat::assert_that(is.numeric(shft_vec))
    assertthat::assert_that(length(shft_vec) == 2) # 2D vector
    
    base::return(sf_poly + shft_vec)
}

#' For 2 polygons (a source polygon and target polygon), with positive lebesgue
#' measure for the target polygon, get the proportion of the volume of the
#' intersection of both polygons relative to the target polygon
#'
#' @param src_poly (sf) : An \code{sf} polygon object
#' @param target_poly (sf) : An \code{sf} polygon object
#'
#' @return (numeric) : The proportion of overlap volume between the source
#' polygon relative to the target polygon
#' @export
get_intersect_prop <- function(src_poly, target_poly){
    assertthat::assert_that(sf::st_area(x = target_poly) > 0,
                            msg = "Target Body must have positive volume")
    intersect_vol <- sf::st_area(sf::st_intersection(x = src_poly,
                                                     y = target_poly))
    target_vol <- sf::st_area(x = target_poly)
    base::return(intersect_vol/target_vol)
}

#' Generate all data for our 2D polygon location estimation example (this
#' currently assumes our scale parameter to be equal to 1)
#'
#' @param M (integer) : The specified number of vertices (must be positive)
#' @param rad (numberic) : The radius of the circle in which the M-gon
#' is insribed
#' @param centroid (vector) : The centroid of the polygon to be constructed,
#' currently taken to be \code{c(0, 0)} as the origin. The resulting polygon
#' will be shifted by this centroid
#' @param closed (logical) : Will ensure that resulting M-gon constructed has
#' the same starting an end point i.e. will generate \code{M + 1} points with
#' the first point and \code{M + 1}th point coinciding. This is because the
#' \code{sf} package only accepts polygons being closed. Default value is
#' \code{TRUE}.
#' @param n_samps (integer) : The number of points to sample from the 2D polygon
#' we have generated
#' @param v_star (double) : The location shift vector, this is the vector we will
#' shift all the sampled points in our generated 2D polygon by
#' @param sigma_star (double) : The scale parameter, this is the number by which
#' we will scale our original generated 2D polygon
#' @param K_verts (list) : A list of closed vertices manually defining the 2D
#' polygon, where the centroid should be the origin. If this is set to \code{NULL}
#' then all the \code{rad, M, centroid, closed} values must all be non-\code{NULL}
#'
#' @return (list) : Returns a list containing all relevant data generation
#' values for the generated 2D polygon required for explaining our plotted
#' example
#' @export
#'
#' @examples
#' \dontrun{
#' reg_poly <- gen_uniform_2D_data_rev(rad = 1, M = 5,
#'                                     centroid = c(0, 0), closed = TRUE,
#'                                     n_samps = 20, v_star = c(5, 2),
#'                                     sigma_star = 1,
#'                                     K_verts = NULL)
#'
#' cust_poly <- gen_uniform_2D_data_rev(rad = NULL, M = NULL,
#'                                      centroid = NULL, closed = NULL,
#'                                      n_samps = 20, v_star = c(5, 2),
#'                                      sigma_star = 1,
#'                                      K_verts = rbind(c(-1,-1), c(1,-1),
#'                                                     c(1,1), c(-1,1),
#'                                                     c(-1, -1)))
#'}
gen_uniform_2D_data_rev <- function(rad, M,
                                    centroid = c(0, 0), closed = TRUE,
                                    n_samps, v_star, sigma_star = 1, K_verts = NULL){
    assertthat::assert_that(((!base::is.null(rad) && !base::is.null(M) &&
                                  !base::is.null(centroid) &&
                                  !base::is.null(closed)) &&
                                 base::is.null(K_verts))
                            || ((base::is.null(rad) && base::is.null(M) &&
                                     base::is.null(centroid) &&
                                     base::is.null(closed)) &&
                                    !base::is.null(K_verts)))
    
    if(is.null(K_verts)){
        K_verts         <- gen_verts_m_gon_cvx(M = M, rad = rad,
                                                    centroid = centroid,
                                                    closed = closed)
    } else{
        # Check that our 2D polygon vertices are in the right format and
        # not just a single point
        assertthat::assert_that(base::is.double(K_verts) &&
                                    base::length(K_verts) > 2)
        
        # Determine the key 2D polygon shapes
        # Ensure that manually defined polygons are re-centered at the origin
        # (if they already aren't defined accordingly)
        K_verts_sf          <- sf::st_polygon(x = list(K_verts))
        K_verts_sf_centroid <- sf::st_centroid(x = K_verts_sf)
        K_verts             <- sf::st_coordinates(x = K_verts_sf -
                                                      K_verts_sf_centroid)[, 1:2] %>%
            unname(obj = .)
        # TODO: Add code to ensure that the defined polygon is closed
    }
    
    # This should ensure that out 2D polygon is centered at c(0, 0)
    K_star <- sf::st_polygon(x = list(K_verts))
    K_star <- K_star - sf::st_centroid(x = K_star)
    
    # Define key shapes --------------------------------------------------------
    
    # Derived true values of K, and scaled and shifted versions
    K_star_s    <- K_star * sigma_star
    K_star_t_s  <- K_star_s + v_star # Scale and shift the polygon
    
    # More operations on the shifted and scaled polygon
    # Centroid of the true scaled and shifted polygon
    K_star_t_s_cent <- sf::st_centroid(x = K_star_t_s) %>%
        sf::st_coordinates(x = .) %>%
        base::unname(obj = .) %>%
        base::as.numeric(x = .)
    
    # Uniform sampling of points Y_i from the true scaled and shifted polygon
    # This is as a matrix class
    Y_i_unif_samples_mat <-
        K_star_t_s %>%
        sf::st_coordinates(x = .) %>%
        .[, 1:2] %>%
        base::unname(obj = .) %>%
        uniformly::runif_in_polygon(n = n_samps,
                                    vertices = .,
                                    center = K_star_t_s_cent)
    
    # Get them as a sf multipoint object, for plotting as a single collection
    Y_i_unif_samples <- Y_i_unif_samples_mat %>%
        sf::st_multipoint(x = .)
    
    # Get the sampled simulated vectors from within K, we need this to shi
    Y_i_unif_samples_vs <-
        Y_i_unif_samples_mat %>%
        magrittr::set_colnames(x = .,
                               value = c("V1", "V2")) %>%
        tibble::as_tibble(x = ., .name_repair = NULL) %>%
        purrr::transpose(.l = .) %>%
        purrr::map(.x = .,
                   .f = ~unlist(x = ., use.names = FALSE))
    
    # Get translated polygon covers of uniformly sampled vectors from polygon K
    unif_samps_K_s_polys <-
        Y_i_unif_samples_vs %>%
        purrr::map(.x = .,
                   .f = ~get_shifted_sf_polygon(sf_poly = -K_star_s,
                                                     shft_vec = .x)) %>%
        sf::st_multipolygon(x = .)
    
    # Perform our estimation ---------------------------------------------------
    
    # Get the common intersection of all of the Minkowski sum
    # translated convex bodies
    out_intersection <- unif_samps_K_s_polys %>%
        purrr::reduce(.x = ., sf::st_intersection)
    
    # Estimate location vector v_star -----------------------------------------------
    # Get sample mean of Y_i's, this is our naive estimator
    v_hat_naive <- base::colMeans(x = Y_i_unif_samples_mat)
    # Convert to an sf point, will be used for projection later
    v_hat_naive_pt <- sf::st_point(x = v_hat_naive)
    
    # Get the Pitman estimator
    v_hat_pitman <- sf::st_centroid(out_intersection) %>%
        sf::st_coordinates(x = .) %>%
        base::unname(obj = .) %>%
        base::as.numeric(x = .)
    
    
    v_hat_proj_rand <- out_intersection %>%
        sf::st_cast(x = sf::st_sfc(.), to = "POINT") %>%
        sample(x = ., size = 1) %>%
        sf::st_coordinates(x = .) %>%
        base::unname(obj = .) %>%
        base::as.numeric(x = .)
    
    # Get projection of sample means on the intersection, or just uniformly
    # sample a point from the intersection.
    # First get the projection points. This will be unique.
    # We project from the sample mean to the intersection set
    # This will return the full straight line starting at the sample mean, and
    # ending at the projection point on the intersection set.
    # If the sample mean lies in the intersection set, then the projection line
    # will lie inside the linestring.
    prj_line <- sf::st_nearest_points(v_hat_naive_pt, out_intersection)
    
    # Get the ending projection points of our linestring.
    # Cast as a sf point object.
    end_prj_pts <-  sf::st_cast(prj_line, "POINT")
    
    # Get projection of sample means on the intersection.
    # This is the second endpoint of the linestring we have formed in our
    # projection to our intersection set. The first point is the sample mean,
    # which we can safely ignore.
    v_hat_proj <- end_prj_pts[2][[1]] %>%
        base::unname(obj = .) %>%
        base::as.numeric(x = .)
    
    # Estimate sigma -----------------------------------------------------------
    # TODO: For now, we just set it to the true value, we will estimate this
    #       later using a linear program
    sigma_hat <- sigma_star
    
    # Get translated and scaled K_hat (for sample mean and our estimator)
    # Derived true values of K, and scaled and shifted versions
    K_hat_s          <- K_star * sigma_hat
    K_hat_naive_t_s  <- K_hat_s + v_hat_naive # Scale/naive translate the polygon
    K_hat_proj_t_s   <- K_hat_s + v_hat_proj # Scale/projected translate the polygon
    K_hat_pitman_t_s <- K_hat_s + v_hat_pitman # Scale/Pitman translate the polygon
    
    # Get intersection volume for all estimators -------------------------------
    naive_prop <- get_intersect_prop(src_poly = K_hat_naive_t_s,
                                     target_poly = K_star_t_s)
    
    proj_prop <- get_intersect_prop(src_poly = K_hat_proj_t_s,
                                    target_poly = K_star_t_s)
    
    pitman_prop <- get_intersect_prop(src_poly = K_hat_pitman_t_s,
                                      target_poly = K_star_t_s)
    
    # Output all of the required variables for plotting and analysis -----------
    out_vars <- c("n_samps", "v_star", "sigma_star",
                  "K_star", "K_star_t_s", "K_star_t_s_cent",
                  "unif_samps_K_s_polys", "Y_i_unif_samples",
                  "out_intersection",
                  "v_hat_naive", "v_hat_pitman",
                  "v_hat_proj_rand", "v_hat_proj",
                  "K_hat_naive_t_s", "K_hat_proj_t_s",
                  "sigma_hat", "naive_prop", "proj_prop")
    base::return(base::mget(x = out_vars))
}

# Plot outputs -----------------------------------------------------------------
#' Creates a summary plot of all of our generated data for our 2D polygon
#' location estimation example (this currently assumes our scale parameter
#' to be equal to 1)
#'
#' @param uniform_2D_data (list) : A list object that is the output of calling
#' the \code{gen_uniform_2D_data} function
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' reg_poly <- gen_uniform_2D_data_rev(rad = 5, M = 9,
#'                                     centroid = c(0, 0), closed = TRUE,
#'                                     n_samps = 20,
#'                                     v_star = c(4, 5), sigma_star = 1,
#'                                     K_verts = NULL)
#' reg_poly_plt <- gen_uniform_2D_plot_rev(uniform_2D_data = reg_poly)
#' reg_poly_plt$p1
#'
#' cust_poly <- gen_uniform_2D_data_rev(rad = NULL, M = NULL,
#'                                      centroid = NULL, closed = NULL,
#'                                      n_samps = 20,
#'                                      v_star = c(4, 5), sigma_star = 1,
#'                                      K_verts = rbind(c(1,-1), c(3,-1),
#'                                                      c(3,1), c(1,1), c(1, -1)))
#' cust_poly_plt <- gen_uniform_2D_plot_rev(uniform_2D_data = cust_poly)
#' cust_poly_plt$p1
#' }
gen_uniform_2D_plot_rev <- function(uniform_2D_data){
    p1 <- ggplot2::ggplot() +
        ggplot2::theme_minimal() +
        ggplot2::geom_sf(data = uniform_2D_data$K_star, color = "lightblue",
                         fill = NA, linetype = "dashed", linewidth = 1.5) +
        ggplot2::geom_sf(data = uniform_2D_data$K_star_t_s, fill = NA,
                         linetype = "dashed", color = "black", linewidth = 1.5) +
        ggplot2::geom_sf(data = sf::st_centroid(x = uniform_2D_data$K_star_t_s),
                         fill = "yellow",
                         shape = 23, linewidth = 1) +
        ggplot2::geom_sf(data = uniform_2D_data$Y_i_unif_samples,
                         color = "purple", linewidth = 1) +
        ggplot2::geom_sf(data = uniform_2D_data$K_hat_naive_t_s, fill = NA,
                         linetype = "dotted", color = "darkgreen") +
        ggplot2::geom_sf(data = uniform_2D_data$K_hat_proj_t_s, fill = NA,
                         linetype = "dashed", color = "red", linewidth = 1.5) +
        ggplot2::geom_sf(data = uniform_2D_data$out_intersection,
                         color = "green", fill = "green") +
        ggplot2::geom_sf(data =
                             sf::st_centroid(x = uniform_2D_data$out_intersection),
                         color = "blue", fill = "blue", linewidth = 0.5) +
        ggplot2::labs(title = glue::glue("$n = {uniform_2D_data$n_samps}$")) +
        ggplot2::theme(text = element_text(size=12))
    
    p2 <- p1 +
        ggplot2::geom_sf(data = uniform_2D_data$unif_samps_K_s_polys,
                         color = "purple",
                         fill = "purple",
                         alpha = 0.001,
                         linewidth = 0.1)
    out_plots_vars <- c("p1", "p2")
    base::return(base::mget(out_plots_vars))
}

# Produce a more lightweight TikZ representation of plot
gen_tikz_plot <- function(width, height, plt, plt_outdir, plt_outname){
    # turn of any plot devices, so that we save a clean figure output
    if(!is.null(dev.list())){
      dev.off()
    }
    
    plt_outpath <- here::here(plt_outdir, glue::glue("{plt_outname}.tex"))
    # Delete the TeX file, so that we always recreate it
    if(file.exists(plt_outpath)){
      file.remove(plt_outpath)
    }
    tikz(
      file = plt_outpath,
      width = width,
      height = width
    )
    print(plt)
    dev.off()
    
    # removes unnecessary whitespace around the plot when importing into LaTeX
    # this deletes all lines that invisibly mess up the bounding box
    # source: https://stackoverflow.com/a/41186942/4687531
    lines <- readLines(con = plt_outpath)
    lines <- lines[-which(grepl("\\path\\[clip\\]*", lines, 
                                perl = F))]
    lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines, 
                                perl = F))]
    writeLines(lines, con = plt_outpath)
}