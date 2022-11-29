# ule

Repository to reproduce simulated figures from the paper *"Uniform Location Estimation on Convex Bodies"*. This repository is developed and maintained by

- [Matey Neykov](https://www.shamindras.com/)
- [Shamindra Shrotriya](https://www.shamindras.com/)

## Citation

To reference this work, please cite the latest version of the paper

```bib
@misc{shrotriya2022ulseconvexbodies,
	title        = {Uniform Location Estimation on Convex Bodies},
	author       = {Shamindra Shrotriya and Matey Neykov},
	year         = 2022,
	journal      = {Preprint}
}
```

## Installation

In order to reproduce the figures you need to first install the `renv` package
from CRAN in a separate `R` session.

``` r
install.packages("renv")
```

You can then clone the repository locally as usual, for example using SSH

```bash
git@github.com:shamindras/ule.git #SSH approach
```
## Reproducibility

1. After locally cloning the repository, you can open the `ule.Rproj` project file
in RStudio. This will create a separate RStudio session.

2. Once `ule.Rproj` is opened in RStudio, you can open the file

   ```
   analysis/simulations-uniform-location-estimation.R
   ```

3. Select all the contents, and run the entire file. It will produce both
   `png/TeX` versions of the three figures, as follows:

   ```bash
   analysis/plots/all_rep_n_combs_plt_rev.png # -> Figure 2 in paper
   analysis/plots/comb_conv_poly_n_plt_rev.png # -> Figure 6 in paper
   analysis/plots/conv_nonagon_p10_plt_rev.png # -> Figure 7 in paper
   ```

   **Note:** In the paper we use the `.tex` versions for the figures not the
   `.png` to reduce the resulting paper file size. However both the `.tex` and
   corresponding named `.png` files produce equivalent plots. The `.png` formats
   are typically easier to view.