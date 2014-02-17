Approximation of the inventory volume integral
========================================================

<!---
Just for some tests
-->

Here is a quick overview of the approximation used to compute the volume integral :

The mass of tritium present at the site (in units of activity i.e. Ci) can be computed as follows :

$$m_{tritium}=\int_D nCdV$$

where n is the porosity and C is the activity concentration. The domain for this integral corresponds to the drainage area downgradient of the F-area seepage basins and the aquifers below. The domain is not a regular polyhedron, and we do not know the integrand at every point :

$$m_{tritium}=\iiint_D n(x,y,z)C(x,y,z)dxdydz$$

The first approximation is the use of and average porosity over the whole domain :

$$m_{tritium}\approx\frac{1}{V_{tot}}\iiint_D n(x,y,z)dxdydz \cdot \iiint_DC(x,y,z)dxdydz \\ 
   m_{tritium}\approx \bar{n} \int_DC(x,y,z)dxdydz$$

The second approximation comes from the fact that we do not know the concentration at every x, y and z. We know for a fact that the concentration is not uniform in z, but the measurements collected provide the concentration average over the screen length $h(x,y)$, which in most cases we can replace by the aquifer thickness $h(x,y)$ in the case of fully penetrating wells i.e.:

$$\overline{C_z(x,y)} = \frac{1}{h(x,y)} \int_h C(x,y,z) dz$$

$$m_{tritium}\approx \bar{n} \iint_{D_{xy}}\left(\int_{h}C(x,y,z)dz\right)dxdy \\ 
   m_{tritium}\approx \bar{n} \iint_{D_{xy}}\overline{C_z(x,y)}h(x,y)dxdy$$

There are several ways to deal with that final integral. One can look at the available samples for $\overline{C_z(x,y)}$ and $h(x,y)$, and use a bootstrap estimate to provide mean and standard errors. We approximate the intergral by a point by point computation using an interpolant on a regular grid :

$$
\begin{aligned}
  m_{tritium}\approx \bar{n} \sum_{i,j}\overline{C_z(x_i,y_j)}h(x_i,y_j)A_{ij} \\
   m_{tritium}\approx \bar{n} A_{tot} \frac{1}{n_in_j} \sum_{i,j}\overline{C_z(x_i,y_j)}h(x_i,y_j)
\end{aligned}
$$
   
As one can see this is equivalent to computing a mean of the population and multiply this by the total area. This estimate could be obtained by Monte-Carlo Integration
