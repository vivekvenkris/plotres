# plotres
plotres is an interactive GUI for analysing TEMPO residuals. 

The software is written by Prof. Norbert Wex (wex@mpifr-bonn.mpg.de) and Prof. Michael Kramer (mkramer@mpifr-bonn.mpg.de). I have not contributed to the development of this software. This repository is created just to pull it inside my docker container. 

# Requirements:
```
gfortran
pgplot
X11
```
# Compilation:
```
gfortran -o ~/bin/plotres plotres.f -lcpgplot -lpgplot -lX11 -lm
gfortran -o ~/bin/plotres_ps plotres_ps.f -lcpgplot -lpgplot -lX11 -lm 
```

# Usage:

1. Run `tempo` with your `.tim` and `.par` files which will create a `resid2.tmp` file
2. run `./plotres` in the same directory with no other arguments. 
