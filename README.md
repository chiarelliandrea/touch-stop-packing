# touch-stop-packing
An R implementation of a touch-and-stop model of particle packing. 

This code is an evolution of my previous work available on Figshare [here](https://doi.org/10.6084/m9.figshare.3839526.v2) and [here](https://doi.org/10.6084/m9.figshare.3839532.v1). The previous code was in MATLAB language; I have now optimised my original code and converted it to R, so it is more broadly available but also better documented.

My original MATLAB code underpins a few academic articles, which you can find below. I am sure that the same or similar principles can be used in other fields of research, however!

* [Generation of virtual asphalt mixture porosity for computational modelling](https://doi.org/10.1016/j.powtec.2015.01.070)
* [Stochastic generation of virtual air pores in granular materials](https://doi.org/10.1007/s10035-015-0585-x)
* [Generation Of 3D Soil/Asphalt Porosity Patterns For Numerical Modelling](https://dx.doi.org/10.3233/978-1-61499-603-3-1089)

The code seeks to fill up a rectangle with growing circles (also called "particles" in the code), following a "touch-and-stop" model. An example of the code's output is available below, and the repository includes a commented R notebook (R Studio code + pdf knit) and the clean source code in R.

If you run this code on your machine, the results will always differ - and that’s exactly what I originally set out to achieve. Please note that I am not a professional programmer, so this code may not be perfect or might be improved. As you can see, it works well as things stand, but do get in touch if you have any suggestions.

![Sample results](https://i.imgur.com/wRCk1IL.png)
