# Vector Graphics and Morphic 3 #

One of the original goals of the Cuis project has always been to enable the evolution of the Morphic graphics and UI framework inherited from Squeak and Self. The objective is to build pixel resolution independent graphics system, based on Vector Graphics. You can see more about the early development of the projects at: [The Morphic 3 Project](http://www.jvuletich.org/Morphic3/Morphic3-200911.html), [Morphic 3 in action](http://www.jvuletich.org/Morphic3/Morphic3-201006.html), [First public presentation of the project (video)](http://www.jvuletich.org/Morphic3/Smalltalks2007/Smalltalks2007.html) and [A short history of Cuis](CuisHistory.md).

A more recent landmark was in 2013, when a defensive publication on the techniques used in the project was made: [Prefiltering Antialiasing for General Vector Graphics](https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics), [(also here)](https://priorart.ip.com/IPCOM/000232657). This meant the code could now be open-sourced without concerns of someone else trying to get a patent on it.

The following years, progress has been steady. The refactoring of Morphic is essentially complete. The most important feature is that in Cuis, each morph defines its own coordinate system, via the #morphLocalBounds method. Transformation to owner coordinates is an instance of the GeometryTransformation hierarchy, stored in the 'location' instance variable. This coordinate system is used by the Morph for its own drawing and also for positioning its submorphs.

By default, Cuis still uses BitBltCanvas and BitBltEngine, and the only transformation supported is MorphicTranslation.

But the Cuis repo includes VectorGraphics.pck.st. This includes VectorCanvas and VectorEngine. So, with an updated Cuis setup do:
```
World / Preferences... / Load extra fonts
World / Preferences... / Set font... / CMU Concrete
```
If you have any FileList open, close it, and open it again if you want (FileList is not updating all the panes to use the new default font).

Now all the text you see is done using instances of TrueTypeFont. All the glyphs were built using VectorGraphics. If you want to see the whole desktop as drawn by the VectorGraphics engine, evaluate:
```
self runningWorld backgroundImage displayAt: 0@0.
(VectorCanvas onForm: Display) fullDraw: self runningWorld.
Display copy inspect.
```

In addition to MorphicTranslation, VectorCanvas and VectorEngine can handle more general AffineTransformations. This means that each Morph can be scaled and / or rotated. All coordinates are FloatingPoint numbers. Rasterization is done with high quality, Signal Processing based Anti-Aliasing. You can, for example, rotate any Morph (including text) by 3 degrees, displace it 0.2 pixels, or zoom it by 5% without any loss of visual quality. See the comment for VectorCanvas. It includes a doIt you can run, to display your Morphic desktop using only VectorGraphics.

You can also do:
```
Feature require: 'SVG'.
SVGElementMorph examples.
```
to see the SVG support, and the various examples I used.

The high performance OpenCL implementation of the VectorGraphics engine is work in progress. Maybe this is the year it will become usable enough to completely replace BitBltCanvas and BitBltCanvasEngine... You can follow ongoing development in the commits that affect VectorGraphics.pck.st in GitHub. 

Morphic 3 and Vector Graphics in Cuis are in active development, and all the code is available for you to experiment and play with it.

