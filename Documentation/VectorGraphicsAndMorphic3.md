# Vector Graphics and Morphic 3 #

One of the original goals of the Cuis project was to enable the evolution of the Morphic graphics and UI framework inherited from Squeak and Self. The objective is to build pixel resolution independent, zoomeable graphics system, based on Vector Graphics. This is still work in progress, so until this text gets outdated, this is the...

## Current status

By default, Cuis still uses BitBltCanvas and BitBltEngine, and the only geometric transformation supported is MorphicTranslation.

But the Cuis repo includes VectorGraphics.pck.st. This includes VectorCanvas and VectorEngine. So, with an updated Cuis setup do:
```
World / Preferences... / Set System Font... / DejaVu... / DejaVuSans
```
Now all the text you see is done using instances of TrueTypeFont. All the glyphs are built using VectorGraphics, and cached in instances of TrueTypeFont. There are several other TrueType fonts available, including Don Knuth's ComputerModern and NeoEuler. Additional fonts can be added simply by copying them to the TrueTypeFonts folder, following the folder structure. You can also do:
```
BitBltCanvas demoUTF8Unicode.							'Uses glyphs cached in Forms inside TrueTypeFonts. Sample text is UTF-8.'.
VectorCanvas demoUTF8Unicode.							'Draws glyphs each time, using VectorEngine. TrueTypeFont cache not used. Supports arbitrary scaling and rotation. Sample text is UTF-8.'
```
Chinese example requires HanWangMingMedium font, freely available on the Web, but not included in our repo because the license is not MIT. Just add the TTF file to your base Cuis folder.

In addition to MorphicTranslation, VectorCanvas and VectorEngine can handle more general AffineTransformations. This means that each Morph can be scaled and / or rotated. All coordinates are FloatingPoint numbers. Rasterization is done with high quality, Signal Processing based Anti-Aliasing. You can, for example, rotate any Morph (including text) by 3 degrees, displace it 0.2 pixels, or zoom it by 5% without any loss of visual quality.

There are several example morphs using Vector Graphics, and showing some of the things that can be done.
```
World / New morph... / From Alphabetica List... / Sample01Star
```
Sample01Star and friends show how simple a Morph can be, and how the programmer is relieved from the complexity in detecting the mouse cursor touching arbitrary geometric shapes, clipping, occlusion, collision detection, scaling, rotation, and lower level problems as bounds detection, area invalidation, etc. Please play with all these examples with the Halos. Read the class comments. Embed them one in another. Understand the code. Modify the code to try new things. See the drawing services provided by VectorCanvas, especially the 'stroke & fill' and 'paths' categories. Open an inspector on your morph, and see how the instance variable 'location' is modified as you move / scale / rotate it. See what happens if you embed your morph in another, and only move the outer morph.

You can also do:
```
Feature require: 'SVG'.
SVGMainMorph exampleWizard openInWorld.
```
There are several other examples included, chosen to show the capabilities of the VectorGraphics engine and SVG support. Browse the class side of SVGMainMorph, category 'examples'. SVG support is quite complete, so you can also try other SVG files as well. Note that drawing complex SVG without the VectorEngine VM Plugin will slow down Cuis significantly. The Virtual Machine Plugin to enable fast drawing of vector graphics is integrated in the OpenSmalltalk Virtual Machine GitHub repository, but not yet in the official builds. In the meantime, you might run one of the VMs built by members of our community, that include the plugin: https://www.dropbox.com/sh/rhkt4ayq24t2xbf/AACDb3mrjMUDB8Mptd-Bi6Zsa?dl=0

If you run Cuis with one of these VMs, and have selected a TrueType font as the system default, you'll see that the Morphic Halos include two new handles: "Change Scale" (center right) and "Rotate" (bottom left). Try them on any Morph!

## History of development

Vector Graphics is actually what got the Cuis project started. In 2003, ten years before the Retina display, I (Juan Vuletich) decided that making Morphic zoomeable and independent of pixel resolution would require completely abandoning back compatibility with the existing Morphic in Squeak. I took Squeak 3.7 and started working on what would later be named Cuis Smalltalk.

You can see more about the early development of the projects at: [The Morphic 3 Project](http://www.jvuletich.org/Morphic3/Morphic3-200911.html), [Morphic 3 in action](http://www.jvuletich.org/Morphic3/Morphic3-201006.html), [First public presentation of the project (video)](http://www.jvuletich.org/Morphic3/Smalltalks2007/Smalltalks2007.html) and [A short history of Cuis](CuisHistory.md).

A more recent landmark was in 2013, when a defensive disclosure on the techniques used in the project was published: [Prefiltering Antialiasing for General Vector Graphics](https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics), [(also here)](https://priorart.ip.com/IPCOM/000232657). This meant the code could now be open-sourced without worries of someone else trying to get a patent on it.

The following years, progress has been steady. The refactoring of Morphic is essentially complete. The most important feature is that in Cuis, each morph defines its own coordinate system. Transformation to owner coordinates is an instance of the GeometryTransformation hierarchy, stored in the 'location' instance variable. This coordinate system is used by the Morph for its own drawing and also for positioning its submorphs.

In 2019, support was added to read and use TrueType fonts. In 2020 I made it possible to include VectorGraphics based morphs (including morphs built from SVG files) to a regular World. In 2021, I wrote a VM plugin for the VectorEngine, gaining enough performance to use VectorGraphics for all morphs, including browsers and other dev tools.
