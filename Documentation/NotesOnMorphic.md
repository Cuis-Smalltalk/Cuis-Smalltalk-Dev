Notes on Morphic
================

Part of the Cuis development is the move to a new version of Morphic called *Morphic3*.

The notes below indicate some of the changes and give the reasons for it.


Scalable graphics instead of bitmapped graphics
-----------------------------------------------

Morphic3 will have scalable graphics.

To achieve this the following changes have been made

1. Integer coordinates of Squeak have been changed into float coordinates.
2. Morphs are positioned relative to their container (relative positioning), rather than a single, global screen position (absolute positioning).
   
   
Cuis Morph attributes
---------------------

A **Cuis Morph** has 
- a location (relative to its owner) and 
- an extent.

This means that the origin of its rectangle is always 0@0.

You can see this in the difference in #drawOn: Squeak and **Cuis**.


           [Squeak]Morph>>drawOn: aCanvas
             aCanvas fillRectangle: self bounds
                    fillStyle: self fillStyle
                    borderStyle: self borderStyle.

versus


           [CUIS]Morph>>drawOn: aCanvas
          "A canvas is already set with a proper transformation from our
                       coordinates to those of the Canvas target."
            aCanvas fillRectangle: (0@0 extent: self morphExtent)
                color: self color


MorphExtension (Squeak) versus properties (Cuis)
------------------------------------------------

Following the Cuis philosophy, Morphs have been 'simplified'.

Note the difference in Ivars (instant variables) from Squeak. 
Squeak Morphs look slimmer in terms of number of variables, but a Squeak morph typically  holds 
a MorphExtension instance -- which is not slim at all.

Cuis Morphs just use properties where needed.


Classes Rectangle and RectangleLikeMorph
----------------------------------------



**Rectangle** in *Cuis*:

    Object subclass: #Rectangle
    
"I represent a rectangular area of the screen. Arithmetic functions take points as arguments 
      and carry out scaling and translating operations to create new instances of me. Rectangle functions create new instances by determining intersections of rectangles with rectangles.
Note that only rectangles parallel to reference frame (Screen) can be represented by this class."


**RectangleLikeMorph** in *Cuis*:

      Morph subclass: #RectangleLikeMorph
    
"Hierarchy for morphs that are rectangle like. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Subclasses can add things like 'roundedCornerRadious' or such."


**Conclusion**

- A Cuis Rectangle is very much like Squeak Rectangle. 
- A Cuis RectangleLikeMorph is not too unlike Squeak RectangleMorph.



How to analyse a Morphic GUI
----------------------------

[These notes](HowToAnalyseAMorphicGUI.md) show you how an existing Morphic GUI like the FileList browser may be taken apart to see how it is constructed. It is possible to find out live which instances of model classes certain GUI elements activate.

