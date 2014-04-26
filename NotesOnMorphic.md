Notes on Morphic
================

Part of the Cuis development is the move to a new version of Morphic called *Morphic3*.

The notes below indicate some of the changes and give the reasons for it.


Scalable graphics instead of bitmapped graphics
-----------------------------------------------

Morphic3 will have scalable graphics.

To achieve this the follwoing changes have been made

1. Integer coordinates of Squeak have been changed into float coordinates.
2. Morphs be positioned relative to their container, rather than a single, global screen position (absolute positioning).
   
   
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
