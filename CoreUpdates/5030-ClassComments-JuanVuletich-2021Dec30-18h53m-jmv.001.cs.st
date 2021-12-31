'From Cuis 5.0 [latest update: #5028] on 30 December 2021 at 7:01:05 pm'!

!Morph commentStamp: 'jmv 12/30/2021 18:57:48' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (a WorldMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets 
drawn. To hide a Morph and its submorphs, send the #visible: message. 

Subclass PlacedMorph defines a local coordinate system (see its class comment). Morphs NOT in the PlacedMorph hierarchy live in the space defined by their owner. Their local coordinate system is the same as the owner's. They ignore #morphPosition:.

Local coordinates are used in the #drawOn: method (the Canvas understands positions in the local coordinate system), for the positions of submorphs (for example #morphPosition and #morphPosition:) and for positions carried by mouse events.

Events are delivered to morphs in Z-order, i.e. if a morph occludes another the event is only delivered to the foremost (just like physical objects). Events received by a morph carry positions in the local coordinate system.

Structure:
instance var 				Type 						Description 
owner 				Morph or nil		 				My parent Morph, or nil for the top-level Morph, which is a WorldMorph
submorphs 				Array 						My child Morphs.!


!PlacedMorph commentStamp: 'jmv 12/30/2021 19:00:39' prior: 0!
PlacedMorphs have a location instance variable, and therefore a specific position (scale and rotation) inside the space defined by their owner. They can move to different places (by user action or by code), and they can be zoomed (i.e. scaled) and rotated.

The location instance variable holds a kind of GeometryTransformation. This defines the local coordinate system of their inner space, used to interpret positions.
Local coordinates are used in the #drawOn: method (the Canvas understands positions in the local coordinate system), for the positions of submorphs (for example #morphPosition and #morphPosition:) and for positions carried by mouse events.
!

