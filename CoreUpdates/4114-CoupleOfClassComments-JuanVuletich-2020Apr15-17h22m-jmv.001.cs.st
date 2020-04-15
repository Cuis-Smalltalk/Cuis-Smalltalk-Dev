'From Cuis 5.0 [latest update: #4113] on 15 April 2020 at 5:38:57 pm'!

!GeometryTransformation commentStamp: '<historical>' prior: 0!
Superclass of several Geometry Transformations used mainly to specify locations of Morphs.

Morphs specify a coordinate system in which they and their submorphs are expressed. A Morph's coordinate system is defined by a GeometryTransformation that is applied to points in inner space to convert them to points in outer space. Therefore #externalizePosition: is equivalent of #transform: and #internalizePosition: is equivalent to #inverseTransform:!


!Morph commentStamp: '<historical>' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

Structure:
instance var 	Type 						Description 
owner 		Morph		 				My parent Morph, or nil for the top-level Morph, which is a
 				or nil						world, typically a PasteUpMorph.
submorphs 	Array 						My child Morphs. 
location 	GeometryTransformation 	Specifies position (and possibly, angle of rotation and scale change) inside owner
												See comment at GeometryTransformation
extension 	MorphExtension 			Allows extra properties to be stored without adding a
				or nil  						storage burden to all morphs. 
!

