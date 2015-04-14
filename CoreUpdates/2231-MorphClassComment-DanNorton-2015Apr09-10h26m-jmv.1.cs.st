'From Cuis 4.2 of 25 July 2013 [latest update: #2231] on 9 April 2015 at 10:29:27.450522 am'!

!Morph commentStamp: 'dhn 4/8/2015 19:56' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

Structure:
instance var 	Type 					Description 
owner 			Morph		 			My parent Morph, or nil for the top-level Morph, which is a
 				or nil					world, typically a PasteUpMorph.
submorphs 	Array 					My child Morphs. 
location 		MorphicLocation		Specifies position (and possibly, angle of rotation and scale change) inside owner
extension 		MorphExtension 		Allows extra properties to be stored without adding a
				or nil  					storage burden to all morphs. 
!

