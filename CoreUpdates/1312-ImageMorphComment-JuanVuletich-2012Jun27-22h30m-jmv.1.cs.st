'From Cuis 4.0 of 21 April 2012 [latest update: #1311] on 27 June 2012 at 10:33:01 pm'!

!ImageMorph commentStamp: '<historical>' prior: 0!
ImageMorph is a morph that displays a picture (Form). My extent is determined by the extent of my form.

Use #image: to set my picture.

Structure:
 instance var		Type 		Description
 image				Form		The Form to use when drawing

Code examples:
	ImageMorph new openInWorld; grabFromScreen

	ImageMorph new image: (Form fromFileNamed: 'myGraphicsFileName'); openInWorld.

Relationship to SketchMorph: ImageMorph should be favored over SketchMorph, a parallel, legacy class -- see the Swiki FAQ for details ( http://minnow.cc.gatech.edu/squeak/1372 ). 
!

