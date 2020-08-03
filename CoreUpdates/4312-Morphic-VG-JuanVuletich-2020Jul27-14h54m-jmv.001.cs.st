'From Cuis 5.0 [latest update: #4307] on 27 July 2020 at 2:58:50 pm'!

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 7/27/2020 14:58:44'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass.
	PasteUpMorph allInstancesDo: [ :w | w clearCanvas ]! !

