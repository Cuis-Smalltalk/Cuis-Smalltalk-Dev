'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 22 February 2015 at 9:50:47.657519 pm'!
"Change Set:		2170-CuisCore-DanNorton-2015Feb22-21h35m
Date:			22 February 2015
Author:			Dan Norton

Make it possible to name, for example, a LayoutMorph. This makes it easier to distinguish morphs in a complex layout. Currently only morphs with a labelString, such as SystemWindow, are identified when the third button is used."!

!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'extent color name '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'dhn 2/22/2015 21:37'!
morphName: anObject

"Set the receiver's name"
	name _ anObject! !

!RectangleLikeMorph methodsFor: 'as yet unclassified' stamp: 'dhn 2/22/2015 21:38'!
printOn: aStream 

	super printOn: aStream.
	
	name ifNotNil: [aStream print: name asString]! !

!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'extent color name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
