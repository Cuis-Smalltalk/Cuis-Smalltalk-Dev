'From Cuis 4.2 of 25 July 2013 [latest update: #1949] on 2 February 2014 at 10:43:09.347348 am'!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
RectangleLikeMorph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector keyStrokeSelector '
	classVariableNames: 'CircleForm '
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!methodRemoval: HaloHandleMorph #handlesMouseStillDown:!
HaloHandleMorph removeSelector: #handlesMouseStillDown:!
!methodRemoval: HaloHandleMorph #keyStrokeSelector:!
HaloHandleMorph removeSelector: #keyStrokeSelector:!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
RectangleLikeMorph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector'
	classVariableNames: 'CircleForm'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
