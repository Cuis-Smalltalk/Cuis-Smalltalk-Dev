'From Cuis 5.0 [latest update: #4536] on 17 March 2021 at 9:53:17 am'!

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 3/17/2021 09:53:05'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	dragOnOrOff _ false.  "So improperly started drags will have not effect"
	dragStartRow _ nil! !

