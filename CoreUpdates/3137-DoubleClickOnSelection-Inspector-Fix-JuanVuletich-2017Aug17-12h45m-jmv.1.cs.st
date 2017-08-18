'From Cuis 5.0 of 7 November 2016 [latest update: #3134] on 17 August 2017 at 1:06:03 pm'!

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/17/2017 13:05:27'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index _ self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self selectionIndex
		ifFalse: [ self changeModelSelection: index ].
	^ self model perform: doubleClickSelector! !

