'From Cuis7.3 [latest update: #7158] on 16 September 2025 at 7:42:21 pm'!

!AutoCompleterMorph methodsFor: 'events' stamp: 'nh 9/16/2025 19:40:24'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	self stillActive .
	aMouseEvent direction = #up ifTrue: [^self goUp] .
	aMouseEvent direction = #down ifTrue: [^self goDown ] .! !

!AutoCompleterMorph methodsFor: 'event handling testing' stamp: 'nh 9/9/2025 21:53:31'!
handlesMouseScroll: aMouseScrollEvent
	^true! !

