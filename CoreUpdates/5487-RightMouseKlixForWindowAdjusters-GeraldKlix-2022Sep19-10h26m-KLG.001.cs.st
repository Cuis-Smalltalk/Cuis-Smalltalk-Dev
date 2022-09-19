'From Haver 6.0 [latest update: #5485] on 19 September 2022 at 11:35:10 am'!

!WindowEdgeAdjustingMorph methodsFor: 'events' stamp: 'KLG 9/19/2022 11:30:07'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Some nice window expansions."
	
	self owner ifNotNil: [ :o |
		o perform: ((selector copyUpToLast: $:) copyReplaceAll: 'window' with: 'resize') asSymbol ]! !

