'From Cuis7.3 [latest update: #6944] on 23 December 2024 at 10:24:59 am'!

!Morph methodsFor: 'events-processing' stamp: 'JEC 12/23/2024 09:53:40'!
processDropMorph: aDropEvent
	"Handle a dropping morph."
	| aMorph |
	
	aDropEvent wasHandled ifTrue: [ ^self ]. "Do it just once, for one drop destination"
	
	aMorph := aDropEvent contents.
	aDropEvent wasHandled: true.
	aMorph justDroppedInto: self event: aDropEvent.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	! !

