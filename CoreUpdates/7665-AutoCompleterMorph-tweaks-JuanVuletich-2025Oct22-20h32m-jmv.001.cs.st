'From Cuis7.5 [latest update: #7664] on 22 October 2025 at 8:35:34 pm'!

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:31'!
fullScreenDeskMargin
	^ (Preferences at: #fullScreenLeavesDeskMargins) ifTrue: [48] ifFalse: [0]! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 10/22/2025 20:30:23'!
resetMenu

	| width newExtent p |
	self initMenu.
	width := self calculateWidth.
	newExtent := width + 4 @ (self itemsPerPage * self itemHeight + 2).
	self extentInOwner: newExtent.
	p := self morphPosition.
	self fitInOwner.
	self morphPosition y < p y ifTrue: [
		self morphPosition: p - (0 @ (self morphHeight + self itemHeight)).
		self fitInOwner ].
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded.! !

