'From Cuis 5.0 [latest update: #4995] on 2 December 2021 at 5:36:03 pm'!

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/2/2021 17:15:54'!
computeSlider

	| buttonRoom delta |
	buttonRoom _ upButton visible ifTrue: [self buttonExtent] ifFalse: [0].
	delta _ buttonRoom + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth +  delta @ borderWidth ]
		ifFalse: [
			slider morphPosition: borderWidth @ (borderWidth + delta) ] ! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/2/2021 17:29:30'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."
	| isH wasH b p |

	super extentChanged: oldExtent.
	"Doesn't move!!"
	"upButton morphPosition: borderWidth@borderWidth."
	p _ extent - borderWidth - downButton morphExtent.
	downButton morphPosition: p.
	b _ Theme current scrollbarShowButtons and: [(p - borderWidth) rho > (self buttonExtent * 4)].
	upButton visible: b.
	downButton visible: b.
	wasH _ oldExtent notNil and: [oldExtent x > oldExtent y].
	isH _ self isHorizontal.
	isH = wasH ifFalse: [
		isH
			ifTrue: [
				self updateLeftButton: upButton.
				self updateRightButton: downButton ]
			ifFalse: [
				self updateUpButton: upButton.
				self updateDownButton: downButton ]].! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/2/2021 17:15:26'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. subtract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."
	| buttonsRoom |
	buttonsRoom _ upButton visible ifTrue: [self buttonExtent * 2] ifFalse: [0].
	^ ((self isHorizontal
		ifTrue: [extent x - slider morphWidth]
		ifFalse: [extent y - slider morphHeight])
			- (borderWidth * 2) - buttonsRoom) max: 4! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/2/2021 17:17:40'!
totalSliderRoom
	"Answer the length or height of the slider area"

	| buttonsRoom |
	buttonsRoom _ upButton visible ifTrue: [self buttonExtent * 2] ifFalse: [0].
	^ (self isHorizontal
		ifTrue: [ extent x ]
		ifFalse: [ extent y ])
			- (borderWidth * 2) - buttonsRoom.! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 12/2/2021 17:27:41'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ PluggableButtonMorph new.
	downButton model: self.
	downButton morphExtent: e@e.
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 12/2/2021 17:27:45'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ PluggableButtonMorph new.
	upButton model: self.
	upButton morphExtent: e@e.
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 12/2/2021 17:16:08'!
scrollTo: handPositionRelativeToSlider
	| v handPositionRelativeToUs buttonRoom |
	grabPosition ifNotNil: [
		buttonRoom _ upButton visible ifTrue: [self buttonExtent] ifFalse: [0].
		handPositionRelativeToUs _ slider externalize: handPositionRelativeToSlider.
		v _ (self isHorizontal
			ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
			ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
				- borderWidth - buttonRoom * 1.0
					/ self freeSliderRoom.
		self internalScrollValue: v ]! !

