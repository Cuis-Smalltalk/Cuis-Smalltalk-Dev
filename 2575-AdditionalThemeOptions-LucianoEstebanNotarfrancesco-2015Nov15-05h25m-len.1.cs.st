'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 15 November 2015 at 6:18:17.742576 am'!

!Theme methodsFor: 'colors' stamp: 'len 11/14/2015 02:34'!
scrollbarButtonColor
	^Color gray: 0.95! !

!Theme methodsFor: 'colors' stamp: 'len 11/14/2015 02:34'!
scrollbarSliderShadowColor
	^Color white! !

!Theme methodsFor: 'other options' stamp: 'len 11/15/2015 06:14'!
buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^Preferences standardButtonFont height * 14 // 8! !

!Theme methodsFor: 'other options' stamp: 'len 11/14/2015 03:38'!
layoutAdjusterThickness
	^ 3! !

!Theme methodsFor: 'other options' stamp: 'len 11/13/2015 18:36'!
minimalWindows
	^ false! !

!Theme methodsFor: 'other options' stamp: 'len 11/14/2015 04:02'!
scrollbarThickness
	^ Preferences scrollbarThickness! !

!Theme methodsFor: 'other options' stamp: 'len 11/13/2015 18:45'!
useTaskbar
	^true! !


!MenuMorph methodsFor: 'initialization' stamp: 'len 11/12/2015 20:16'!
defaultBorderWidth
	^ (Theme current roundWindowCorners or: [Theme current minimalWindows])
		ifTrue: [0]
		ifFalse: [Preferences menuBorderWidth]! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'len 11/12/2015 20:18'!
defaultBorderWidth
	^ Theme current minimalWindows ifTrue: [0] ifFalse: [1]! !


!PluggableScrollPane methodsFor: 'events' stamp: 'len 11/12/2015 20:28'!
scrollByKeyboard: event
	"If event is ctrl+up/down then scroll and answer true"
	event controlKeyPressed ifFalse: [ ^ false ].
	event keyValue = 30 ifTrue: [
		scrollBar scrollUp: 1.
		^ true ].
	event keyValue = 31 ifTrue: [
		scrollBar scrollDown: 1.
		^ true ].
	^ false.! !


!ScrollBar methodsFor: 'access' stamp: 'len 11/14/2015 02:35'!
color: aColor
	"Change the color of the scrollbar to go with aColor."
	| buttonColor |
	super color: aColor.
	buttonColor _ color alphaMixed: 0.7 with: Theme current scrollbarButtonColor.
	upButton color: buttonColor.
	downButton color: buttonColor.
	slider color: buttonColor slightlyLighter.
	sliderShadow color: (color alphaMixed: 0.45 with: Theme current scrollbarSliderShadowColor)! !

!ScrollBar methodsFor: 'geometry' stamp: 'len 11/14/2015 02:42'!
computeSlider

	| delta |
	delta _ (Theme current minimalWindows ifTrue: [0] ifFalse: [self buttonExtent]) + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth +  delta @ borderWidth ]
		ifFalse: [
			slider morphPosition: borderWidth @ (borderWidth + delta) ] ! !

!ScrollBar methodsFor: 'geometry' stamp: 'len 11/12/2015 03:43'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. substract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."

	| buttonsRoom |
	buttonsRoom _ Theme current minimalWindows ifTrue: [0] ifFalse: [self buttonExtent * 2].
	^ ((self isHorizontal
		ifTrue: [ extent x - slider morphWidth]
		ifFalse: [ extent y - slider morphHeight])
			- (borderWidth * 2) - buttonsRoom) max: 4! !

!ScrollBar methodsFor: 'initialization' stamp: 'len 11/12/2015 03:30'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	downButton morphExtent: e@e.
	Theme current minimalWindows ifTrue: [^ self].
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'len 11/12/2015 03:30'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	upButton morphExtent: e@e.
	Theme current minimalWindows ifTrue: [^ self].
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].
! !


!ScrollBar class methodsFor: 'constants' stamp: 'len 11/14/2015 04:03'!
scrollbarThickness

	^ Theme current scrollbarThickness! !


!SystemWindow methodsFor: 'GUI building' stamp: 'len 11/15/2015 06:15'!
defaultButtonPaneHeight
	"Answer the user's preferred default height for new button panes."

	^ Theme current buttonPaneHeight! !

!SystemWindow methodsFor: 'initialization' stamp: 'len 11/12/2015 02:44'!
boxExtent
	"answer the extent to use in all the buttons. 
	 
	the label height is used to be proportional to the fonts preferences"
	| e |
	Theme current minimalWindows ifTrue: [^0@0].
	e _ Preferences windowTitleFont height.
	^e@e! !

!SystemWindow methodsFor: 'initialization' stamp: 'len 11/12/2015 02:37'!
defaultBorderWidth
	"answer the default border width for the receiver"
	Theme current minimalWindows ifTrue: [^ 1].
	^ Theme current roundWindowCorners
		ifTrue: [ 3 ]
		ifFalse: [ 2 ]! !

!SystemWindow methodsFor: 'printing' stamp: 'len 11/12/2015 20:08'!
printOn: aStream 
	aStream nextPutAll: labelString asString! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 11/15/2015 06:10'!
buildMorphicClassColumn
	| column |
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: self buildMorphicClassList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: (Theme current minimalWindows ifTrue: [AbstractFont default height + 4] ifFalse: [AbstractFont default height *2-4]).
	^column! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'len 11/12/2015 20:05'!
drawOn: aCanvas

	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: color
		at: 0@0! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'len 11/14/2015 03:33'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: color! !


!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'len 11/14/2015 03:38'!
addAdjusterMorph
	"So the user can adjust layout"
	"twekear para ui grande..."

	| thickness |
	thickness _ Theme current layoutAdjusterThickness.

	direction == #horizontal ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSpec fixedWidth: thickness) ].

	direction == #vertical ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSpec fixedHeight: thickness)]! !


!Preferences class methodsFor: 'menu parameters' stamp: 'len 11/12/2015 20:15'!
menuBorderWidth

	^self parameters at: #menuBorderWidth ifAbsentPut: [ 1 ]! !

!Preferences class methodsFor: 'standard queries' stamp: 'len 11/12/2015 02:55'!
optionalButtons
	^ self
		valueOfFlag: #optionalButtons
		ifAbsent: [ false ].! !


!Theme class methodsFor: 'class initialization' stamp: 'len 11/13/2015 18:46'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		w hideTaskbar.
		CurrentTheme useTaskbar ifTrue: [w showTaskbar].
		w restoreMorphicDisplay ].
	
	^ CurrentTheme! !

