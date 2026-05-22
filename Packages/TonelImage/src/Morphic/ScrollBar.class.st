"
A ScrollBar for general use in Morphic.
"
Class {
	#name : 'ScrollBar',
	#superclass : 'PluggableMorph',
	#instVars : [
		'slider',
		'value',
		'setValueSelector',
		'sliderShadow',
		'upButton',
		'downButton',
		'scrollDelta',
		'pageDelta',
		'interval',
		'nextPageDirection',
		'grabPosition'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'accessing' }
ScrollBar >> adoptWidgetsColor: aColor [
	"Adopt the given pane color"
	self color: aColor.
]

{ #category : 'geometry' }
ScrollBar >> buttonExtentToUse [
	"Answer zero if buttons are not visible.
	Answer zero if no button is to be drawn due to non-standard thickness."

	| desired possible |
	upButton visible ifFalse: [ ^0 ].
	desired := self thickness. "To suit us"
	possible := Theme current scrollbarThickness. "To be drawn by any canvas"
	^desired = possible ifTrue: [desired] ifFalse: [0]
]

{ #category : 'access' }
ScrollBar >> color: aColor [
	"Change the color of the scrollbar to go with aColor."
	| buttonColor sliderColor |
	super color: aColor.

	buttonColor := Color transparent.
	upButton color: buttonColor.
	downButton color: buttonColor.

	sliderColor := color alphaMixed: 0.7 with: Theme current scrollbarButtonColor.
	slider color: sliderColor slightlyLighter.

	sliderShadow color: (color alphaMixed: 0.45 with: Theme current scrollbarSliderShadowColor)
]

{ #category : 'geometry' }
ScrollBar >> computeSlider [

	| delta |
	delta := self buttonExtentToUse + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth + delta @ (borderWidth + self sliderSlack) ]
		ifFalse: [
			slider morphPosition: (borderWidth + self sliderSlack) @ (borderWidth + delta) ] .
]

{ #category : 'drawing' }
ScrollBar >> drawOn: aCanvas [

	aCanvas
		fillRectangle: self localBounds
		color: (color alphaMixed: 0.3 with: Theme current scrollbarColor)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor
]

{ #category : 'geometry' }
ScrollBar >> expandSlider [
	"Compute the new size of the slider."

	| e |
	e := (self totalSliderRoom * interval) asInteger max: 7.
	slider morphExtent: (self isHorizontal
		ifTrue: [ e @ (self thickness-(self sliderSlack*2)) ]
		ifFalse: [ self thickness-(self sliderSlack*2) @ e ])
]

{ #category : 'geometry' }
ScrollBar >> extentChanged: oldExtent [
	"Our extent changed. Must layout submorphs again."
	| isH wasH p |

	super extentChanged: oldExtent.
	"Doesn't move!"
	"upButton morphPosition: borderWidth@borderWidth."
	p := extent - borderWidth - downButton morphExtent.
	downButton morphPosition: p.
	self hideOrShowButtons.
	wasH := oldExtent notNil and: [oldExtent x > oldExtent y].
	isH := self isHorizontal.
	isH = wasH ifFalse: [
		isH
			ifTrue: [
				self updateLeftButton: upButton.
				self updateRightButton: downButton ]
			ifFalse: [
				self updateUpButton: upButton.
				self updateDownButton: downButton ]].
]

{ #category : 'geometry' }
ScrollBar >> freeSliderRoom [
	"Answer the length or height of the free slider area, i.e. subtract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."

	^ ((self isHorizontal
		ifTrue: [extent x - slider morphWidth]
		ifFalse: [extent y - slider morphHeight])
			- (borderWidth * 2) - (self buttonExtentToUse*2)) max: 4
]

{ #category : 'event handling testing' }
ScrollBar >> handlesMouseDown: aMouseButtonEvent [
	"Do I want to receive mouseDown events (mouseDown:, mouseUp:)?"
	^true
]

{ #category : 'event handling testing' }
ScrollBar >> handlesMouseStillDown: evt [
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	^true
]

{ #category : 'geometry' }
ScrollBar >> hideOrShowButtons [
	| b |
	b := self showsButtons and: [
		self thickness = Theme current scrollbarThickness and: [
			extent rho > (self thickness * 4)]].
	upButton visible: b.
	downButton visible: b.
	self expandSlider.
	self computeSlider.
]

{ #category : 'initialization' }
ScrollBar >> initialize [
	super initialize.
	extent := Theme current scrollbarThickness @ 100.
	value := 0.0.
	self recreateSubmorphs.
	scrollDelta := 0.02.
	pageDelta := 0.2.
	interval := 1.0.
]

{ #category : 'initialization' }
ScrollBar >> initializeDownButton [
	"Initialize the receiver's downButton"

	| e |
	e := Theme current scrollbarThickness. "To be drawn by any canvas"
	downButton := PluggableButtonMorph new.
	downButton actionTarget: self.
	downButton morphExtent: e@e.
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ].
	downButton visible: self showsButtons.
]

{ #category : 'initialization' }
ScrollBar >> initializeSlider [
	"initialize the receiver's slider"

	sliderShadow := ColoredBoxMorph new.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider := DraggableButtonMorph new.
	slider actionTarget: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider
]

{ #category : 'initialization' }
ScrollBar >> initializeUpButton [
	"Initialize the receiver's upButton"

	| e |
	e := Theme current scrollbarThickness. "To be drawn by any canvas"
	upButton := PluggableButtonMorph new.
	upButton actionTarget: self.
	upButton morphExtent: e@e.
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
	upButton visible: self showsButtons.
]

{ #category : 'model access' }
ScrollBar >> internalScrollValue: newValue [
	"Called internally for propagation to model"
	self scrollValue: newValue.
	setValueSelector ifNotNil: [
		model perform: setValueSelector with: value ]
]

{ #category : 'access' }
ScrollBar >> interval: d [
	"Supply an optional floating fraction so slider can expand to indicate range"
	interval := d min: 1.0.
	self expandSlider.
	self computeSlider.
]

{ #category : 'testing' }
ScrollBar >> isHorizontal [

	^extent x > extent y
]

{ #category : 'geometry' }
ScrollBar >> minimumExtent [
	^6@6
]

{ #category : 'accessing' }
ScrollBar >> model: thang setValueSelector: aSymbol [
	model := thang.
	setValueSelector := aSymbol
]

{ #category : 'events' }
ScrollBar >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Update visual feedback"

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self setNextDirectionFromEvent: aMouseButtonEvent.
	self scrollByPage
]

{ #category : 'events' }
ScrollBar >> mouseStillDown [

	nextPageDirection notNil ifTrue: [
		self scrollByPage ]
]

{ #category : 'initialization' }
ScrollBar >> recreateSubmorphs [
	self removeAllMorphs.
	self
		initializeUpButton;
		initializeDownButton;
		initializeSlider.
	"Set color for submorphs"
	self color: color.
]

{ #category : 'scrolling' }
ScrollBar >> scrollByPage [
	"Scroll automatically while mouse is down"
	nextPageDirection
		ifTrue: [self internalScrollValue: (value + pageDelta min: 1.0)]
		ifFalse: [self internalScrollValue: (value - pageDelta max: 0.0)]

]

{ #category : 'access' }
ScrollBar >> scrollDelta: d1 pageDelta: d2 [
	"Supply optional increments for better scrolling of, eg, text"
	scrollDelta := d1.
	pageDelta := d2.
]

{ #category : 'scrolling' }
ScrollBar >> scrollDown [
	self scrollDown: 1
]

{ #category : 'scrolling' }
ScrollBar >> scrollDown: count [
	self internalScrollValue: (value + (scrollDelta * count) + 0.000001 min: 1.0)
]

{ #category : 'scrolling' }
ScrollBar >> scrollTo: handPositionRelativeToSlider [
	| v handPositionRelativeToUs |
	grabPosition ifNotNil: [
		handPositionRelativeToUs := slider externalize: handPositionRelativeToSlider.
		v := (self isHorizontal
			ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
			ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
				- borderWidth - self buttonExtentToUse * 1.0
					/ self freeSliderRoom.
		self internalScrollValue: v ]
]

{ #category : 'scrolling' }
ScrollBar >> scrollUp [
	self scrollUp: 1
]

{ #category : 'scrolling' }
ScrollBar >> scrollUp: count [
	self internalScrollValue: (value - (scrollDelta * count) - 0.000001 max: 0.0)
]

{ #category : 'access' }
ScrollBar >> scrollValue [
	^ value
]

{ #category : 'model access' }
ScrollBar >> scrollValue: newValue [
	"Drive the slider position externally..."
	value := newValue min: 1.0 max: 0.0.
	self computeSlider
]

{ #category : 'scrolling' }
ScrollBar >> setNextDirectionFromEvent: event [

	nextPageDirection := self isHorizontal
		ifTrue: [ event eventPosition x >= slider referencePosition x ]
		ifFalse: [ event eventPosition y >= slider referencePosition y ]
]

{ #category : 'options' }
ScrollBar >> showButtons: aBoolean [
	"Default is `Theme current scrollbarShowButtons`. Can be overriden for true or false.
	Buttons will only be shown if there's enough room for a reasonable slider size + buttons."

	self setProperty: #showButtons toValue: aBoolean.
	self hideOrShowButtons.
]

{ #category : 'options' }
ScrollBar >> showsButtons [
	"Default is `Theme current scrollbarShowButtons`. Can be overriden for true or false.
	Buttons will only be shown if there's enough room for a reasonable slider size + buttons."

	^ self valueOfProperty: #showButtons ifAbsent: [ Theme current scrollbarShowButtons ]
]

{ #category : 'scrolling' }
ScrollBar >> sliderGrabbedAt: handPositionRelativeToSlider [

	| e p |
	grabPosition := handPositionRelativeToSlider.
	self isHorizontal
		ifTrue: [
			e := slider morphWidth @ self thickness.
			p := slider morphPosition x @ borderWidth ]
		ifFalse: [
			e := self thickness @ slider morphHeight.
			p := borderWidth @ slider morphPosition y ].
	sliderShadow
		position: p extent: e;
		show
]

{ #category : 'scrolling' }
ScrollBar >> sliderReleased [

	grabPosition := nil.
	sliderShadow hide
]

{ #category : 'geometry' }
ScrollBar >> sliderSlack [
	^self thickness * self sliderSlackFactor
]

{ #category : 'options' }
ScrollBar >> sliderSlackFactor [

	^ self valueOfProperty: #sliderSlackFactor ifAbsent: [ `1.0 / 6.0` ]
]

{ #category : 'options' }
ScrollBar >> sliderSlackFactor: aNumber [

	self setProperty: #sliderSlackFactor toValue: aNumber.
	self hideOrShowButtons.
]

{ #category : 'geometry' }
ScrollBar >> thickness [
	"width for vertical scrollbars, height for horizontal scrollbars"
	^extent x min: extent y
]

{ #category : 'geometry' }
ScrollBar >> totalSliderRoom [
	"Answer the length or height of the slider area"

	^ (self isHorizontal
		ifTrue: [ extent x ]
		ifFalse: [ extent y ])
			- (borderWidth * 2) - (self buttonExtentToUse * 2).
]

{ #category : 'initialization' }
ScrollBar >> updateDownButton: aPluggableButtonMorph [
	"Update the argument as a downButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawDownIcon;
		action: #scrollDown;
		buttonStyle: #rectangular;
		redrawNeeded
]

{ #category : 'initialization' }
ScrollBar >> updateLeftButton: aPluggableButtonMorph [
	"Update the argument as a leftButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawLeftIcon;
		action: #scrollUp;
		buttonStyle: #rectangular;
		redrawNeeded
]

{ #category : 'initialization' }
ScrollBar >> updateRightButton: aPluggableButtonMorph [
	"Update the argument as a rightButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawRightIcon;
		action: #scrollDown;
		buttonStyle: #rectangular;
		redrawNeeded
]

{ #category : 'initialization' }
ScrollBar >> updateUpButton: aPluggableButtonMorph [
	"Update the argument as an upButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawUpIcon;
		action: #scrollUp;
		buttonStyle: #rectangular;
		redrawNeeded
]
