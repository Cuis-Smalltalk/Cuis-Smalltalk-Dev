'From Cuis 6.0 [latest update: #5102] on 8 April 2022 at 10:02:13 am'!
!classDefinition: #PluggableButtonMorph category: 'Morphic-Widgets'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon stateSelector enableSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector stateArgument actionArgument mouseOverColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 4/8/2022 09:53:39'!
magnifiedIcon: aForm
	"Meaning: Don't magnify aForm to adapt it to our extent."

	icon _ nil.
	magnifiedIcon _ aForm! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 4/8/2022 09:54:21'!
mouseOverColor: aColor
	"If set, ignore #adoptWidgetsColor: for mouse over!!"

	mouseOverColor _ aColor! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 4/8/2022 09:55:29'!
actionArgument: actionArg

	actionArgument _ actionArg.! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 4/4/2022 14:20:33'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ nil! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 4/8/2022 09:56:23'!
model: anObject stateGetter: getStateSel stateArgument: getStateArg action: actionSel actionArgument: actionArg label: aString

	stateSelector _ getStateSel.
	stateArgument _ getStateArg.
	actionSelector _ actionSel.
	actionArgument _ actionArg.
	self model: anObject.
	self label: aString! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 4/4/2022 14:40:45'!
stateArgument: getStateArg

	stateArgument _ getStateArg.! !


!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 4/8/2022 09:56:47'!
model: anObject stateGetter: getStateSel stateArgument: getStateArg action: actionSel actionArgument: actionArg label: aString
	^ self new
		model: anObject
		stateGetter: getStateSel
		stateArgument: getStateArg
		action: actionSel
		actionArgument: actionArg
		label: aString! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 4/4/2022 15:09:55'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	mouseOverColor ifNil: [
		self color: (Theme current buttonColorFrom: paneColor) ]! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 4/8/2022 09:59:49'!
performAction
	"Inform the model that this button has been pressed. "
	self isEnabled ifFalse: [^ self].
	actionSelector ifNotNil: [
		actionArgument
			ifNil: [ model perform: actionSelector ]
			ifNotNil: [ model perform: actionSelector with: actionArgument ]].! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/4/2022 15:04:03'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver | self isPressed ifTrue: [
		c _ mouseOverColor ifNil: [ c lighter ]].
	c _ self enableColorWith: c.
	aCanvas
		fillRectangle: self morphLocalBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: (borderColor ifNil: [ c ]).

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/4/2022 15:08:49'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton _ self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 mouseOverColor ifNil: [ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]]
				ifFalse: [ color ]]
		ifTrue: [ mouseOverColor ifNil: [ color adjustSaturation: 0.1 brightness: -0.1 ]].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		rect _ self morphLocalBounds insetBy: `3@3`.
		r _ r min: (rect width min: rect height) * 0.5.
		aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r ].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 4/8/2022 10:01:16'!
initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle _ nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model _ nil.
	enableSelector _ nil.
	stateSelector _ nil.
	stateArgument _ nil.
	actionSelector _ nil.
	actionArgument _ nil.
	isPressed _ false.
	mouseIsOver _ false.
	mouseOverColor _ nil.
	actWhen _ #buttonUp.
	extent _  `20 @ 15`! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 4/8/2022 10:01:37'!
model: anObject stateGetter: getStateSel action: actionSel label: aString

	stateSelector _ getStateSel.
	stateArgument _ nil.
	actionSelector _ actionSel.
	actionArgument _ nil.
	self model: anObject.
	self label: aString! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 4/8/2022 09:59:44'!
getState
	"Answer the result of sending the receiver's model the stateSelector message."

	^ stateSelector 
		ifNil: [false]
		ifNotNil: [
			stateArgument
				ifNil: [	model perform: stateSelector]
				ifNotNil: [	model perform: stateSelector with: stateArgument]].! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 4/4/2022 14:52:00'!
magnifiedIcon
	| factor magnifiedExtent w h |

	magnifiedIcon ifNotNil: [ ^magnifiedIcon ].
	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		w _ icon width.
		h _ icon height.
		w*h = 0 ifFalse: [
			factor _ 1.0 * extent x / w min: 1.0 * extent y / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (icon extent * factor) rounded.
				magnifiedIcon _ icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 4/4/2022 14:52:16'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	icon ifNotNil: [
		magnifiedIcon _ nil ]! !

!methodRemoval: PluggableButtonMorph class #model:stateGetter:stateArgument:action:label: stamp: 'jmv 4/8/2022 09:57:46'!
PluggableButtonMorph class removeSelector: #model:stateGetter:stateArgument:action:label:!
!methodRemoval: PluggableButtonMorph #model:stateGetter:stateArgument:action:label: stamp: 'jmv 4/8/2022 09:57:46'!
PluggableButtonMorph removeSelector: #model:stateGetter:stateArgument:action:label:!
!classDefinition: #PluggableButtonMorph category: 'Morphic-Widgets'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon stateSelector enableSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector stateArgument actionArgument mouseOverColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
