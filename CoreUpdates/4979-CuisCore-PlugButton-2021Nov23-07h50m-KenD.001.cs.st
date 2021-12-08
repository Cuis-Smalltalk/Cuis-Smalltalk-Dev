'From Cuis 5.0 [latest update: #4975] on 23 November 2021 at 7:57:47 am'!
!classDefinition: #PluggableButtonMorph category: 'Morphic-Widgets'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon getStateSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector stateSelector enableSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableButtonMorph commentStamp: '<historical>' prior: 0!
A PluggableButtonMorph is a combination of an indicator for a boolean value and an action button. The action of a button is often, but not always, to toggle the boolean value that it shows.  Models may range from simple to sophisticated.

Its pluggable selectors are:
	enableSelector a boolean value or a selector to query the model (Am I enabled, i.e. Can I be activated?)
	stateSelector		 fetch a boolean value from the model (Am I selected?)
	actionSelector			invoke this button's action on the model
	secondaryActionSelector invoke this action when the button-up event occures

Any of the above selectors can be nil, meaning that the model does not supply behavior for the given action, and the default behavior should be used. For example, if stateSelector is nil, then this button shows the state of a read-only boolean that is always false.  if enableSelector is nil, then this button is always enabled.

The model informs its view(s) of changes by sending #changed: to itself with stateSelector as a parameter. The view tells the model when the button is pressed by sending actionSelector.

If the actionSelector takes one or more arguments, then the following are relevant:
	arguments			          A list of arguments to provide when the actionSelector is called.
	argumentsProvider	 The object that is sent the argumentSelector to obtain arguments, if dynamic
	argumentsSelector	 The message sent to the argumentProvider to obtain the arguments.
		!


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'KenD 11/22/2021 13:21:46'!
enableColorWith: aColor
	^ self isEnabled 
		ifTrue: [aColor] ifFalse: [aColor darker darker "twiceLighter" ]! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'hlsf 11/5/2021 09:21:00'!
enableSelector: aSymbol
	enableSelector _ aSymbol ! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'KenD 11/22/2021 12:55:26'!
getEnabled
	
		^self isEnabled ! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'hlsf 11/5/2021 09:18:26'!
getState
	"Answer the result of sending the receiver's model the stateSelector message."

	^ stateSelector 
		ifNil: [false]
		ifNotNil: [model perform: stateSelector]! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'KenD 11/22/2021 13:33:45'!
isDisabled
	
	^ self isEnabled not! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'KenD 11/22/2021 12:53:52'!
isEnabled
	"My enableSelector may be nil, in which case I answer the default: true.
	My  enableSelector may be a selector, which I and to the model to answer.
	Then again, my enableSelector may be simply true or false.
	"

	enableSelector ifNil: [^true].
	^(enableSelector isKindOf: Boolean)
		ifTrue: [enableSelector] 
		ifFalse: [model perform: enableSelector ]! !

!PluggableButtonMorph methodsFor: 'enabling' stamp: 'KenD 11/22/2021 12:57:24'!
disable

	enableSelector := false.
	self update: #enableSelector! !

!PluggableButtonMorph methodsFor: 'enabling' stamp: 'KenD 11/22/2021 12:57:10'!
enable

	enableSelector := true.
	self update: #enableSelector! !

!PluggableButtonMorph methodsFor: '*morphic-misc1' stamp: 'KenD 12/23/2016 09:19:33'!
font

	^ self fontToUse! !

!PluggableButtonMorph methodsFor: '*morphic-misc1' stamp: 'KenD 12/23/2016 09:22:52'!
font: someFont

	PopUpMenu inform: 'You can''t set this'! !


!Switch methodsFor: 'state' stamp: 'hlsf 11/5/2021 09:51:46'!
turnOff
	"Set the state of the receiver to 'off'. If the state of the receiver was 
	previously 'on', then 'self change' is sent and the receiver's off action is 
	executed."

	self isOn
		ifTrue: 
			[on _ false.
			self changed: #isOn.
			self doAction: offAction]! !

!Switch methodsFor: 'state' stamp: 'hlsf 11/5/2021 09:51:57'!
turnOn
	"Set the state of the receiver to 'on'. If the state of the receiver was 
	previously 'off', then 'self change' is sent and the receiver's on action is 
	executed."

	self isOff
		ifTrue: 
			[on _ true.
			self changed: #isOn.
			self doAction: onAction]! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2012 18:24'!
actWhen: condition
	"Accepts symbols:  #buttonDown, #buttonStillDown or #buttonUp (default)"
	actWhen _ condition! !

!PluggableButtonMorph methodsFor: 'accessing'!
action: aSymbol 
	"Set actionSelector to be the action defined by aSymbol."

	actionSelector _ aSymbol.
! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'sw 12/28/2000 16:17'!
actionSelector
	"Answer the receiver's actionSelector"

	^ actionSelector! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 12/30/2011 19:41'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	self color: (Theme current buttonColorFrom: paneColor)! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 17:06:53'!
icon: aForm
	icon _ aForm.
	magnifiedIcon _ nil! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 10:42:16'!
iconDrawSelector
	"Must be understood by drawing canvas."

	^iconName! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 10:41:45'!
iconDrawSelector: aSymbol
	"aSymbol must be understood by drawing canvas."

	iconName _ aSymbol! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'KenD 9/17/2021 16:05:19'!
iconName
	
	^ self valueOfProperty: #iconName! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 17:30:25'!
iconName: name
	self setProperty: #iconName toValue: name! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 5/6/2009 08:18'!
label: aString
	"Label this button with the given string."

	self label: aString font: nil! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 4/24/2019 21:03:12'!
label: aStringOrNil font: aFontOrNil
	"Label this button with the given string."
	label _ aStringOrNil.
	font _ aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			extent := (self fontToUse widthOfString: label) + 10 @ (self fontToUse lineSpacing + 10) ]! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'KenD 11/22/2021 12:53:52'!
performAction
	"Inform the model that this button has been pressed. "
	self isEnabled ifFalse: [^ self].
	actionSelector ifNotNil: [
		model perform: actionSelector ]! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 5/24/2011 09:23'!
roundButtonStyle: aBooleanOrNil
	roundButtonStyle _ aBooleanOrNil! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 11/5/2021 09:54:03'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphLocalBounds
		color: (self enableColorWith: c)
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: (self enableColorWith: c).

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 11/5/2021 09:53:29'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ self enableColorWith: Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f lineSpacing // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 22:52:40'!
drawOn: aCanvas

	self isRoundButton
		ifTrue: [
			aCanvas drawButtonIconFromCurrentMorph ifFalse: [
				self drawRoundLookOn: aCanvas ]]
		ifFalse: [
			self draw3DLookOn: aCanvas.
			aCanvas drawButtonIconFromCurrentMorph ].! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 11/5/2021 09:53:29'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f lineSpacing // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: (self enableColorWith: Theme current buttonLabel) ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 11/5/2021 09:54:39'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton _ self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [ color ]]
		ifTrue: [ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		rect _ self morphLocalBounds insetBy: `3@3`.
		r _ r min: (rect width min: rect height) * 0.5.
		aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r ].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/6/2009 08:33'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [Preferences standardButtonFont].
	"
	Could add emphasis...
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]
	"
	^fontToUse! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'KenD 11/22/2021 13:30:36'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	self isEnabled ifFalse: [^self].  "guard condition"
	isPressed _ true.
	self redrawNeeded.
	(actWhen == #buttonDown or: [ actWhen == #buttonStillDown ])
		ifTrue: [
			self performAction ]
		ifFalse: [
			"Don't make multi-click slower if we act on button down, just do multiple actions"
			aMouseButtonEvent hand
				waitForClicksOrDragOrSimulatedMouseButton2: self
				event: aMouseButtonEvent
				clkSel: nil
				clkNHalf: nil
				dblClkSel: #doubleClick:localPosition:
				dblClkNHalfSel: nil
				tripleClkSel: nil ]! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 10/28/2021 12:02:45'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ 
		self includesPixel: aMouseButtonEvent eventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 5/24/2020 21:38:17'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition

	secondaryActionSelector ifNotNil: [
		model perform: secondaryActionSelector ].
	self redrawNeeded! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseEnter: event
	"The mouse entered the receiver"
	mouseIsOver _ true.
	self redrawNeeded.
	^super mouseEnter: event! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseLeave: event
	"The mouse has left the area of the receiver"
	mouseIsOver _ false.
	self redrawNeeded.
	^super mouseLeave: event! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 1/30/2014 00:25'!
mouseStillDown
	"Acting when down (instead of waiting until releasing the button)
	also means that the button actin is repeated if the button is kept pressed.
	See #handlesMouseStillDown:"
	self performAction! !

!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'KenD 11/22/2021 14:15:36'!
handlesMouseDown: evt
	"False if not enabled"
	
	^self isEnabled ! !

!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'KenD 11/22/2021 12:53:52'!
handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^self isEnabled ! !

!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'KenD 11/22/2021 12:53:52'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	"Acting when down (instead of waiting until releasing the button)
	also means that the button action is repeated if the button is kept pressed"
	^actWhen == #buttonStillDown and: [self isEnabled ]! !

!PluggableButtonMorph methodsFor: 'event handling' stamp: 'jmv 1/30/2014 00:27'!
mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?"
	^40! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'len 3/29/2021 10:36:35'!
defaultBorderWidth
	^ Theme current buttonBorderWidth! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'hlsf 11/5/2021 09:18:40'!
initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle _ nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model _ nil.
	enableSelector _ nil.
	stateSelector _ nil.
	actionSelector _ nil.
	isPressed _ false.
	mouseIsOver _ false.
	actWhen _ #buttonUp.
	extent _  `20 @ 15`! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'hlsf 11/5/2021 09:17:47'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeDependent: self].
	stateSelector ifNotNil: [
		anObject ifNotNil: [anObject addDependent: self]].
	model _ anObject! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'hlsf 11/5/2021 09:17:47'!
model: anObject stateGetter: getStateSel action: actionSel label: aString

	stateSelector _ getStateSel.
	actionSelector _ actionSel.
	self model: anObject.
	self label: aString! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 5/24/2020 21:36:57'!
secondaryActionSelector: actionSel

	secondaryActionSelector _ actionSel.! !

!PluggableButtonMorph methodsFor: 'updating' stamp: 'hlsf 11/5/2021 09:50:54'!
update: aSymbol
	super update: aSymbol.
	(aSymbol == stateSelector or: [aSymbol == enableSelector ]) ifTrue: [
		self redrawNeeded ]! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 7/19/2021 17:06:35'!
magnifiedIcon
	| factor magnifiedExtent w h |

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

!PluggableButtonMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:33'!
is: aSymbol
	^ aSymbol == #PluggableButtonMorph or: [ super is: aSymbol ]! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'hlsf 11/5/2021 09:18:26'!
isPressed
	^isPressed | self getState! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'jmv 5/24/2011 09:24'!
isRoundButton
	^roundButtonStyle ifNil: [ Theme current roundButtons ]! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'KenD 11/22/2021 12:53:52'!
mouseIsOver
	^mouseIsOver and: [self isEnabled ]! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:34:23'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	magnifiedIcon _ nil! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'KenD 11/22/2021 14:51:15'!
minimumExtent
	
	| unit |
	unit :=  Preferences windowTitleFont pointSize.
	^label 
		ifNil: [ (unit+2) @ (unit+2) ] "Assume title button"
		ifNotNil: [ (4 * unit) @ (3 * unit) ] "Assure some space for text."
! !

!PluggableButtonMorph methodsFor: '*morphic-misc1' stamp: 'KenD 12/24/2016 18:01:06'!
label

	^ label! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'hlsf 11/5/2021 09:51:34'!
example
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 _ Switch new.
	s2 _ Switch new turnOn.
	s3 _ Switch new.
	s1 onAction: [s3 turnOn].
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 _ (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 _ (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 _ (PluggableButtonMorph model: s3 stateGetter: nil action: #switch) 
		enableSelector: #isOn;
		label: 'S3'.
	b1 color: `Color lightRed`.
	b2 color: `Color lightRed`.
	b3 color: `Color lightRed`.
	row _ LayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: `120@35`.
	^ row! !

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 3/1/2011 15:00'!
model: anObject action: actionSel

	^ self new
		model: anObject
		stateGetter: nil
		action: actionSel
		label: nil
! !

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 3/16/2012 17:48'!
model: anObject action: actionSel label: aString

	^ self new
		model: anObject
		stateGetter: nil
		action: actionSel
		label: aString! !

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 3/1/2011 11:11'!
model: anObject stateGetter: getStateSel action: actionSel

	^ self new
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: nil
! !

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 3/1/2011 11:10'!
model: anObject stateGetter: getStateSel action: actionSel label: aString
	^ self new
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: aString! !

!classDefinition: #PluggableButtonMorph category: 'Morphic-Widgets'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon stateSelector enableSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: icon: iconDrawSelector iconDrawSelector: iconName iconName: label: label:font: performAction roundButtonStyle:)
('drawing' draw3DLookOn: drawEmbossedLabelOn: drawOn: drawRegularLabelOn: drawRoundLookOn: enableColorWith: fontToUse)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition: mouseEnter: mouseLeave: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' defaultBorderWidth enableSelector: initialize model: model:stateGetter:action:label: secondaryActionSelector:)
('updating' update:)
('private' getEnabled getModelState getState magnifiedIcon)
('testing' is: isDisabled isEnabled isPressed isRoundButton mouseIsOver)
('geometry' extentChanged: minimumExtent)
('geometry testing')
('scrollbar button')
('enabling' disable enable)
!

