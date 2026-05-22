"
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
		
"
Class {
	#name : 'PluggableButtonMorph',
	#superclass : 'PluggableMorph',
	#instVars : [
		'label',
		'font',
		'icon',
		'stateSelector',
		'enableSelector',
		'actionSelector',
		'isPressed',
		'mouseIsOver',
		'magnifiedIcon',
		'actWhen',
		'buttonStyle',
		'iconName',
		'secondaryActionSelector',
		'stateArgument',
		'actionArgument',
		'mouseOverColor'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
PluggableButtonMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'example' }
PluggableButtonMorph class >> example [
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 := Switch new.
	s2 := Switch new turnOn.
	s3 := Switch new.
	s1 onAction: [s3 turnOn].
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 := (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 := (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 := (PluggableButtonMorph model: s3 stateGetter: nil action: #switch) 
		enableSelector: #isOn;
		label: 'S3'.
	b1 color: `Color lightRed`.
	b2 color: `Color lightRed`.
	b3 color: `Color lightRed`.
	row := LinearLayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: `120@35`.
	^ row
]

{ #category : 'new-morph participation' }
PluggableButtonMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"
	
	^ true
]

{ #category : 'new-morph participation' }
PluggableButtonMorph class >> initializedInstance [

	^ self example
]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject action: actionSel [

	^ self new
		model: anObject
		stateGetter: nil
		action: actionSel
		label: nil

]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject action: actionSel actionArgument: actionArg [

	^ self new
		model: anObject
		stateGetter: nil
		stateArgument: nil
		action: actionSel
		actionArgument: actionArg
		label: nil

]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject action: actionSel actionArgument: actionArg label: aString [

	^ self new
		model: anObject
		stateGetter: nil
		stateArgument: nil
		action: actionSel
		actionArgument: actionArg
		label: aString

]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject action: actionSel label: aString [

	^ self new
		model: anObject
		stateGetter: nil
		action: actionSel
		label: aString
]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject stateGetter: getStateSel action: actionSel [

	^ self new
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: nil

]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject stateGetter: getStateSel action: actionSel label: aString [
	^ self new
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: aString
]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject stateGetter: getStateSel action: actionSel label: aString balloonText: aBallonText [
	^ (self 
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: aString)
		setBalloonText: aBallonText;
		yourself
]

{ #category : 'instance creation' }
PluggableButtonMorph class >> model: anObject stateGetter: getStateSel stateArgument: getStateArg action: actionSel actionArgument: actionArg label: aString [
	^ self new
		model: anObject
		stateGetter: getStateSel
		stateArgument: getStateArg
		action: actionSel
		actionArgument: actionArg
		label: aString
]

{ #category : 'accessing' }
PluggableButtonMorph >> actWhen: condition [
	"Accepts symbols:  #buttonDown, #buttonStillDown or #buttonUp (default)"
	actWhen := condition
]

{ #category : 'accessing' }
PluggableButtonMorph >> action: aSymbol  [
	"Set actionSelector to be the action defined by aSymbol."

	actionSelector := aSymbol.

]

{ #category : 'initialization' }
PluggableButtonMorph >> actionArgument: actionArg [

	actionArgument := actionArg.
]

{ #category : 'private' }
PluggableButtonMorph >> actionBlock [

	actionArgument
		ifNil: [ ^ [ model perform: actionSelector ]]
		ifNotNil: [ ^ [ model perform: actionSelector with: actionArgument ]]
]

{ #category : 'accessing' }
PluggableButtonMorph >> actionSelector [
	"Answer the receiver's actionSelector"

	^ actionSelector
]

{ #category : 'initialization' }
PluggableButtonMorph >> actionTarget: anObject [
	"Set my model.
	We will not observe the model. We will not receive #update: or other notifications from them.
	Call #model: if observing the model for updates is required."

	model ifNotNil: [model removeActionsWithReceiver: self].
	model := anObject.
]

{ #category : 'accessing' }
PluggableButtonMorph >> adoptWidgetsColor: paneColor [
	super adoptWidgetsColor: paneColor.
	mouseOverColor ifNil: [
		self color: (Theme current buttonColorFrom: paneColor) ]
]

{ #category : 'accessing' }
PluggableButtonMorph >> borderWidth: anInteger [
	super borderWidth: anInteger.
	magnifiedIcon := nil.
	self redrawNeeded 
]

{ #category : 'debug and other' }
PluggableButtonMorph >> buildDebugMenu: aHand  [
	
	| aMenu |
	aMenu := super buildDebugMenu: aHand.
	
	aMenu addLine.
	aMenu add: 'debug action' action: #debugAction.
	
	^aMenu
]

{ #category : 'accessing' }
PluggableButtonMorph >> buttonStyle [
	^ buttonStyle 
]

{ #category : 'accessing' }
PluggableButtonMorph >> buttonStyle: aSymbol [
" 
	#none 			Draw only the icon and or label, with no button shape
	#theme 		Honor theme
	#round 			Draw as round button
	#rectangular 	Draw as classic 3d border rectangular button
"

	buttonStyle := aSymbol 
]

{ #category : 'debug and other' }
PluggableButtonMorph >> debugAction [
	| debugger |
	(self isEnabled or: [ PopUpMenu confirm: 'The button is disabled, do you want to continue?' ])
		ifFalse: [ ^ self ].
	
	debugger := Debugger
		openDebugging: self actionBlock
		label: 'Debugging action #', actionSelector.
	
	[debugger interruptedContext receiver == model]
		whileFalse: [debugger send].
]

{ #category : 'initialization' }
PluggableButtonMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ nil
]

{ #category : 'initialization' }
PluggableButtonMorph >> defaultBorderWidth [
	^ Theme current buttonBorderWidth
]

{ #category : 'initialization' }
PluggableButtonMorph >> defaultColor [
	^Theme current defaultButtonColor
]

{ #category : 'enabling' }
PluggableButtonMorph >> disable [

	enableSelector := false.
	self redrawNeeded.
]

{ #category : 'drawing' }
PluggableButtonMorph >> draw3DLookOn: aCanvas [

	| borderStyleSymbol c |
	borderStyleSymbol := self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c := color.
	(self mouseIsOver or: [ self isPressed ]) ifTrue: [
		c := mouseOverColor ifNil: [ c lighter ]].
	c := self enableColorWith: c.
	aCanvas
		fillRectangle: self localBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: (borderColor ifNil: [ c ]).

	self drawRegularLabelOn: aCanvas
]

{ #category : 'drawing' }
PluggableButtonMorph >> drawEmbossedLabelOn: aCanvas [

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel := self enableLabelColorWith: Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel := colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel := colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f := self fontToUse.
		center := extent // 2.
		labelMargin := 3.
		w := f widthOfString: label.
		availableW := extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l := label ]
			ifFalse: [
				x := labelMargin.
				targetSize := label size * availableW // w.
				l := label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize := targetSize - 1.
					l := label squeezedTo: targetSize ]].
		
		w := f widthOfString: l.
		x := center x - (w // 2).
		y := center y - (f lineSpacing // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]
]

{ #category : 'drawing' }
PluggableButtonMorph >> drawOn: aCanvas [
	buttonStyle == #none ifFalse: [
		self isRound 
			ifTrue:  [ self drawRoundLookOn: aCanvas ]
			ifFalse: [ self draw3DLookOn: aCanvas ]].

	aCanvas drawButtonIconFromCurrentMorph.
]

{ #category : 'drawing' }
PluggableButtonMorph >> drawRegularLabelOn: aCanvas [

	| w f center x y  availableW l labelMargin |

	f := self fontToUse.
	center := extent // 2.

	label ifNotNil: [
		labelMargin := 4.
		w := f widthOfString: label.
		availableW := extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x := center x - (w // 2).
				l := label ]
			ifFalse: [
				x := labelMargin.
				l := label squeezedTo: (label size * availableW / w) rounded ].
		y := center y - (f lineSpacing // 2).
		self isPressed ifTrue: [
			x := x + 1.
			y := y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: (self enableLabelColorWith: Theme current buttonLabel) ]
]

{ #category : 'drawing' }
PluggableButtonMorph >> drawRoundLookOn: aCanvas [
	| r colorForButton rect |
	colorForButton := self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 mouseOverColor ifNil: [ Color h: color hue s: color saturation * 1.2 v: color brightness * 0.92 ]]
				ifFalse: [ color ]]
		ifTrue: [ mouseOverColor ifNil: [ color adjustSaturation: 0.1 brightness: -0.15 ]].

	r := Theme current roundedButtonRadius.
	rect := self roundButtonRect.
	r := r min: (rect width min: rect height) * 0.5.
	aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r.

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].
]

{ #category : 'enabling' }
PluggableButtonMorph >> enable [

	enableSelector := true.
	self redrawNeeded.
]

{ #category : 'drawing' }
PluggableButtonMorph >> enableColorWith: aColor [
	^ self isEnabled 
		ifTrue: [aColor] ifFalse: [aColor duller]
]

{ #category : 'drawing' }
PluggableButtonMorph >> enableLabelColorWith: aColor [
	^ self isEnabled 
		ifTrue: [aColor] ifFalse: [aColor paler "twiceLighter" ]
]

{ #category : 'initialization' }
PluggableButtonMorph >> enableSelector: aSymbol [

	enableSelector := aSymbol.
	enableSelector notNil ifTrue: [
		model ifNotNil: [
			"So we get #update: called"
			model addDependent: self]].
]

{ #category : 'geometry' }
PluggableButtonMorph >> extentChanged: oldExtent [
	"Our extent changed."

	super extentChanged: oldExtent.
	icon ifNotNil: [
		magnifiedIcon := nil ]
]

{ #category : 'accessing' }
PluggableButtonMorph >> font [

	^ self fontToUse
]

{ #category : 'accessing' }
PluggableButtonMorph >> font: someFont [

	PopUpMenu inform: 'You can''t set this'
]

{ #category : 'drawing' }
PluggableButtonMorph >> fontToUse [
	| fontToUse |
	fontToUse := font ifNil: [Preferences at: #standardButtonFont].
	"
	Could add emphasis...
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]
	"
	^fontToUse
]

{ #category : 'private' }
PluggableButtonMorph >> getEnabled [
	
		^self isEnabled 
]

{ #category : 'private' }
PluggableButtonMorph >> getState [
	"Answer the result of sending the receiver's model the stateSelector message."

	^ stateSelector 
		ifNil: [false]
		ifNotNil: [
			stateArgument
				ifNil: [	model perform: stateSelector]
				ifNotNil: [	model perform: stateSelector with: stateArgument]].
]

{ #category : 'event handling testing' }
PluggableButtonMorph >> handlesMouseDown: evt [
	"False if not enabled"
	
	^self isEnabled 
]

{ #category : 'event handling testing' }
PluggableButtonMorph >> handlesMouseOver: evt [
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^self isEnabled 
]

{ #category : 'event handling testing' }
PluggableButtonMorph >> handlesMouseStillDown: evt [
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	"Acting when down (instead of waiting until releasing the button)
	also means that the button action is repeated if the button is kept pressed"
	^actWhen == #buttonStillDown and: [self isEnabled ]
]

{ #category : 'accessing' }
PluggableButtonMorph >> icon: aForm [
	icon := aForm.
	magnifiedIcon := nil.
	self isRound ifTrue: [self morphExtent: icon extent + 5].
	self redrawNeeded.
]

{ #category : 'accessing' }
PluggableButtonMorph >> iconDrawSelector [
	"Must be understood by drawing canvas."

	^iconName
]

{ #category : 'accessing' }
PluggableButtonMorph >> iconDrawSelector: aSymbol [
	"aSymbol must be understood by drawing canvas."

	iconName := aSymbol
]

{ #category : 'accessing' }
PluggableButtonMorph >> iconName [
	
	^ self valueOfProperty: #iconName
]

{ #category : 'accessing' }
PluggableButtonMorph >> iconName: name [
	self setProperty: #iconName toValue: name
]

{ #category : 'initialization' }
PluggableButtonMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.

	self buttonStyle: #theme.	
	model := nil.
	enableSelector := nil.
	stateSelector := nil.
	stateArgument := nil.
	actionSelector := nil.
	actionArgument := nil.
	isPressed := false.
	mouseIsOver := false.
	mouseOverColor := nil.
	actWhen := #buttonUp.
	extent :=  `20 @ 15`
]

{ #category : 'testing' }
PluggableButtonMorph >> is: aSymbol [
	^ aSymbol == #PluggableButtonMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
PluggableButtonMorph >> isDisabled [
	
	^ self isEnabled not
]

{ #category : 'testing' }
PluggableButtonMorph >> isEnabled [
	"My enableSelector may be nil, in which case I answer the default: true.
	My  enableSelector may be a selector, which I and to the model to answer.
	Then again, my enableSelector may be simply true or false.
	"

	enableSelector ifNil: [^true].
	^(enableSelector isKindOf: Boolean)
		ifTrue: [enableSelector] 
		ifFalse: [model perform: enableSelector ]
]

{ #category : 'testing' }
PluggableButtonMorph >> isPressed [
	^isPressed or: [ self getState ]
]

{ #category : 'testing' }
PluggableButtonMorph >> isRound [
	^ buttonStyle == #round 
	or: [ buttonStyle == #theme and: [Theme current roundButtons] ]

]

{ #category : 'accessing' }
PluggableButtonMorph >> label [

	^ label
]

{ #category : 'accessing' }
PluggableButtonMorph >> label: aString [
	"Label this button with the given string."

	self label: aString font: nil
]

{ #category : 'accessing' }
PluggableButtonMorph >> label: aStringOrNil font: aFontOrNil [
	"Label this button with the given string."
	label := aStringOrNil.
	font := aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			self morphExtent: (self fontToUse widthOfString: label) + 10 @ (self fontToUse lineSpacing + 10) ]
		ifFalse: [ self redrawNeeded ].
]

{ #category : 'private' }
PluggableButtonMorph >> magnifiedIcon [
	| factor magnifiedExtent w h interiorExtent |

	magnifiedIcon ifNotNil: [ ^magnifiedIcon ].
	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		interiorExtent := self interiorExtent.
		magnifiedIcon := icon.
		w := icon width.
		h := icon height.
		w*h = 0 ifFalse: [
			factor := 1.0 * interiorExtent x / w min: 1.0 * interiorExtent y / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent := (icon extent * factor) rounded.
				magnifiedIcon := icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon
]

{ #category : 'accessing' }
PluggableButtonMorph >> magnifiedIcon: aForm [
	"Meaning: Don't magnify aForm to adapt it to our extent."

	icon := nil.
	magnifiedIcon := aForm.
	self redrawNeeded.
]

{ #category : 'geometry' }
PluggableButtonMorph >> minimumExtent [
	
	| unit |
	label ifNil: [
		^super minimumExtent ].

	"Assure some space for text."
	unit :=  (Preferences at: #windowTitleFont) pointSize.
	^(4 * unit) @ (3 * unit).
]

{ #category : 'initialization' }
PluggableButtonMorph >> model: anObject stateGetter: getStateSel action: actionSel label: aString [

	stateSelector := getStateSel.
	stateArgument := nil.
	actionSelector := actionSel.
	actionArgument := nil.
	stateSelector notNil
		ifTrue: [
			"So we get #update: called"
			self model: anObject ]
		ifFalse: [ self actionTarget: anObject ].
	self label: aString.
]

{ #category : 'initialization' }
PluggableButtonMorph >> model: anObject stateGetter: getStateSel stateArgument: getStateArg action: actionSel actionArgument: actionArg label: aString [

	stateSelector := getStateSel.
	stateArgument := getStateArg.
	actionSelector := actionSel.
	actionArgument := actionArg.
	stateSelector notNil
		ifTrue: [
			"So we get #update: called"
			self model: anObject ]
		ifFalse: [ self actionTarget: anObject ].
	self label: aString.
]

{ #category : 'events' }
PluggableButtonMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	self isEnabled ifFalse: [^self].  "guard condition"
	isPressed := true.
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
				tripleClkSel: nil ]
]

{ #category : 'events' }
PluggableButtonMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [

	isPressed := false.
	mouseIsOver := false.
	(actWhen == #buttonUp and: [ 
		self includesPixel: aMouseButtonEvent eventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded
]

{ #category : 'events' }
PluggableButtonMorph >> mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition [

	secondaryActionSelector ifNotNil: [
		model perform: secondaryActionSelector ].
	self redrawNeeded
]

{ #category : 'events' }
PluggableButtonMorph >> mouseEnter: aMouseEnterEvent [
	"The mouse entered the receiver"
	mouseIsOver := true.
	self redrawNeeded.
	super mouseEnter: aMouseEnterEvent
]

{ #category : 'testing' }
PluggableButtonMorph >> mouseIsOver [
	^mouseIsOver and: [self isEnabled ]
]

{ #category : 'events' }
PluggableButtonMorph >> mouseLeave: aMouseLeaveEvent [
	"The mouse has left the area of the receiver"
	mouseIsOver := false.
	self redrawNeeded.
	super mouseLeave: aMouseLeaveEvent.
]

{ #category : 'accessing' }
PluggableButtonMorph >> mouseOverColor: aColor [
	"If set, ignore #adoptWidgetsColor: for mouse over!"

	mouseOverColor := aColor
]

{ #category : 'events' }
PluggableButtonMorph >> mouseStillDown [
	"Acting when down (instead of waiting until releasing the button)
	also means that the button actin is repeated if the button is kept pressed.
	See #handlesMouseStillDown:"
	self performAction
]

{ #category : 'event handling' }
PluggableButtonMorph >> mouseStillDownStepRate [
	"At what rate do I want to receive #mouseStillDown notifications?
	This is actually the time interval between notifications, in milliseconds."
	^40
]

{ #category : 'accessing' }
PluggableButtonMorph >> noButtonShape [
	""
	self buttonStyle: #none.
	self redrawNeeded.
]

{ #category : 'accessing' }
PluggableButtonMorph >> padding: numberOrPoint [
	super padding: numberOrPoint .
	magnifiedIcon := nil.
	self redrawNeeded 
]

{ #category : 'accessing' }
PluggableButtonMorph >> performAction [
	"Inform the model that this button has been pressed. "
	self isEnabled ifFalse: [^ self ].
	actionSelector ifNil: [ ^ self ].
	
	self actionBlock value.
]

{ #category : 'drawing' }
PluggableButtonMorph >> roundButtonRect [
	^self localBounds insetBy: 3
]

{ #category : 'initialization' }
PluggableButtonMorph >> secondaryActionSelector: actionSel [

	secondaryActionSelector := actionSel.
]

{ #category : 'initialization' }
PluggableButtonMorph >> stateArgument: getStateArg [

	stateArgument := getStateArg.
]

{ #category : 'updating' }
PluggableButtonMorph >> update: aSymbol [
	super update: aSymbol.
	"Note: if any of these selectors is not nil, then we need to observe on the model,
	setting it with #model: and not with #actionTarget:"
	(aSymbol == stateSelector or: [aSymbol == enableSelector ]) ifTrue: [
		self redrawNeeded ].
]
