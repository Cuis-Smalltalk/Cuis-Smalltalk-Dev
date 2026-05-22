"
I show the possible completions in a menu like appearance. The user may choose an entry from my list and complete the word he was typing in the editor. I'm showed with the Tab key and will be deleted when with ESC key or when a successful completion occurs. The following keystrokes are supported:

Ctrl-Space or Tab: Open a new morph. Tab requires at least one character in front of the cursor. When already open complete the selected entry. 
Esc: Close me
Arrow Up: Move one entry up.
Arrow Down: Move one entry down
Return: (like Ctrl-Space and Tab): Complete with the selected item and close the morph
any letter or digit: Narrow the completion further


"
Class {
	#name : 'AutoCompleterMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'completer',
		'selected',
		'firstVisible',
		'itemHeight',
		'lastActivity',
		'itemsPerPage',
		'downButtonForm',
		'upButtonForm'
	],
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'new-morph participation' }
AutoCompleterMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'instance creation' }
AutoCompleterMorph class >> completer: anAutoCompleter position: aPoint  [

	| newObject |

	newObject := self new.
	newObject setCompleter: anAutoCompleter position: aPoint.

	^ newObject
]

{ #category : 'instance creation' }
AutoCompleterMorph class >> initializedInstance [
	| completer m |
	completer := SmalltalkCompleter withModel: (TextModel withText: 'Small').
	completer
		instVarNamed: 'position'
		put: 5.
	completer computeEntries.
	m := completer morphClass
		completer: completer
		position: `10@10`.
	completer instVarNamed: 'menuMorph' put: m.
	^m
]

{ #category : 'preferences' }
AutoCompleterMorph class >> itemHeight [
	"height must be forced to be even to allow the detail arrow to be drawn correctly"	
	^ (self listFont lineSpacing + 2) roundUpTo: 2"14".
]

{ #category : 'preferences' }
AutoCompleterMorph class >> listFont [

	^Preferences at: #standardListFont
]

{ #category : 'drawing' }
AutoCompleterMorph >> calculateWidth [

	| width font |

	width := 120.
	font := self class listFont.

	1
		to: self entryCount
		do: [ :index | width := width max: (font widthOfString: (completer entries at: index) asPlainString)].

	self entryCount > self itemsPerPage ifTrue: [ width := width + Theme current scrollbarThickness ].

	^ width 
]

{ #category : 'testing' }
AutoCompleterMorph >> canSelectEntryAt: anIndex [
	
	^completer canSelectEntryAt: anIndex
]

{ #category : 'drawing' }
AutoCompleterMorph >> colorOf: entry [

	^(completer isPossibleInvalidEntry: entry) 
		ifTrue: [ Theme current autoCompleterMaybeInvalid ] 
		ifFalse: [ Theme current autoCompleter ]
	
]

{ #category : 'initialization' }
AutoCompleterMorph >> defaultBorderColor [
	"My default border color.
	
	Note: My background color is derived from this color."
	^ Theme current autoCompleterDefaultBorderColor 
]

{ #category : 'initialization' }
AutoCompleterMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : 'initialization' }
AutoCompleterMorph >> defaultColor [
	^ Theme current paneBackgroundFrom: self defaultBorderColor
]

{ #category : 'accessing' }
AutoCompleterMorph >> defaultLayerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^20
]

{ #category : 'drawing' }
AutoCompleterMorph >> downButtonPosition [

	^ `0@0` + (extent - Theme current scrollbarThickness)
]

{ #category : 'drawing' }
AutoCompleterMorph >> drawContainingRectangle: aCanvas [
	
	aCanvas frameAndFillRectangle: self localBounds fillColor: color borderWidth: borderWidth borderColor: borderColor.

]

{ #category : 'drawing' }
AutoCompleterMorph >> drawDownArrowOn: aCanvas thickness: scrollbarThickness [

	downButtonForm ifNil: [
		downButtonForm := (ScrollBar new instVarNamed: 'downButton') imageForm: 32 ].
	aCanvas
		image: downButtonForm
		at: self downButtonPosition.

]

{ #category : 'drawing' }
AutoCompleterMorph >> drawItemOf: index on: aCanvas width: width top: itemTop [

	| rectangle entry |
	
	rectangle := 1@itemTop extent: width@self itemHeight.
	index = self selected ifTrue: [ aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].

	entry := completer entries at: index.
	aCanvas
		drawString: entry asPlainString
		at: rectangle topLeft
		font: self class listFont
		color: (self colorOf: entry).
	
]

{ #category : 'drawing' }
AutoCompleterMorph >> drawItemsOn: aCanvas width: width [

	| itemTop |

	itemTop := 1.
	self firstVisible
		to: self lastVisible
		do: [ :index |
			self drawItemOf: index on: aCanvas width: width top: itemTop.
			itemTop := itemTop + self itemHeight ].
]

{ #category : 'drawing' }
AutoCompleterMorph >> drawOn: aCanvas [
	
	| width |
	
	self drawContainingRectangle: aCanvas.
	width := self drawScrollBarOn: aCanvas.
	self drawItemsOn: aCanvas width: width 

]

{ #category : 'drawing' }
AutoCompleterMorph >> drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness [
	
	| height top bottom |

	height := extent y - (2 * scrollbarThickness).
	top := (1.0 * self firstVisible-1 / self entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom := (1.0 * self lastVisible / self entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray`
]

{ #category : 'drawing' }
AutoCompleterMorph >> drawScrollBarOn: aCanvas [

	| scrollbarThickness width |

	width := extent x-2.
	self entryCount > self itemsPerPage  ifTrue: [
		scrollbarThickness := Theme current scrollbarThickness.
		width := width - scrollbarThickness.

		self drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness.
		self drawUpArrowOn: aCanvas thickness: scrollbarThickness.
		self drawDownArrowOn: aCanvas thickness: scrollbarThickness.
		self drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness ].

	^width

]

{ #category : 'drawing' }
AutoCompleterMorph >> drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness [
	
	aCanvas
		frameRectangle: (extent x - scrollbarThickness@0 extent: scrollbarThickness @ extent y)
		borderWidth: 1
		color: borderColor.

]

{ #category : 'drawing' }
AutoCompleterMorph >> drawUpArrowOn: aCanvas thickness: scrollbarThickness [

	upButtonForm ifNil: [
		upButtonForm := (ScrollBar new instVarNamed: 'upButton') imageForm: 32 ].
	aCanvas
		image: upButtonForm
		at: self upButtonPosition.

]

{ #category : 'accessing' }
AutoCompleterMorph >> entryCount [
	
	^completer entryCount
]

{ #category : 'private' }
AutoCompleterMorph >> firstSelectableEntryIndex [
	
	^self nextSelectableEntryIndexFrom: 0
]

{ #category : 'private' }
AutoCompleterMorph >> firstVisible [

	^firstVisible min: self entryCount
]

{ #category : 'private' }
AutoCompleterMorph >> firstVisible: anIndex [
	
	firstVisible := anIndex
		min: self entryCount - self itemsPerPage + 1
		max: 1.
]

{ #category : 'actions' }
AutoCompleterMorph >> goDown [
	
	self selected: (self nextSelectableEntryIndexFrom: self selected).
	(self selected between: self firstVisible and: self lastVisible)
		ifFalse: [self lastVisible: self selected].
	
	self redrawNeeded.
]

{ #category : 'actions' }
AutoCompleterMorph >> goHome [

	self selected: self firstSelectableEntryIndex.
	self firstVisible: 1.
	self redrawNeeded.
]

{ #category : 'actions' }
AutoCompleterMorph >> goPageDown [
	
	| oldEntry newEntry nextEntry |
	
	oldEntry := self selected.
	newEntry := oldEntry.
	[nextEntry := self nextSelectableEntryIndexFrom: newEntry.
	nextEntry > oldEntry and: [nextEntry - oldEntry <= self itemsPerPage]]
		whileTrue: [newEntry := nextEntry].
	
	self selected: newEntry.
	self firstVisible: newEntry.
	
	self redrawNeeded.
]

{ #category : 'actions' }
AutoCompleterMorph >> goPageUp [
	
	| oldEntry newEntry nextEntry |
	
	oldEntry := self selected.
	newEntry := oldEntry.
	[nextEntry := self previousSelectableEntryIndexFrom: newEntry.
	oldEntry > nextEntry and: [oldEntry - nextEntry <= self itemsPerPage]]
		whileTrue: [newEntry := nextEntry].
	
	self selected: newEntry.
	self firstVisible: newEntry.
	
	self redrawNeeded.
]

{ #category : 'actions' }
AutoCompleterMorph >> goToEnd [
	
	self selected: self lastSelectableEntryIndex.
	self lastVisible: self selected.
	self redrawNeeded.
]

{ #category : 'actions' }
AutoCompleterMorph >> goUp [

	(self selected = 0 and: [self firstVisible = 1]) ifTrue: [^ self].
	
	self selected: (self previousSelectableEntryIndexFrom: self selected).
	(self selected between: self firstVisible and: self lastVisible)
		ifFalse: [self firstVisible: self selected].
		
	self redrawNeeded.
]

{ #category : 'event handling testing' }
AutoCompleterMorph >> handlesMouseDown: aMouseButtonEvent [

	^ true
]

{ #category : 'event handling testing' }
AutoCompleterMorph >> handlesMouseOver: evt [
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^true
]

{ #category : 'event handling testing' }
AutoCompleterMorph >> handlesMouseScroll: aMouseScrollEvent [
	^true
]

{ #category : 'actions' }
AutoCompleterMorph >> help [

	TextModel new contents: AutoCompleter helpText; openLabel: 'uCompletion Keyboard Help'
]

{ #category : 'actions' }
AutoCompleterMorph >> initMenu [
	| maxItemsPerPage |

	maxItemsPerPage := 13.
	itemsPerPage := self entryCount min: maxItemsPerPage.

	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.
]

{ #category : 'accessing' }
AutoCompleterMorph >> itemHeight [
	"cached to minimise recalculation"
	^ itemHeight ifNil: [itemHeight := self class itemHeight]
]

{ #category : 'accessing' }
AutoCompleterMorph >> itemsPerPage [

	^itemsPerPage
]

{ #category : 'activity tracking' }
AutoCompleterMorph >> lastActivity  [
	lastActivity ifNil: [self stillActive].
	^ lastActivity 
]

{ #category : 'private' }
AutoCompleterMorph >> lastSelectableEntryIndex [
	
	^self previousSelectableEntryIndexFrom: 1
]

{ #category : 'private' }
AutoCompleterMorph >> lastVisible [
	
	^self firstVisible + self itemsPerPage - 1 min: self entryCount
]

{ #category : 'private' }
AutoCompleterMorph >> lastVisible: anIndex [
	
	self firstVisible: anIndex - self itemsPerPage + 1.
]

{ #category : 'events' }
AutoCompleterMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [

	(self includesPixel: aMouseButtonEvent eventPosition)
		ifTrue: [
			((self upButtonPosition extent: Theme current scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ self stillActive; goUp. ^self ].
			((self downButtonPosition extent: Theme current scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ self stillActive; goDown. ^self ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]
]

{ #category : 'events' }
AutoCompleterMorph >> mouseScroll: aMouseEvent localPosition: localEventPosition [
	self stillActive .
	aMouseEvent direction = #up ifTrue: [^self goUp] .
	aMouseEvent direction = #down ifTrue: [^self goDown ] .
]

{ #category : 'private' }
AutoCompleterMorph >> nextSelectableEntryIndexFrom: anIndex [
	
	^self nextSelectableEntryIndexFrom: anIndex goingForwards: true
]

{ #category : 'private' }
AutoCompleterMorph >> nextSelectableEntryIndexFrom: anIndex goingForwards: goingForwardsBoolean [
	
	| direction indicesFromAnIndex |
	
	direction := goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	indicesFromAnIndex := (1 to: self entryCount)
		collect: [ :offset | self wrapIndex: anIndex + (offset*direction) by: self entryCount ].
	
	^indicesFromAnIndex
		detect: [ :index | self canSelectEntryAt: index ]
		ifNone: [self error: 'there are no selectable entries']
]

{ #category : 'private' }
AutoCompleterMorph >> previousSelectableEntryIndexFrom: anIndex [
	
	^self nextSelectableEntryIndexFrom: anIndex goingForwards: false
]

{ #category : 'actions' }
AutoCompleterMorph >> resetMenu [

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
	self redrawNeeded.
]

{ #category : 'accessing' }
AutoCompleterMorph >> selected [
	"Answer the value of selected"
	selected ifNil: [ self selected: self firstVisible ].
	^ selected
]

{ #category : 'accessing' }
AutoCompleterMorph >> selected: aNumber [
	"Set the value of selected"

	((aNumber between: 1 and: self entryCount) and: [ aNumber ~= selected ])
		ifTrue: [
			selected := aNumber ]
]

{ #category : 'initialization' }
AutoCompleterMorph >> setCompleter: anAutoCompleter position: aPoint  [

	completer := anAutoCompleter.
	self initMenu.
	self openInWorld.
	self morphPosition: aPoint.
	self resetMenu.
]

{ #category : 'drawing' }
AutoCompleterMorph >> setDefaultColors [

	self color: self defaultColor.
	self borderColor: self defaultBorderColor 
]

{ #category : 'stepping' }
AutoCompleterMorph >> step [

	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]
]

{ #category : 'stepping' }
AutoCompleterMorph >> stepTime  [
	^ 100
]

{ #category : 'activity tracking' }
AutoCompleterMorph >> stillActive [
	lastActivity := Time localMillisecondClock
]

{ #category : 'activity tracking' }
AutoCompleterMorph >> timeOfLastActivity [
	^ (Time localMillisecondClock - self lastActivity)
]

{ #category : 'activity tracking' }
AutoCompleterMorph >> timeout [
	^ 5000
]

{ #category : 'drawing' }
AutoCompleterMorph >> upButtonPosition [

	^extent x - Theme current scrollbarThickness@0
]

{ #category : 'activity tracking' }
AutoCompleterMorph >> updateColor [

	| remaining alpha |

	remaining := (self timeout - self timeOfLastActivity).
	remaining < 1000 
		ifTrue: [
			alpha := remaining / 1000.0.
			self color: (self color alpha: alpha).
			self borderColor: (borderColor alpha: alpha) ]
		ifFalse: [ self setDefaultColors ]
		
			
]

{ #category : 'stepping' }
AutoCompleterMorph >> wantsSteps [
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message"

	^true
]

{ #category : 'private' }
AutoCompleterMorph >> wrapIndex: anIndex by: aSize [
	
	^anIndex - 1 \\ aSize + 1
]
