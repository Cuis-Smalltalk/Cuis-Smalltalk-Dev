"
A plain text editor for Morphic. Handles only one line. Does not handle fonts/styles, alignment, Smalltalk utilities and any other advanced stuff in TextModelMorph. Just a simple text entry field.
"
Class {
	#name : 'EntryFieldMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'font',
		'emphasis',
		'contents',
		'editor',
		'showTextCursor',
		'pauseBlinking',
		'textCursorRect',
		'backgroundColor'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
EntryFieldMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'instance creation' }
EntryFieldMorph class >> contents: aString [
	"
	(EntryFieldMorph contents: 'Some string') openInWorld
	"
	^ self contents: aString font: nil
]

{ #category : 'instance creation' }
EntryFieldMorph class >> contents: aString font: aFont [
	^ self basicNew initWithContents: aString font: aFont emphasis: 0
]

{ #category : 'instance creation' }
EntryFieldMorph class >> contents: aString font: aFont emphasis: emphasisCode [
	^ self basicNew initWithContents: aString font: aFont emphasis: emphasisCode
]

{ #category : 'examples' }
EntryFieldMorph class >> example1 [
"
	EntryFieldMorph example1 openInWorld
"
	^ EntryFieldMorph new
		borderWidth: 4;
		borderColor: Color blue;
		yourself
]

{ #category : 'examples' }
EntryFieldMorph class >> example2 [
"
	EntryFieldMorph example2 openInWorld
"
	| editor |
	editor := EntryFieldMorph 
		contents: 'Cuis-Smalltalk rocks' 
		font: FontFamily defaultFamilyAndPointSize 
		emphasis: AbstractFont boldCode.
	editor
		morphWidth: 400;
		color: Color blue;
		borderWidth: 6;
		borderColor: Color red.
	^ editor
]

{ #category : 'instance creation' }
EntryFieldMorph class >> new [
	^self contents: 'edit one line'
]

{ #category : 'unaccepted edits' }
EntryFieldMorph >> aboutToEdit [
	"NOP in this morph."
]

{ #category : 'accessing' }
EntryFieldMorph >> backgroundColor [
	
	^backgroundColor 
]

{ #category : 'accessing' }
EntryFieldMorph >> backgroundColor: aColor [
	backgroundColor = aColor ifFalse: [
		backgroundColor := aColor.
		self redrawNeeded]
]

{ #category : 'accessing' }
EntryFieldMorph >> baseFont [

	font ifNil: [ font := FontFamily defaultFamilyAndPointSize ].
	^font
]

{ #category : 'drawing' }
EntryFieldMorph >> characterIndexAtPoint: aPoint [

	| line block f |
	f := self fontToUse.
	
	line := TextLine 
		start: 1
		stop: contents size
		internalSpaces: 0
		paddingWidth: 0.
	line
		rectangle: (0@0 extent: extent);
		lineHeight: f lineSpacing baseline: f ascent.
		
	block := (CharacterBlockScanner new text: (contents asText font: f))
		defaultFont: f;
		characterBlockAtPoint: aPoint 		in: line forCursorPosition: true.

	^ block stringIndex
]

{ #category : 'events' }
EntryFieldMorph >> clickAndHalf: aMouseButtonEvent localPosition: localEventPosition [
	self editor clickAndHalf.
	self redrawNeeded.
]

{ #category : 'accessing' }
EntryFieldMorph >> contents [
	^contents
]

{ #category : 'accessing' }
EntryFieldMorph >> contents: newContents  [
	(newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			 contents := newContents string asUnicodeString]
		ifFalse: [
			contents = newContents ifTrue: [^self].	"no substantive change"
			contents := newContents asUnicodeString ].
	editor := nil.
	self fitContents.
	self redrawNeeded
]

{ #category : 'accessing' }
EntryFieldMorph >> crAction [
	"Return the action to perform when user presses <Return> key"
	^self valueOfProperty: #crAction
]

{ #category : 'accessing' }
EntryFieldMorph >> crAction: aBlock [
	"Sets the action to perform when user presses <Return> key"
	self setProperty: #crAction toValue: aBlock
]

{ #category : 'initialization' }
EntryFieldMorph >> defaultBackgroundColor [

	^ Theme current textPane 
]

{ #category : 'initialization' }
EntryFieldMorph >> defaultBorderWidth [
	^ Theme current buttonBorderWidth * 2
]

{ #category : 'initialization' }
EntryFieldMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Theme current text
]

{ #category : 'accessing' }
EntryFieldMorph >> disableEditing [
	self setProperty: #disablesEditing toValue: true.
	self stopBlinking
]

{ #category : 'event handling testing' }
EntryFieldMorph >> disablesEditing [

	^self hasProperty: #disablesEditing
]

{ #category : 'drawing' }
EntryFieldMorph >> displayTextCursorAtX: x top: top bottom: bottom emphasis: anEmphasis on: aCanvas [
	| textCursorColor x1 isBold isItalic x0 h w halfW r d |
	isBold := anEmphasis allMask: 1.
	isItalic := anEmphasis allMask: 2.
	textCursorColor := Theme current textCursor.
	h := bottom - top.
	w := isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW := w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!"
			d := isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 := x- (h*5//24) + d.
			x1 := x + d ]
		ifFalse: [
			x0 := x.
			x1 := x].
	x0 < halfW ifTrue: [
		x1 := x1 - x0 + halfW.
		x0 := halfW ].
	r := extent x-halfW-1.
	r < x1 ifTrue: [
		x0 := x0 + r - x1.
		x1 := r ].
	textCursorRect := x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		line: x0+halfW@bottom to: x1+halfW@(top+w)
		width: w color: textCursorColor
]

{ #category : 'drawing' }
EntryFieldMorph >> drawOn: aCanvas [

	aCanvas
		fillRectangle: self localBounds
		color: backgroundColor
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.
		
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	
	self hasTextCursor ifTrue: [ self drawTextCursorOn: aCanvas ].
	
	aCanvas
		drawString: contents
		at: self textOffset
		font: self fontToUse
		color: color.

]

{ #category : 'drawing' }
EntryFieldMorph >> drawSelectionOn: aCanvas [
	| rightX leftX bottom |

	bottom := self measureContents y + self textOffset y.
	leftX := self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	leftX := (leftX + self textOffset x) min: extent x.
	rightX := self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.
	rightX := (rightX + self textOffset x) min: extent x.

	aCanvas
		fillRectangle: ((leftX+1 @ self textOffset y) corner: (rightX + 2) @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)
]

{ #category : 'drawing' }
EntryFieldMorph >> drawTextCursorOn: aCanvas [
	|  bottom x |

	showTextCursor ifTrue: [
		bottom := self measureContents y + self textOffset y.
		x := self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayTextCursorAtX: (x + self textOffset x)  
			top: self borderWidth
			bottom: bottom 
			emphasis: emphasis 
			on: aCanvas ]
]

{ #category : 'accessing' }
EntryFieldMorph >> editor [
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditor ].
	^editor
]

{ #category : 'accessing' }
EntryFieldMorph >> emphasis [
	
	^ emphasis
]

{ #category : 'accessing' }
EntryFieldMorph >> emphasis: emphasisCode [
	
	emphasis := emphasisCode.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.
]

{ #category : 'accessing' }
EntryFieldMorph >> escAction [
	"Return the action to perform when user presses <Esc> key"

	^self valueOfProperty: #escAction
]

{ #category : 'accessing' }
EntryFieldMorph >> escAction: aBlock [
	"Sets the action to perform when user presses <Esc> key"

	self setProperty: #escAction toValue: aBlock.
]

{ #category : 'accessing' }
EntryFieldMorph >> fitContents [
	"Measures contents later at #minimumExtent"
	self morphExtent: extent.
]

{ #category : 'accessing' }
EntryFieldMorph >> font [

	^ font
]

{ #category : 'accessing' }
EntryFieldMorph >> font: aFont [

	font := aFont.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.
]

{ #category : 'events-processing' }
EntryFieldMorph >> fontPreferenceChanged [

	font := nil.
	self baseFont.
]

{ #category : 'accessing' }
EntryFieldMorph >> fontToUse [

	^ (emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ self baseFont ]
		ifFalse: [ self baseFont emphasized: emphasis ]
]

{ #category : 'event handling testing' }
EntryFieldMorph >> handlesKeyboardFocus [
	"Not if we are read-only"
	
	^self disablesEditing not
]

{ #category : 'event handling testing' }
EntryFieldMorph >> handlesMouseDown: aMouseButtonEvent [
	^ true
]

{ #category : 'event handling testing' }
EntryFieldMorph >> handlesMouseOver: evt [
	"implements #mouseEnter: and/or #mouseLeave:"
	^true
]

{ #category : 'testing' }
EntryFieldMorph >> hasSelection [

	^editor notNil and: [editor hasSelection]
]

{ #category : 'testing' }
EntryFieldMorph >> hasTextCursor [

	^ self hasKeyboardFocus and: [editor notNil and: [editor hasSelection not]]
]

{ #category : 'unaccepted edits' }
EntryFieldMorph >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag to the given value. "
	
	self flag: #jmv.
	"Not used in this morph, as it doesn't do accept / cancel"
	self redrawNeeded
]

{ #category : 'initialization' }
EntryFieldMorph >> initWithContents: aString font: aFont emphasis: emphasisCode  [
	self initialize.
	
	font := aFont.
	emphasis := emphasisCode.
	self contents: aString
]

{ #category : 'initialization' }
EntryFieldMorph >> initialize [
	super initialize.
	contents := UnicodeString new.
	font := nil.
	emphasis := 0.
	showTextCursor := false. "Should never be nil"
	backgroundColor := self defaultBackgroundColor.
]

{ #category : 'private' }
EntryFieldMorph >> installEditor [
	"Install an editor for my contents.  This constitutes 'hasFocus'.
	If priorEditor is not nil, then initialize the new editor from its state.
	We may want to rework this so it actually uses the prior editor."

	editor := SimpleEditor new morph: self.
	editor changeString: contents.
	self redrawNeeded.
	^editor
]

{ #category : 'events' }
EntryFieldMorph >> keyStroke: aKeyboardEvent [
	"Handle a keystroke event."
	
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [
		contents := self editor string.
		self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
		self redrawNeeded.
		^self ].
	
	self pauseBlinking.

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.

	"Return - check for special action"
	aKeyboardEvent isReturnKey ifTrue: [
		self crAction ifNotNil: [ :action |
			action value.
			^self ]].

	"Esc - check for special action"
	aKeyboardEvent isEsc ifTrue: [
		self escAction ifNotNil: [ :action |
			action value.
			^self  ]].

	self editor processKeystrokeEvent: aKeyboardEvent.
	contents := self editor string.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.
]

{ #category : 'events-processing' }
EntryFieldMorph >> keyboardFocusChange: aBoolean [
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor ].
			self disablesEditing ifFalse: [
				self startBlinking ]]
		ifFalse: [
			self stopBlinking ].
	self redrawNeeded.
]

{ #category : 'geometry' }
EntryFieldMorph >> measureContents [
	"Answer text extent"
	
	| myFont  |
	myFont := self fontToUse.
	^((myFont widthOfString: contents) max: 3)  @ myFont lineSpacing.
]

{ #category : 'menu - shortcuts' }
EntryFieldMorph >> menuNshortcutsSpecsDo: aBlock [
	"Iterate over our own menu and shortcut specs.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."

	"First take default editor menu and shortcut specs"
	self editor defaultMenuSpec do: aBlock.

	self window ifNotNil: [ :myWindow |

		"Then include additions, overrides and deletions registered for #Editor, #TextEditor, #SmalltalkEditor, etc.
		Hierarchy order allows more specific classes to override choices made by superclasses."
		(self editor class withAllSuperclassesDownFrom: Editor) do: [ :editorClass |
			myWindow allRegisteredMenuAndShortcutsSpecProviders: editorClass name do: aBlock widget: self ]].
]

{ #category : 'geometry' }
EntryFieldMorph >> minimumExtent [
	| h |
	h := self textBox y.
	^3@1 * h
]

{ #category : 'events' }
EntryFieldMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: localEventPosition).
	self redrawNeeded.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil
]

{ #category : 'events' }
EntryFieldMorph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self ].
	self editor mouseMove: aMouseMoveEvent index: (self characterIndexAtPoint: localEventPosition).
	self redrawNeeded.
]

{ #category : 'blink cursor' }
EntryFieldMorph >> onBlinkCursor [
	"Blink the cursor"
	showTextCursor := showTextCursor not | pauseBlinking.
	pauseBlinking := false.
	textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r]
]

{ #category : 'blink cursor' }
EntryFieldMorph >> pauseBlinking [
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking := true.
	self disablesEditing ifFalse: [
		"Show cursor right now if needed"
		showTextCursor ifFalse: [
			showTextCursor := true.
			textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]]
]

{ #category : 'typing/selecting keys' }
EntryFieldMorph >> selectAll [
	self editor selectAll.
	self redrawNeeded
]

{ #category : 'blink cursor' }
EntryFieldMorph >> startBlinking [
	"And show the cursor"
	pauseBlinking := true.
	"Start blinking in a short while"
	showTextCursor := true.
	self startStepping: #onBlinkCursor stepTime: 500
]

{ #category : 'blink cursor' }
EntryFieldMorph >> stopBlinking [
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	"Hide cursor right now if needed"
	showTextCursor ifTrue: [
		showTextCursor := false.
		textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]
]

{ #category : 'geometry' }
EntryFieldMorph >> textBox [
	"Answer amount of interior space including text"
	^ self measureContents + (2 * self textOffset)
]

{ #category : 'geometry' }
EntryFieldMorph >> textOffset [
	"Answer offset to upper-left of text drawing origin"
	| off |
	off := (2 * self borderWidth) + self fontToUse ascent + self fontToUse descent / 4.  "NB: lineGap is frequently zero"
	^ off @ off
]

{ #category : 'drawing' }
EntryFieldMorph >> textOffsetdrawTextCursorOn: aCanvas [
	|  bottom x |

	showTextCursor ifTrue: [
		bottom := self textBox y.
		x := self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayTextCursorAtX: (x + self textOffset x)  top: (self textOffset y) bottom: bottom emphasis: emphasis on: aCanvas ]
]
