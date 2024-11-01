'From Cuis7.1 [latest update: #6809] on 1 November 2024 at 11:39:00 am'!
!classDefinition: #EntryFieldMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #EntryFieldMorph
	instanceVariableNames: 'font emphasis contents editor showTextCursor pauseBlinking textCursorRect backgroundColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!EntryFieldMorph commentStamp: '<historical>' prior: 0!
A plain text editor for Morphic. Handles only one line. Does not handle fonts/styles, alignment, Smalltalk utilities and any other advanced stuff in TextModelMorph. Just a simple text entry field.!

!classDefinition: #SimpleEditor category: #'System-Text'!
Editor subclass: #SimpleEditor
	instanceVariableNames: 'string pointIndex markIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!SimpleEditor commentStamp: '<historical>' prior: 0!
See comment in Editor!


!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
backgroundColor
	
	^backgroundColor ! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
backgroundColor: aColor
	backgroundColor = aColor ifFalse: [
		backgroundColor := aColor.
		self redrawNeeded]! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
baseFont

	font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ].
	^font! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
contents
	^contents! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
contents: newContents 
	(newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			 contents := newContents string asUnicodeString]
		ifFalse: [
			contents = newContents ifTrue: [^self].	"no substantive change"
			contents := newContents asUnicodeString ].
	editor := nil.
	self fitContents.
	self redrawNeeded! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
disableEditing
	self setProperty: #disablesEditing toValue: true.
	self stopBlinking! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
editor
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditor ].
	^editor! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
emphasis
	
	^ emphasis! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
emphasis: emphasisCode
	
	emphasis := emphasisCode.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:12:18'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: extent.! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
font

	^ font! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
font: aFont

	font := aFont.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 11/1/2024 11:10:46'!
fontToUse

	^ (emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ self baseFont ]
		ifFalse: [ self baseFont emphasized: emphasis ]! !

!EntryFieldMorph methodsFor: 'blink cursor' stamp: 'jmv 11/1/2024 11:10:46'!
onBlinkCursor
	"Blink the cursor"
	showTextCursor _ showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r]! !

!EntryFieldMorph methodsFor: 'blink cursor' stamp: 'jmv 11/1/2024 11:10:46'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	self showsBlinkingCursor ifTrue: [
		"Show cursor right now if needed"
		showTextCursor ifFalse: [
			showTextCursor _ true.
			textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]]! !

!EntryFieldMorph methodsFor: 'blink cursor' stamp: 'jmv 11/1/2024 11:10:46'!
showsBlinkingCursor

	^self handlesKeyboard and: [ self disablesEditing not ]! !

!EntryFieldMorph methodsFor: 'blink cursor' stamp: 'jmv 11/1/2024 11:10:46'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	showTextCursor _ true.
	self startStepping: #onBlinkCursor stepTime: 500! !

!EntryFieldMorph methodsFor: 'blink cursor' stamp: 'jmv 11/1/2024 11:10:46'!
stopBlinking
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	"Hide cursor right now if needed"
	showTextCursor ifTrue: [
		showTextCursor _ false.
		textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 11/1/2024 11:10:46'!
characterIndexAtPoint: aPoint

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

	^ block stringIndex! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 11/1/2024 11:10:46'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: anEmphasis on: aCanvas
	| textCursorColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ anEmphasis allMask: 1.
	isItalic _ anEmphasis allMask: 2.
	textCursorColor _ Theme current textCursor.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!!"
			d _ isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0 < halfW ifTrue: [
		x1 _ x1 - x0 + halfW.
		x0 _ halfW ].
	r _ extent x-halfW-1.
	r < x1 ifTrue: [
		x0 _ x0 + r - x1.
		x1 _ r ].
	textCursorRect _ x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		line: x0+halfW@bottom to: x1+halfW@(top+w)
		width: w color: textCursorColor! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 11/1/2024 11:10:46'!
drawOn: aCanvas

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
! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 11/1/2024 11:10:46'!
drawSelectionOn: aCanvas
	| rightX leftX bottom |

	bottom := self measureContents y + self textOffset y.
	leftX := self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	leftX := (leftX + self textOffset x) min: extent x.
	rightX := self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.
	rightX := (rightX + self textOffset x) min: extent x.

	aCanvas
		fillRectangle: ((leftX+1 @ self textOffset y) corner: (rightX + 2) @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 11/1/2024 11:10:47'!
drawTextCursorOn: aCanvas
	|  bottom x |

	showTextCursor ifTrue: [
		bottom := self measureContents y + self textOffset y.
		x := self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayTextCursorAtX: (x + self textOffset x)  
			top: self borderWidth
			bottom: bottom 
			emphasis: emphasis 
			on: aCanvas ]! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 11/1/2024 11:10:47'!
textOffsetdrawTextCursorOn: aCanvas
	|  bottom x |

	showTextCursor ifTrue: [
		bottom := self textBox y.
		x := self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayTextCursorAtX: (x + self textOffset x)  top: (self textOffset y) bottom: bottom emphasis: emphasis on: aCanvas ]! !

!EntryFieldMorph methodsFor: 'editing' stamp: 'jmv 11/1/2024 11:28:32'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the composition"

	"Also couple the editor to Morphic keyboard events"

	| oldEditor oldContents |
	oldEditor := editor.
	oldContents := contents.
	interactionBlock value.
	oldContents == contents 
		ifTrue: [ 						"this will not work if the composition changed"
			editor := oldEditor					"since it may have been changed while in block"].
	self redrawNeeded! !

!EntryFieldMorph methodsFor: 'event handling testing' stamp: 'jmv 11/1/2024 11:10:47'!
disablesEditing

	^self hasProperty: #disablesEditing! !

!EntryFieldMorph methodsFor: 'event handling testing' stamp: 'jmv 11/1/2024 11:10:47'!
handlesKeyboard

	^self visible! !

!EntryFieldMorph methodsFor: 'event handling testing' stamp: 'jmv 11/1/2024 11:10:47'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!EntryFieldMorph methodsFor: 'event handling testing' stamp: 'jmv 11/1/2024 11:10:47'!
handlesMouseOver: evt
	"implements #mouseEnter: and/or #mouseLeave:"
	^true! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 11/1/2024 11:10:47'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	self handleInteraction: [ self editor clickAndHalf ]! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 11/1/2024 11:19:41'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ 
		self updateFromContents.
		self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
		self redrawNeeded.
		^self ].

	self pauseBlinking.
	self handleInteraction: [ self editor processKeystrokeEvent: aKeyboardEvent ].
	self updateFromContents.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 11/1/2024 11:10:47'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent hand newKeyboardFocus: self.

	self handleInteraction: [
		self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: localEventPosition) ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 11/1/2024 11:10:47'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking
! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 11/1/2024 11:10:47'!
mouseEnter: event
	super mouseEnter: event.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 11/1/2024 11:10:47'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self ].
	self handleInteraction: [
		self editor mouseMove: aMouseMoveEvent index: (self characterIndexAtPoint: localEventPosition) ]! !

!EntryFieldMorph methodsFor: 'events-processing' stamp: 'jmv 11/1/2024 11:10:47'!
focusKeyboardFor: aKeyboardEvent
	"If aKeyboardEvent tab or shift-tab use it to navigate keyboard focus.
	ctrl key ignored."
	aKeyboardEvent keyValue = 9
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifFalse: [ aKeyboardEvent hand keyboardFocusNext ]
				ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ].
			^ true ].
	^super focusKeyboardFor: aKeyboardEvent! !

!EntryFieldMorph methodsFor: 'events-processing' stamp: 'jmv 11/1/2024 11:10:47'!
fontPreferenceChanged

	font := nil.
	self baseFont.! !

!EntryFieldMorph methodsFor: 'events-processing' stamp: 'jmv 11/1/2024 11:12:44'!
keyboardFocusChange: aBoolean
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor ].
			self showsBlinkingCursor ifTrue: [
				self startBlinking ]]
		ifFalse: [
			self stopBlinking ].
	self redrawNeeded.! !

!EntryFieldMorph methodsFor: 'geometry' stamp: 'jmv 11/1/2024 11:10:47'!
measureContents
	"Answer text extent"
	
	| myFont  |
	myFont := self fontToUse.
	^((myFont widthOfString: contents) max: 3)  @ myFont lineSpacing.! !

!EntryFieldMorph methodsFor: 'geometry' stamp: 'jmv 11/1/2024 11:10:47'!
minimumExtent

	^ self textBox! !

!EntryFieldMorph methodsFor: 'geometry' stamp: 'jmv 11/1/2024 11:10:47'!
textBox
	"Answer amount of interior space including text"
	^ self measureContents + (2 * self textOffset)! !

!EntryFieldMorph methodsFor: 'geometry' stamp: 'jmv 11/1/2024 11:10:47'!
textOffset
	"Answer offset to upper-left of text drawing origin"
	| off |
	off := (2 * self borderWidth) + self fontToUse ascent + self fontToUse descent / 4.  "NB: lineGap is frequently zero"
	^ off @ off! !

!EntryFieldMorph methodsFor: 'initialization' stamp: 'jmv 11/1/2024 11:10:47'!
defaultBackgroundColor

	^ Theme current textPane ! !

!EntryFieldMorph methodsFor: 'initialization' stamp: 'jmv 11/1/2024 11:32:47'!
defaultBorderWidth
	^ Theme current buttonBorderWidth * 2! !

!EntryFieldMorph methodsFor: 'initialization' stamp: 'jmv 11/1/2024 11:10:47'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current text! !

!EntryFieldMorph methodsFor: 'initialization' stamp: 'jmv 11/1/2024 11:10:47'!
initWithContents: aString font: aFont emphasis: emphasisCode 
	self initialize.
	
	font _ aFont.
	emphasis _ emphasisCode.
	self contents: aString! !

!EntryFieldMorph methodsFor: 'initialization' stamp: 'jmv 11/1/2024 11:10:47'!
initialize
	super initialize.
	contents := UnicodeString new.
	font := nil.
	emphasis := 0.
	showTextCursor := false. "Should never be nil"
	backgroundColor := self defaultBackgroundColor.! !

!EntryFieldMorph methodsFor: 'testing' stamp: 'jmv 11/1/2024 11:10:47'!
hasSelection

	^editor notNil and: [editor hasSelection]! !

!EntryFieldMorph methodsFor: 'testing' stamp: 'jmv 11/1/2024 11:10:47'!
hasTextCursor

	^ self hasKeyboardFocus and: [editor notNil and: [editor hasSelection not]]! !

!EntryFieldMorph methodsFor: 'typing/selecting keys' stamp: 'jmv 11/1/2024 11:10:47'!
selectAll
	self editor selectAll.
	self redrawNeeded! !

!EntryFieldMorph methodsFor: 'unaccepted edits' stamp: 'jmv 11/1/2024 11:10:47'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	
	self flag: #jmv.
	"Not used in this morph, as it doesn't do accept / cancel"
	self redrawNeeded! !

!EntryFieldMorph methodsFor: 'menu - shortcuts' stamp: 'jmv 11/1/2024 11:26:33'!
menuNshortcutsSpecsDo: aBlock
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
			(myWindow class withAllSuperclassesDownFrom: SystemWindow) do: [ :windowClass |
				(windowClass registeredMenuAndShortcutsSpecProviders: editorClass name) ifNotNil: [ :providers |
					providers do: [ :eachMessageSend |
						(eachMessageSend valueWithPossibleArgument: self) do: aBlock ]]		]]].! !

!EntryFieldMorph methodsFor: 'private' stamp: 'jmv 11/1/2024 11:10:47'!
installEditor
	"Install an editor for my contents.  This constitutes 'hasFocus'.
	If priorEditor is not nil, then initialize the new editor from its state.
	We may want to rework this so it actually uses the prior editor."

	editor _ SimpleEditor new morph: self.
	editor changeString: contents.
	self redrawNeeded.
	^editor! !

!EntryFieldMorph methodsFor: 'private' stamp: 'jmv 11/1/2024 11:10:47'!
updateFromContents

	contents := self editor string.
	self redrawNeeded! !


!EntryFieldMorph class methodsFor: 'new-morph participation' stamp: 'jmv 11/1/2024 11:10:47'!
categoryInNewMorphMenu

	^ 'User Interface'! !

!EntryFieldMorph class methodsFor: 'instance creation' stamp: 'jmv 11/1/2024 11:30:17'!
contents: aString
	"
	(EntryFieldMorph contents: 'Some string') openInWorld
	"
	^ self contents: aString font: nil! !

!EntryFieldMorph class methodsFor: 'instance creation' stamp: 'jmv 11/1/2024 11:10:47'!
contents: aString font: aFont
	^ self basicNew initWithContents: aString font: aFont emphasis: 0! !

!EntryFieldMorph class methodsFor: 'instance creation' stamp: 'jmv 11/1/2024 11:10:47'!
contents: aString font: aFont emphasis: emphasisCode
	^ self basicNew initWithContents: aString font: aFont emphasis: emphasisCode! !

!EntryFieldMorph class methodsFor: 'instance creation' stamp: 'jmv 11/1/2024 11:10:47'!
new
	^self contents: 'edit one line'! !

!EntryFieldMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2024 11:37:58'!
example1
"
	EntryFieldMorph example1 openInWorld
"
	^ EntryFieldMorph new
		borderWidth: 4;
		borderColor: Color blue;
		yourself! !

!EntryFieldMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2024 11:38:09'!
example2
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
	^ editor! !


!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 12/19/2011 12:24'!
addString: aString
	"Think of a better name"

	self replaceSelectionWith: aString! !

!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 12/19/2011 12:24'!
backTo: startIndex
	"During typing, backspace to startIndex."

	self markIndex: startIndex.
	self replaceSelectionWith: ''.
	self markIndex: self pointIndex! !

!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 8/1/2024 11:34:57'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."
	| codePoint asciiValue |
	codePoint := aKeyboardEvent keyValue.
	
	codePoint > 255 ifTrue: [
		^self normalCharacter: aKeyboardEvent ].

	"We don't support multiple lines. Therefore, we don't process return as a #normalCharacter:"
	aKeyboardEvent isReturnKey ifTrue: [
		^ true].

	asciiValue := codePoint.
	^ self perform: (self keyboardActions at: asciiValue + 1) with: aKeyboardEvent! !

!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 1/5/2021 14:34:14'!
processKeystrokeEvent: aKeyboardEvent

	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		^self].
	self markIndex: self pointIndex! !

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 11/4/2008 13:08'!
changeString: aString
	"Install aString as the one to be edited by the receiver."

	string _ aString.
	self resetState! !

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 4/16/2023 17:10:41'!
initialize
	"Initialize the state of the receiver. Subclasses should include 'super 
	initialize' when redefining this message to insure proper initialization."

	super initialize.
	self changeString: UnicodeString new! !

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:37'!
resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex _ 1.	"Like pointBlock in TextEditor"
	markIndex _ 1.	"Like markBlock in TextEditor"! !

!SimpleEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:42'!
clickAndHalf

	| here |
	here _ self pointIndex.
	(here between: 2 and: string size)
		ifTrue: [ self selectWord ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ].! !

!SimpleEditor methodsFor: 'events' stamp: 'jmv 12/29/2011 14:51'!
mouseDown: evt index: index

	evt shiftPressed
		ifFalse: [
			self markIndex: index pointIndex: index ]
		ifTrue: [
			self mouseMove: evt index: index ]! !

!SimpleEditor methodsFor: 'events' stamp: 'jmv 5/27/2011 15:23'!
mouseMove: evt index: index
	"Change the selection in response to mouse-down drag"

	self pointIndex: index! !

!SimpleEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:37'!
copySelection
	"Copy the current selection and store it in the Clipboard, unless empty."

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection! !

!SimpleEditor methodsFor: 'menu messages' stamp: 'jmv 9/22/2011 17:09'!
cut
	"Cut out the current selection and redisplay the paragraph if necessary."

	| selection |
	self lineSelectAndEmptyCheck: [^ self].
	selection _ self selection.
	self replaceSelectionWith: ''. 
	self clipboardTextPut: selection! !

!SimpleEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 9/19/2011 09:12'!
cursorEnd: aKeyboardEvent
	"Private - Move cursor end of current line."

	self
		moveCursor: [ :dummy | self endOfText ]
		forward: true
		event: aKeyboardEvent.
	^ true.! !

!SimpleEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 9/17/2010 13:42'!
cursorHome: aKeyboardEvent 

	"Private - Move cursor from position in current line to beginning of
	current line. If control key is pressed put cursor at beginning of text"

	self
		moveCursor:  [ :dummy | self beginningOfText ]
		forward: false
		event: aKeyboardEvent.
	^true! !

!SimpleEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/18/2023 11:05:18'!
backWord: aKeyboardEvent
	^false! !

!SimpleEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/19/2011 12:24'!
forwardDelete: aKeyboardEvent
	"Delete forward over the next character."

	| startIndex stopIndex |
	startIndex _ self markIndex.
	startIndex > string size ifTrue: [
		^ false].
	self hasSelection ifTrue: [
		"there was a selection"
		self replaceSelectionWith: ''.
		^ false].
	"Null selection - do the delete forward"
	stopIndex := startIndex.
	(aKeyboardEvent keyValue = 127 and: [ aKeyboardEvent shiftPressed ])
		ifTrue: [ stopIndex := (self nextWordStart: stopIndex) - 1 ].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: ''.
	self deselectAndPlaceCursorAt: startIndex.
	^false! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 22:17'!
hasSelection
	^markIndex ~= pointIndex! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/4/2008 13:57'!
markIndex
	^markIndex! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/5/2008 11:41'!
markIndex: anIndex
	markIndex _ anIndex min: string size + 1 max: 1! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 5/27/2011 15:25'!
markIndex: anIndex pointIndex: anotherIndex
	markIndex _ anIndex min: string size + 1 max: 1.
	pointIndex _ anotherIndex min: string size + 1 max: 1! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/4/2008 13:57'!
pointIndex
	^ pointIndex! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/5/2008 11:41'!
pointIndex: anIndex
	pointIndex _ anIndex min: string size + 1 max: 1! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 1/31/2013 21:09'!
selection

	| firstIndex lastIndex |
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.

	(firstIndex = 1 and: [ lastIndex = string size ])
		ifTrue: [ ^string ].

	^string copyFrom: firstIndex to: lastIndex! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/4/2008 13:55'!
startIndex
	^ pointIndex min: markIndex! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/4/2008 13:55'!
stopIndex
	^ pointIndex max: markIndex! !

!SimpleEditor methodsFor: 'private' stamp: 'jmv 9/20/2011 08:46'!
privateCurrentString
	"Answer the string I'm editing. Private. Just for internal Editor use."

	^string! !

!SimpleEditor methodsFor: 'private' stamp: 'jmv 3/16/2011 10:44'!
sameColumn: start newLine: lineBlock forward: isForward
	"See comment in other implementors."
	^start! !

!SimpleEditor methodsFor: 'accessing' stamp: 'jmv 9/6/2017 10:02:17'!
replaceSelectionWith: aString
	"Deselect, and replace the selection text by aText."

	| start stop |
	morph disablesEditing ifTrue: [
		^ self ].
	start _ self startIndex.
	stop _ self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string _ string copyReplaceFrom: start to: stop-1 with: aString.	
		self deselectAndPlaceCursorAt:  start + aString size.
		self userHasEdited  " -- note text now dirty" ]! !

!SimpleEditor methodsFor: 'accessing' stamp: 'jmv 9/22/2011 16:52'!
string
	^string! !

!SimpleEditor methodsFor: 'new selection' stamp: 'jmv 9/19/2011 09:12'!
selectFrom: start to: stop

	"Select the specified characters inclusive."
	self selectInvisiblyFrom: start to: stop! !

!SimpleEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:10'!
selectLine
	"Make the receiver's selection, if currently empty, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectAll! !

!SimpleEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/5/2024 08:49:25'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	^
	#(
		#(120		'Copy (c)' 				$c 	editor 			copySelection 					copyIcon 				'Copy selection to the Clipboard')
		#(130		'Cut (x)' 				$x 	editor 			cut 					cutIcon 				'Cut selection and store it in the Clipboard')
		#(140		'Paste (v)' 				$v 	editor 			paste 					pasteIcon 				'Paste Clipboard contents')
		"Keyboard shortcuts only"
		#(999 		nil 				$a 	editor 			selectAll 					nil 				'Select all')
	).
! !

