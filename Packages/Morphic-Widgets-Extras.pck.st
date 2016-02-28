'From Cuis 4.2 of 25 July 2013 [latest update: #2696] on 28 February 2016 at 9:49:12.799072 am'!
'Description Please enter a description for this package'!
!provides: 'Morphic-Widgets-Extras' 1 4!
!classDefinition: #SimpleEditor category: #'Morphic-Widgets-Extras'!
Editor subclass: #SimpleEditor
	instanceVariableNames: 'string pointIndex markIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'SimpleEditor class' category: #'Morphic-Widgets-Extras'!
SimpleEditor class
	instanceVariableNames: ''!

!classDefinition: #MagnifierMorph category: #'Morphic-Widgets-Extras'!
BorderedRectMorph subclass: #MagnifierMorph
	instanceVariableNames: 'magnification trackPointer lastPos srcExtent auxCanvas magnifiedForm'
	classVariableNames: 'RecursionLock'
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'MagnifierMorph class' category: #'Morphic-Widgets-Extras'!
MagnifierMorph class
	instanceVariableNames: ''!

!classDefinition: #LimitedHeightTextMorph category: #'Morphic-Widgets-Extras'!
TextModelMorph subclass: #LimitedHeightTextMorph
	instanceVariableNames: 'maxHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'LimitedHeightTextMorph class' category: #'Morphic-Widgets-Extras'!
LimitedHeightTextMorph class
	instanceVariableNames: ''!

!classDefinition: #DragAndDropAreaMorph category: #'Morphic-Widgets-Extras'!
RectangleLikeMorph subclass: #DragAndDropAreaMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'DragAndDropAreaMorph class' category: #'Morphic-Widgets-Extras'!
DragAndDropAreaMorph class
	instanceVariableNames: ''!

!classDefinition: #PartsBinMorph category: #'Morphic-Widgets-Extras'!
DragAndDropAreaMorph subclass: #PartsBinMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'PartsBinMorph class' category: #'Morphic-Widgets-Extras'!
PartsBinMorph class
	instanceVariableNames: ''!

!classDefinition: #FrameRateMorph category: #'Morphic-Widgets-Extras'!
RectangleLikeMorph subclass: #FrameRateMorph
	instanceVariableNames: 'lastStepDelta meanStepDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'FrameRateMorph class' category: #'Morphic-Widgets-Extras'!
FrameRateMorph class
	instanceVariableNames: ''!

!classDefinition: #FunctionGraphMorph category: #'Morphic-Widgets-Extras'!
RectangleLikeMorph subclass: #FunctionGraphMorph
	instanceVariableNames: 'xMin xMax yMin yMax functions colors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'FunctionGraphMorph class' category: #'Morphic-Widgets-Extras'!
FunctionGraphMorph class
	instanceVariableNames: ''!

!classDefinition: #MinimalStringMorph category: #'Morphic-Widgets-Extras'!
RectangleLikeMorph subclass: #MinimalStringMorph
	instanceVariableNames: 'font emphasis contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'MinimalStringMorph class' category: #'Morphic-Widgets-Extras'!
MinimalStringMorph class
	instanceVariableNames: ''!

!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets-Extras'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showTextCursor pauseBlinking textCursorRect keyboardFocusWatcher crAction'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'OneLineEditorMorph class' category: #'Morphic-Widgets-Extras'!
OneLineEditorMorph class
	instanceVariableNames: ''!

!classDefinition: #UpdatingStringMorph category: #'Morphic-Widgets-Extras'!
StringMorph subclass: #UpdatingStringMorph
	instanceVariableNames: 'target getSelector stepTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets-Extras'!
!classDefinition: 'UpdatingStringMorph class' category: #'Morphic-Widgets-Extras'!
UpdatingStringMorph class
	instanceVariableNames: ''!


!SimpleEditor commentStamp: '<historical>' prior: 0!
See comment in Editor!

!MagnifierMorph commentStamp: '<historical>' prior: 0!
Provides a magnifying glass. Magnifies the morphs below (if grabbed) or the area around the mouse pointer.!

!LimitedHeightTextMorph commentStamp: '<historical>' prior: 0!
A TextMorph that will expand and contract vertically to adjust for the contents, but limited to a specific max height. If contents are larger, a scrollbar will be used.

LimitedHeightTextMorph new
	maxHeight: 48;
	model: (TextModel new contents: 'This is some text to test the morph.');
	openInWorld!

!DragAndDropAreaMorph commentStamp: '<historical>' prior: 0!
A simple morph that allows dragging submorphs, and dropping other morphs on it. It is provided as an example, and also as a reference of the methods you might need to implement in your morphs to handle drag and drop.

| m |
m _ DragAndDropAreaMorph new.
m openInWorld.
m addMorph: EllipseMorph new.
m morphExtent: 320@240!

!PartsBinMorph commentStamp: '<historical>' prior: 0!
Like DragAndDropAreaMorph, but when submorphs are dragged, get a copy and not the original one.

| m |
m _ PartsBinMorph new.
m openInWorld.
m addMorph: EllipseMorph new.
m morphExtent: 120@240!

!FrameRateMorph commentStamp: 'jmv 6/11/2012 10:14' prior: 0!
A very simple morph to demo stepping, and for knowing about stepping (and world update) frame rates.

FrameRateMorph new openInHand!

!FunctionGraphMorph commentStamp: 'jmv 7/1/2015 11:28' prior: 0!
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
(g embeddedInMorphicWindowLabeled: 'graph') openInWorld

| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x rem: d ] color: Color green.
g addFunction: [ :x | x quo: d ] color: Color red.
g openInWorld

| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
!

!MinimalStringMorph commentStamp: 'jmv 4/1/2009 21:43' prior: 0!
A greatly reduced StringMorph. May be of some use.!

!OneLineEditorMorph commentStamp: '<historical>' prior: 0!
A plain text editor for Morphic. Handles only one line. Does not handle fonts/styles, alignment, Smalltalk utilities and any other advanced stuff in TextModelMorph. Just a simple text editor.

Can optionally include a crAction: a zero argument closure, to be evaluated on Cr keystroke.!

!UpdatingStringMorph commentStamp: 'jmv 1/5/2013 23:49' prior: 0!
UpdatingStringMorph new
     target: [self runningWorld activeHand morphPosition asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld!

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

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 11/4/2008 13:08'!
changeString: aString
	"Install aString as the one to be edited by the receiver."

	string _ aString.
	self resetState! !

!SimpleEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:42'!
clickAndHalf

	| here |
	here _ self pointIndex.
	(here between: 2 and: string size)
		ifTrue: [ self selectWord ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ].! !

!SimpleEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:37'!
copySelection
	"Copy the current selection and store it in the Clipboard, unless empty."

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection! !

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

!SimpleEditor methodsFor: 'menu messages' stamp: 'jmv 9/22/2011 17:09'!
cut
	"Cut out the current selection and redisplay the paragraph if necessary."

	| selection |
	self lineSelectAndEmptyCheck: [^ self].
	selection _ self selection.
	self replaceSelectionWith: ''. 
	self clipboardTextPut: selection! !

!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 6/10/2012 11:14'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."
	| asciiValue |
	asciiValue _ aKeyboardEvent keyValue.
	"Control keys are handled by #shortcuts even if they have any modifiers"
	(asciiValue >= 32 and: [
		aKeyboardEvent commandAltKeyPressed ]) ifTrue: [
		^self perform: (self cmdShortcuts at: asciiValue + 1) with: aKeyboardEvent ].

	"We don't support multiple lines. Therefore, we don't process return as a #normalCharacter:"
	aKeyboardEvent isReturnKey ifTrue: [
		^ true].

	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent! !

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

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 11/4/2008 13:42'!
initialize
	"Initialize the state of the receiver. Subclasses should include 'super 
	initialize' when redefining this message to insure proper initialization."

	super initialize.
	self changeString: ''! !

!SimpleEditor methodsFor: 'testing' stamp: 'jmv 11/17/2008 17:01'!
isAtEnd
	^pointIndex = (string size + 1)! !

!SimpleEditor methodsFor: 'testing' stamp: 'jmv 11/17/2008 17:01'!
isAtStart
	^pointIndex = 1! !

!SimpleEditor methodsFor: 'editing keys' stamp: 'jmv 9/2/2009 12:40'!
makeCapitalized: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	| prev |
	prev _ $-.  "not a letter"
	self replaceSelectionWith:
		(self selection collect: [:c |
			prev _ prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).
	^ true! !

!SimpleEditor methodsFor: 'editing keys' stamp: 'jmv 12/19/2011 12:22'!
makeLowercase: aKeyboardEvent
	"Force the current selection to lowercase.  Triggered by Cmd-X."

	self replaceSelectionWith: self selection asLowercase.
	^ true! !

!SimpleEditor methodsFor: 'editing keys' stamp: 'jmv 12/19/2011 12:22'!
makeUppercase: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-Y."

	self replaceSelectionWith: self selection asUppercase.
	^ true! !

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

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/4/2008 13:57'!
pointIndex
	^ pointIndex! !

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/5/2008 11:41'!
pointIndex: anIndex
	pointIndex _ anIndex min: string size + 1 max: 1! !

!SimpleEditor methodsFor: 'private' stamp: 'jmv 9/20/2011 08:46'!
privateCurrentString
	"Answer the string I'm editing. Private. Just for internal Editor use."

	^string! !

!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 9/19/2011 09:47'!
processKeyStroke: aKeyboardEvent

	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		^self].
	self markIndex: self pointIndex! !

!SimpleEditor methodsFor: 'accessing' stamp: 'jmv 2/5/2015 00:46'!
replaceSelectionWith: aString
	"Deselect, and replace the selection text by aText."

	| start stop |
	morph disablesEdition ifTrue: [
		^ self ].
	start _ self startIndex.
	stop _ self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string _ string copyReplaceFrom: start to: stop-1 with: aString.	
		self deselectAndPlaceCursorAt:  start + aString size.
		self userHasEdited  " -- note text now dirty" ]! !

!SimpleEditor methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:37'!
resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex _ 1.	"Like pointBlock in TextEditor"
	markIndex _ 1.	"Like markBlock in TextEditor"! !

!SimpleEditor methodsFor: 'private' stamp: 'jmv 3/16/2011 10:44'!
sameColumn: start newLine: lineBlock forward: isForward
	"See comment in other implementors."
	^start! !

!SimpleEditor methodsFor: 'new selection' stamp: 'jmv 9/19/2011 09:12'!
selectFrom: start to: stop

	"Select the specified characters inclusive."
	self selectInvisiblyFrom: start to: stop! !

!SimpleEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:10'!
selectLine
	"Make the receiver's selection, if currently empty, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectAll! !

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

!SimpleEditor methodsFor: 'accessing' stamp: 'jmv 9/22/2011 16:52'!
string
	^string! !

!SimpleEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:07'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$a 	#selectAll:				'Select all')

		#(		$x 	#cut:						'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:					'Paste Clipboard contents')
		#(		$ 	#selectWord:			'Select the current word as with double clicking')
	)! !

!MagnifierMorph methodsFor: 'menu' stamp: 'di 9/28/1999 23:06'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu
		addLine;
		add: 'magnification...' action: #chooseMagnification;
		addUpdating: #trackingPointerString action: #toggleTrackingPointer;
		addUpdating: #toggleRoundString action: #toggleRoundness.! !

!MagnifierMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:28'!
borderWidth: anInteger
	"Grow outwards preserving innerBounds"
	| c |  
	c _ self referencePosition.
	super borderWidth: anInteger.
	super morphExtent: self defaultExtent.
	self referencePosition: c.! !

!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 8/21/2012 21:53'!
chooseMagnification
	| result |
	result _ (SelectionMenu selections: #(1.5 2 4 8))
		startUpWithCaption: 'Choose magnification
(currently ', magnification printString, ')'.
	(result == nil or: [ result = magnification ]) ifTrue: [ ^ self ].
	magnification _ result.
	self morphExtent: extent. "round to new magnification"
	self redrawNeeded. "redraw even if extent wasn't changed"! !

!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 9/22/2012 15:35'!
chooseMagnification: evt
	| handle origin aHand currentMag |
	currentMag _ magnification.
	aHand _ evt ifNil: [ self world activeHand ] ifNotNil: [evt hand].
	origin _ aHand morphPosition y.
	handle _ HandleMorph new forEachPointDo:
		[ :newPoint | self magnification: (newPoint y - origin) / 8.0 + currentMag ].
	aHand attachMorph: handle.
	handle startStepping.
	self redrawNeeded. ! !

!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:20'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1! !

!MagnifierMorph methodsFor: 'geometry' stamp: 'bf 9/21/1999 09:22'!
defaultExtent
	^(srcExtent * magnification) truncated + (2 * borderWidth)! !

!MagnifierMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:43'!
drawOn: aCanvas
	RecursionLock == self ifFalse: [
		super drawOn: aCanvas.		"border and fill"
		aCanvas image: self magnifiedForm at: borderWidth@borderWidth]! !

!MagnifierMorph methodsFor: 'event handling testing' stamp: 'jmv 1/17/2013 17:27'!
handlesMouseDown: aMouseButtonEvent

	^aMouseButtonEvent mouseButton2Pressed! !

!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 10:02'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	self morphExtent: 128@128! !

!MagnifierMorph methodsFor: 'round view' stamp: 'jmv 12/10/2010 09:25'!
isRound

	^ "owner isMemberOf: ScreeningMorph" false! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 12/16/2011 10:28'!
magnification: aNumber
	| c |  
	magnification _ aNumber min: 8 max: 0.5.
	magnification _ magnification roundTo:
		(magnification < 3 ifTrue: [0.5] ifFalse: [1]).
	srcExtent _ srcExtent min: (512@512) // magnification. "to prevent accidents"
	c _ self referencePosition.
	super morphExtent: self defaultExtent.
	self referencePosition: c.! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 10/1/2014 09:47'!
magnifiedForm
	| srcRect form neededExtent |
	lastPos _ self sourcePoint.
	srcRect _ self sourceRectFrom: lastPos.
	((srcRect intersects: self morphBoundsInWorld) and: [ RecursionLock == nil ])
		ifTrue: [
			RecursionLock _ self.
			"try to reuse form if appropriate"
			auxCanvas _ (auxCanvas notNil and: [ auxCanvas extent = srcExtent ])
				ifTrue: [
					"Just in case we go out of the Display"
					srcRect origin > (0@0) ifFalse: [
						auxCanvas form fillBlack ].
					BitBltCanvas on: auxCanvas form over: srcRect ]
				ifFalse: [ BitBltCanvas depth: 32 over: srcRect ].
			auxCanvas fullDraw: self world.
			form _ auxCanvas form.
			RecursionLock _ nil]
		ifFalse: [
			"cheaper method if the source is not occluded"
			form _ Display copy: srcRect].
	"smooth if non-integer scale"
	neededExtent _ (srcExtent * magnification ) truncated.
	(magnifiedForm isNil or: [ magnifiedForm extent ~=  neededExtent ])
		ifTrue: [ magnifiedForm _ Form extent: neededExtent depth: 32 ].
	(WarpBlt toForm: magnifiedForm)
		sourceForm: form;
		colorMap: (form colormapIfNeededFor: magnifiedForm);
		cellSize: (magnification isInteger ifTrue: [1] ifFalse: [2]);  "installs a new colormap if cellSize > 1"
		combinationRule: 3;
		copyQuad: form boundingBox innerCorners toRect: magnifiedForm boundingBox.
	^magnifiedForm.! !

!MagnifierMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:45'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition

	self chooseMagnification: aMouseButtonEvent! !

!MagnifierMorph methodsFor: 'geometry' stamp: 'jmv 6/6/2014 18:06'!
privateExtent: aPoint
	"Round to multiples of magnification"

	srcExtent _ (aPoint - (2 * borderWidth)) // magnification.
	^ super privateExtent: self defaultExtent! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 9/25/2012 22:24'!
sourcePoint
	"If we are being dragged use our center, otherwise use pointer position"
	^ (trackPointer not or: [owner notNil and: [owner is: #HandMorph]])
		ifTrue: [ self morphBoundsInWorld center ]
		ifFalse: [ self world activeHand morphPosition ]! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 8/4/2012 16:41'!
sourceRect
	"world global coordinates, etc"
	self flag: #jmvVer2.
	^self sourceRectFrom: self sourcePoint! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 8/17/2012 18:52'!
sourceRectFrom: aPoint
	^ (aPoint extent: srcExtent) translatedBy: (srcExtent // -2) + 1.
! !

!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 19:53'!
stepAt: millisecondSinceLast

	self redrawNeeded! !

!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:32'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 20! !

!MagnifierMorph methodsFor: 'round view' stamp: 'di 9/28/1999 23:17'!
toggleRoundString
	^ self isRound
		ifTrue: ['be square']
		ifFalse: ['be round']! !

!MagnifierMorph methodsFor: 'round view' stamp: 'jmv 12/11/2011 23:35'!
toggleRoundness
	| |
"	w _ self world.
	self isRound
		ifTrue: [owner delete.
				w addMorph: self]
		ifFalse: [sm _ ScreeningMorph new position: self zzposition.
				sm addMorph: self.
				sm addMorph: (EllipseMorph newBounds: self bounds).
				w addMorph: sm]
			"! !

!MagnifierMorph methodsFor: 'menu' stamp: 'bf 9/20/1999 15:48'!
toggleTrackingPointer
	trackPointer _ trackPointer not! !

!MagnifierMorph methodsFor: 'menu' stamp: 'bf 9/20/1999 15:49'!
trackingPointerString
	^trackPointer
		ifTrue: ['stop tracking pointer']
		ifFalse: ['start tracking pointer']! !

!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:58'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !

!LimitedHeightTextMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 21:59'!
initialize
	super initialize.
	extent _  200 @ 120! !

!LimitedHeightTextMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 14:20'!
innerHeight: aNumber
	"Adjust height and scrollbar to the new contents height."
	self morphHeight: (aNumber + 10 min: maxHeight)! !

!LimitedHeightTextMorph methodsFor: 'accessing' stamp: 'jmv 11/14/2011 16:32'!
maxHeight: aNumber
	maxHeight _ aNumber! !

!LimitedHeightTextMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:12'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			maxHeight _ extent y ]; yourself! !

!LimitedHeightTextMorph class methodsFor: 'instance creation' stamp: 'jmv 11/14/2011 16:32'!
initializedInstance
	^self new
		maxHeight: 48;
		model: (TextModel new contents: 'This is some text to test the morph.')! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."

	^self "Grab me"! !

!DragAndDropAreaMorph methodsFor: 'drag and drop other morphs' stamp: 'jmv 1/24/2013 22:52'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. Redefined to answer true."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop other morphs' stamp: 'jmv 1/24/2013 22:52'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. Redefined to answer true."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
justDroppedInto: newOwnerMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."

	^super justDroppedInto: newOwnerMorph event: anEvent ! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
justGrabbedFrom: formerOwner
	"The receiver was just grabbed from its former owner and is now attached to the hand."
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."! !

!DragAndDropAreaMorph methodsFor: 'drag and drop other morphs' stamp: 'jmv 1/24/2013 22:52'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.
	This method just answers super. It is included here to say it is relevant to D&D behavior."

	^super wantsDroppedMorph: aMorph event: evt! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph. This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out whether they like each other."
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."

	^true! !

!PartsBinMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 2/22/2013 12:49'!
aboutToGrab: aMorph
	"submorph is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^aMorph copy! !

!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2014 09:22'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [
		aCanvas drawString: lastStepDelta rounded printString at: 0@0 font: AbstractFont default color: Color black.
		aCanvas drawString: meanStepDelta rounded printString at: 0@14 font: AbstractFont default color: Color black.
		"aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: AbstractFont default color: Color black "
		]! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 19:52'!
stepAt: millisecondSinceLast

	| n |
	lastStepDelta _ millisecondSinceLast.
	"This factor is a damper, to show a sort of mean of the n latest step deltas"
	meanStepDelta
		ifNil: [ meanStepDelta _ 0. n _ 0 ]
		ifNotNil: [
"			n _ (meanStepDelta / lastStepDelta between: 0.5 and: 2)
				ifTrue: [ 10 ]
				ifFalse: [10 ]."
			n _ 20 ].
	meanStepDelta _ meanStepDelta * n + lastStepDelta / (n+1).
	self redrawNeeded! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:34'!
stepTime

	^20! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:20'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true! !

!FunctionGraphMorph methodsFor: 'accessing' stamp: 'jmv 11/18/2014 08:52'!
addFunction: aOneArgBlock color: aColor

	functions add: aOneArgBlock.
	colors add: aColor! !

!FunctionGraphMorph methodsFor: 'accessing' stamp: 'jmv 12/23/2014 20:34'!
domain: anInterval

	xMin _ anInterval first.
	xMax _ anInterval last.

	yMin _ anInterval first.
	yMax _ anInterval last.! !

!FunctionGraphMorph methodsFor: 'drawing' stamp: 'jmv 11/18/2015 09:59'!
drawOn: aCanvas
	"
	| g |
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: 1 ] color: Color green.
	g addFunction: [ :x | x div: 1 ] color: Color red.
	g openInWorld
	"
	| r y r2 |
	r _ self morphLocalBounds.
	aCanvas
		fillRectangle: r
		color: (Color gray: 0.9).
	aCanvas
		frameAndFillRectangle: r fillColor: (Color gray: 0.9)
		borderWidth: 0.05 borderColor: Color white.
	aCanvas line: (self toMorphic:0@yMin)-(0@2) to: (self toMorphic: 0 @ yMax)+(0@2) width: 2 color: Color lightGray.
	aCanvas line: (self toMorphic: xMin@0)+(2@0) to: (self toMorphic: xMax@0)-(2@0) width: 2 color: Color lightGray.

	xMin*1.0 to: xMax count: 600 do: [ :x |
		functions with: colors do: [ :f :c |
			y _ f value: x.
			(y between: yMin and: yMax) ifTrue: [
				r2 _ (self xToMorphic: x)@(self yToMorphic: y) extent: 5.
				(r containsRect: r2) ifTrue: [
					aCanvas
						fillRectangle: r2
						color: c ]]]]! !

!FunctionGraphMorph methodsFor: 'initialization' stamp: 'jmv 7/1/2015 11:16'!
initialize
	super initialize.
	extent _ 320@240.
	functions _ OrderedCollection new.
	colors _ OrderedCollection new! !

!FunctionGraphMorph methodsFor: 'geometry' stamp: 'jmv 5/17/2015 09:20'!
toMorphic: aPoint
	^(self xToMorphic: aPoint x) @ (self yToMorphic: aPoint y)! !

!FunctionGraphMorph methodsFor: 'geometry' stamp: 'jmv 5/17/2015 09:17'!
xToMorphic: x
	^x - xMin / (xMax - xMin ) * extent x! !

!FunctionGraphMorph methodsFor: 'geometry' stamp: 'jmv 5/17/2015 09:16'!
yToMorphic: y
	^yMax - y / (yMax - yMin ) * extent y! !

!FunctionGraphMorph class methodsFor: 'as yet unclassified' stamp: 'dhn 4/12/2015 15:31'!
example2
"
	self  example2
"
| g |
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x cos ] color: Color green.
g addFunction: [ :x | (x + 1) squared - 3 ] color: Color red. "parabola with vertex at (-1,-3)"
g openInWorld
! !

!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 10:58'!
contents: newContents 
	contents _ (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			 newContents string]
		ifFalse: [
			contents = newContents ifTrue: [^self].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

!MinimalStringMorph methodsFor: 'initialization' stamp: 'cbr 12/3/2010 23:29'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current text! !

!MinimalStringMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:41'!
drawOn: aCanvas

	aCanvas drawString: contents at: 0@0 font: self fontToUse color: color! !

!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 1/1/2015 21:16'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: 0@0! !

!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:22'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [AbstractFont default].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]! !

!MinimalStringMorph methodsFor: 'initialization' stamp: 'jmv 9/10/2010 22:55'!
initWithContents: aString font: aFont emphasis: emphasisCode 
	self initialize.
	
	font _ aFont.
	emphasis _ emphasisCode.
	self contents: aString! !

!MinimalStringMorph methodsFor: 'initialization' stamp: 'jmv 9/10/2010 22:55'!
initialize
	super initialize.
	font _ nil.
	emphasis _ 0! !

!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 12/20/2010 16:25'!
measureContents
	| f |
	f _ self fontToUse.
	^(((f widthOfString: contents) max: 3)  @ f height).! !

!MinimalStringMorph class methodsFor: 'instance creation' stamp: 'jmv 4/3/2011 22:34'!
contents: aString
	" 'StringMorph contents: str' is faster than 'StringMorph new contents: str' 
	(MinimalStringMorph contents: 'Some string') openInWorld
	"
	^ self contents: aString font: nil! !

!MinimalStringMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2008 12:56'!
contents: aString font: aFont
	^ self basicNew initWithContents: aString font: aFont emphasis: 0! !

!MinimalStringMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2008 12:56'!
contents: aString font: aFont emphasis: emphasisCode
	^ self basicNew initWithContents: aString font: aFont emphasis: emphasisCode! !

!MinimalStringMorph class methodsFor: 'instance creation' stamp: 'jmv 4/3/2011 22:34'!
new
	^self contents: 'some string'! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:23'!
baseFont

	font ifNil: [ font _ AbstractFont default ].
	^font! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 21:54'!
characterIndexAtPoint: aPoint

	| line block f |
	f _ self fontToUse.
	
	line _ TextLine 
		start: 1
		stop: contents size
		internalSpaces: 0
		paddingWidth: 0.
	line
		rectangle: (0@0 extent: extent);
		lineHeight: f height baseline: f ascent.
		
	block _ (CharacterBlockScanner new text: 
			(contents asText font: font))
		characterBlockAtPoint: aPoint index: nil
		in: line.

	^ block stringIndex! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:43'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	self handleInteraction: [ self editor clickAndHalf ]! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 3/16/2011 10:34'!
contents
	^contents! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 3/16/2011 10:41'!
contents: newContents 
	contents _ (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			 newContents string]
		ifFalse: [
			contents = newContents ifTrue: [^self].	"no substantive change"
			newContents].
	editor _ nil.
	self fitContents.
	self redrawNeeded! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 1/5/2013 14:23'!
crAction
	"Answer the optional Cr action"
	^crAction! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 1/5/2013 14:24'!
crAction: aBlock
	
	crAction := aBlock! !

!OneLineEditorMorph methodsFor: 'initialization' stamp: 'cbr 12/3/2010 23:29'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current text! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 3/6/2015 08:54'!
disableEdition
	self setProperty: #disablesEdition toValue: true.
	self stopBlinking! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 3/6/2015 08:53'!
disablesEdition

	^self hasProperty: #disablesEdition! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 22:37'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas
	| textCursorColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
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

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:42'!
drawOn: aCanvas
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	self hasTextCursor ifTrue: [ self drawTextCursorOn: aCanvas ].
	aCanvas
		drawString: contents
		at: 0@0
		font: self fontToUse
		color: color! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2013 20:35'!
drawSelectionOn: aCanvas
	| rightX leftX bottom |

	bottom _ self baseFont height.
	leftX _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	leftX _ leftX min: extent x.
	rightX _ self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.
	rightX _ rightX min: extent x.

	aCanvas
		fillRectangle: (leftX @ 0 corner: rightX @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 22:19'!
drawTextCursorOn: aCanvas
	|  bottom x |

	showTextCursor ifTrue: [
		bottom _ self baseFont height.
		x _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayTextCursorAtX: x top: 0 bottom: bottom emphasis: emphasis on: aCanvas ]! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 12/4/2011 22:25'!
editor
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditor ].
	^editor! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 1/1/2015 21:16'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: 0@0! !

!OneLineEditorMorph methodsFor: 'events-processing' stamp: 'jmv 12/28/2011 22:45'!
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

!OneLineEditorMorph methodsFor: 'events-processing' stamp: 'KenD 10/29/2015 20:43'!
fontPreferenceChanged

	font := nil.
	self baseFont.! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 11/5/2008 13:18'!
fontToUse

	^ (emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ self baseFont ]
		ifFalse: [ self baseFont emphasized: emphasis ]! !

!OneLineEditorMorph methodsFor: 'editing' stamp: 'jmv 10/16/2013 21:03'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the composition"

	"Also couple the editor to Morphic keyboard events"

	| oldEditor oldContents |
	oldEditor _ editor.
	oldContents _ contents.
	interactionBlock value.
	oldContents == contents 
		ifTrue: [ "this will not work if the composition changed"
			editor _ oldEditor	"since it may have been changed while in block"].
	self redrawNeeded! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesKeyboard

	^self visible! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesMouseOver: evt
	"implements #mouseEnter: and/or #mouseLeave:"
	^true! !

!OneLineEditorMorph methodsFor: 'testing' stamp: 'jmv 11/5/2008 12:41'!
hasSelection

	^editor notNil and: [editor hasSelection]! !

!OneLineEditorMorph methodsFor: 'testing' stamp: 'jmv 10/16/2013 22:18'!
hasTextCursor

	^ self hasKeyboardFocus and: [editor notNil and: [editor hasSelection not]]! !

!OneLineEditorMorph methodsFor: 'unaccepted edits' stamp: 'jmv 3/14/2011 09:21'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	
	self flag: #jmv.
	"Not used in this morph, as it doesn't do accept / cancel"
	self redrawNeeded! !

!OneLineEditorMorph methodsFor: 'initialization' stamp: 'jmv 11/5/2008 12:15'!
initWithContents: aString font: aFont emphasis: emphasisCode 
	self initialize.
	
	font _ aFont.
	emphasis _ emphasisCode.
	self contents: aString! !

!OneLineEditorMorph methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:19'!
initialize
	super initialize.
	contents _ ''.
	font _ nil.
	emphasis _ 0.
	showTextCursor _ false. "Should never be nil"! !

!OneLineEditorMorph methodsFor: 'private' stamp: 'jmv 12/4/2011 22:25'!
installEditor
	"Install an editor for my contents.  This constitutes 'hasFocus'.
	If priorEditor is not nil, then initialize the new editor from its state.
	We may want to rework this so it actually uses the prior editor."

	editor _ SimpleEditor new morph: self.
	editor changeString: contents.
	self redrawNeeded.
	^editor! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'len 11/15/2015 06:45'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	"Return - check for special action
	Note: Code below assumes that this was some
	input field reacting on Return."
	aKeyboardEvent isReturnKey ifTrue: [
		crAction ifNotNil: [
			"Break the keyboard focus so that the receiver can be safely deleted."
			"Is this needed at all? (jmv)"
			"aKeyboardEvent hand newKeyboardFocus: nil."
			^crAction value ]].

	self pauseBlinking.
	self handleInteraction: [ self editor processKeyStroke: aKeyboardEvent ].
	self updateFromContents.
	super keyStroke: aKeyboardEvent  "sends to keyStroke event handler, if any"! !

!OneLineEditorMorph methodsFor: 'events-processing' stamp: 'jmv 3/6/2015 08:59'!
keyboardFocusChange: aBoolean 
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor ].	"Forces install"
			self selectAll.
			self showsBlinkingCursor ifTrue: [
				self startBlinking ]]
		ifFalse: [
			self stopBlinking.
			keyboardFocusWatcher ifNotNil: [
				keyboardFocusWatcher lostFocus: self ]].
	self redrawNeeded! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 3/17/2011 07:58'!
keyboardFocusWatcher: aMorph
	"We are usually used as a part of a bigger morph.
	Usually, that morph would be interested in us changing keyboard focus.
	An alternative implementation would be to define a new type of event, but:
		- I (jmv) prefer explicit message sends to registering in events.
		- There are too many evens already defined. It would be good to reduce that."
	keyboardFocusWatcher _ aMorph! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 12/20/2010 16:25'!
measureContents
	| f |

	f _ self fontToUse.
	^((f widthOfString: contents) max: 3)  @ f height.! !

!OneLineEditorMorph methodsFor: 'geometry' stamp: 'jmv 3/6/2015 08:52'!
minimumExtent

	^ self measureContents! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:27'!
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

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:16'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking
! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 13:20'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self ].
	self handleInteraction: [
		self editor mouseMove: aMouseMoveEvent index: (self characterIndexAtPoint: localEventPosition) ]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 7/18/2014 14:53'!
onBlinkCursor
	"Blink the cursor"
	showTextCursor _ showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 3/6/2015 08:59'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	self showsBlinkingCursor ifTrue: [
		"Show cursor right now if needed"
		showTextCursor ifFalse: [
			showTextCursor _ true.
			textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]]! !

!OneLineEditorMorph methodsFor: 'typing/selecting keys' stamp: 'jmv 3/16/2011 10:45'!
selectAll
	self editor selectAll.
	self redrawNeeded! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 3/6/2015 08:56'!
showsBlinkingCursor

	^self handlesKeyboard and: [ self disablesEdition not ]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 2/2/2014 22:20'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	showTextCursor _ true.
	self startStepping: #onBlinkCursor stepTime: 500! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 7/18/2014 14:53'!
stopBlinking
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	"Hide cursor right now if needed"
	showTextCursor ifTrue: [
		showTextCursor _ false.
		textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]! !

!OneLineEditorMorph methodsFor: 'private' stamp: 'jmv 12/29/2011 15:05'!
updateFromContents

	"O contents deberia ser un StringHolder????"
	contents _ self editor string.
	self redrawNeeded! !

!OneLineEditorMorph class methodsFor: 'instance creation' stamp: 'jmv 3/16/2011 10:25'!
contents: aString
	" 'StringMorph contents: str' is faster than 'StringMorph new contents: str' 
	(OneLineEditorMorph contents: 'Some string') openInWorld
	"
	^ self contents: aString font: nil! !

!OneLineEditorMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2008 12:47'!
contents: aString font: aFont
	^ self basicNew initWithContents: aString font: aFont emphasis: 0! !

!OneLineEditorMorph class methodsFor: 'instance creation' stamp: 'jmv 11/4/2008 12:47'!
contents: aString font: aFont emphasis: emphasisCode
	^ self basicNew initWithContents: aString font: aFont emphasis: emphasisCode! !

!OneLineEditorMorph class methodsFor: 'instance creation' stamp: 'jmv 4/3/2011 22:35'!
new
	^self contents: 'some string'! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
getSelector: aSymbol
	getSelector _ aSymbol! !

!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 09:23'!
initialize
	super initialize.
	target _ self.
	getSelector _ #contents.
	stepTime _ 50! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast

	self contents: (target perform: getSelector)! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:18'!
stepTime

	^stepTime! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
stepTime: aNumber
	stepTime _ aNumber! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
target: anObject
	target _ anObject! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 1/4/2013 13:18'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true! !

!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 4/15/2014 09:26'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (AbstractFont familyName: 'DejaVu' pointSize: 22) 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !
