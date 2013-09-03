'From Cuis 4.2 of 25 July 2013 [latest update: #1812] on 2 September 2013 at 10:18:00.915685 am'!

!Object methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:09'!
isSequenceableCollection
	"Return true if the receiver is some sort of Collection that has a well-defined order associated with its elements"
	^false! !


!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 9/2/2013 08:52'!
asStringWithNewline
	"Convert to a string with returns between items.  Elements are usually strings.
	 Useful for labels for PopUpMenus.
	#('something' 'there') asStringWithNewline
	"
	
	^String streamContents: [ :labelStream |
		self do: [ :each |
			each isString
				ifTrue: [ labelStream nextPutAll: each; newLine ]
				ifFalse: [
					each printOn: labelStream.
					labelStream newLine ]].
		self size > 0 ifTrue: [ labelStream skip: -1 ]]! !

!SequenceableCollection methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:09'!
isSequenceableCollection
	"Return true if the receiver is some sort of Collection that has a well-defined order associated with its elements"
	^true! !


!DateAndTime methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:19'!
is: aSymbol
	^ aSymbol == #DateAndTime or: [ super is: aSymbol ]! !


!Morph methodsFor: 'structure' stamp: 'jmv 9/2/2013 09:39'!
owningWindow
	"Return the first enclosing morph that is a kind of Window, or nil if none"

	^ self firstOwnerSuchThat: [ :m | m is: #SystemWindow ]! !


!PluggableButtonMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:33'!
is: aSymbol
	^ aSymbol == #PluggableButtonMorph or: [ super is: aSymbol ]! !


!PluggableListMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:28'!
is: aSymbol
	^ aSymbol == #PluggableListMorph or: [ super is: aSymbol ]! !


!Rectangle methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:15'!
is: aSymbol
	^ aSymbol == #Rectangle or: [ super is: aSymbol ]! !


!Object class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 09:01'!
readFrom: aStream
	"Create an object based on the contents of aStream."

	| object ok |
	ok _ (aStream is: #Stream) or: [ aStream isString ].
	(ok or: [ aStream is: #Text ]) ifFalse: [ ^ self error: 'expected String or Text' ].
	object _ Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [ self error: self name, ' expected' ].
	^ object! !


!Browser methodsFor: 'initialize-release' stamp: 'jmv 9/2/2013 09:27'!
setClass: aBehavior selector: aSymbol
	"Set the state of a new, uninitialized Browser."

	| isMeta aClass messageCatIndex |
	aBehavior ifNil: [^ self].
	aBehavior isMeta
		ifTrue: [
			isMeta _ true.
			aClass _ aBehavior soleInstance]
		ifFalse: [
			isMeta _ false.
			aClass _ aBehavior].
	self selectCategoryForClass: aClass.
	self classListIndex: (
		(SystemOrganization listAtCategoryNamed: selectedSystemCategory)
			indexOf: aClass name).
	self metaClassIndicated: isMeta.
	aSymbol ifNil: [^ self].
	messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: (messageCatIndex > 0
		ifTrue: [messageCatIndex + 1]
		ifFalse: [0]).
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex: (
		(aBehavior organization listAtCategoryNumber: messageCatIndex)
			indexOf: aSymbol).! !


!ClassDescription methodsFor: 'organization' stamp: 'jmv 9/2/2013 08:48'!
category: cat 
	"Categorize the receiver under the system category, cat, removing it from 
	any previous categorization."

	| oldCat |
	oldCat _ self category.
	(cat isString)
		ifTrue: [SystemOrganization classify: self name under: cat asSymbol]
		ifFalse: [self errorCategoryName].
	SystemChangeNotifier uniqueInstance classRecategorized: self from: oldCat to: cat asSymbol! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 9/2/2013 08:57'!
fileOutPool: aPool onFileStream: aFileStream 

	| aPoolName |
	(aPool  isKindOf: SharedPool class) ifTrue:[^self notify: 'we do not fileout SharedPool type shared pools for now'].
	aPoolName _ Smalltalk keyAtIdentityValue: aPool.
	Transcript newLine; show: aPoolName.
	aFileStream nextPutAll: 'Transcript show: ''' , aPoolName , '''; newLine!!'; newLine.
	aFileStream nextPutAll: 'Smalltalk at: #' , aPoolName , ' put: Dictionary new!!'; newLine.
	aPool keys sort do: [ :aKey | | aValue |
		aValue _ aPool at: aKey.
		aFileStream nextPutAll: aPoolName , ' at: #''' , aKey asString , '''', ' put:  '.
		aValue isNumber
			ifTrue: [ aValue printOn: aFileStream ]
			ifFalse: [
				aFileStream nextPutAll: '('.
				aValue printOn: aFileStream.
				aFileStream nextPutAll: ')' ].
		aFileStream nextPutAll: '!!'; newLine ].
	aFileStream newLine! !


!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 9/2/2013 08:48'!
removeClassChanges: classOrClassName
	"Remove all memory of changes associated with this class"
	| cname |
	cname _ classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	changeRecords removeKey: cname ifAbsent: nil.
	self noteClassForgotten: cname! !

!ChangeSet methodsFor: 'private' stamp: 'jmv 9/2/2013 08:53'!
changeRecorderFor: classOrClassName

	| cname |
	cname _ classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	"Later this will init the changeRecords so according to whether they should be revertable."
	^ changeRecords at: cname
			ifAbsent: [
				^ changeRecords at: cname
					put: (ClassChangeRecord new initFor: cname revertable: false)]! !


!CodePackage methodsFor: 'dependencies' stamp: 'jmv 9/2/2013 09:03'!
externalCallers
	^ self 
		externalRefsSelect: [ :literal | literal isSymbol ]
		thenCollect: [ :l | l ].! !


!Collection methodsFor: 'testing' stamp: 'jmv 9/2/2013 09:30'!
includesSubstringAnywhere: testString
	"Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"
	self do: [ :element |
		element isString
			ifTrue: [
				(element includesSubString: testString) ifTrue: [^ true]].
		element isCollection
			ifTrue: [
				(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
	^ false

"#(first (second third) ((allSentMessages ('Elvis' includes:)))) includesSubstringAnywhere:  'lvi'"! !


!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 9/2/2013 09:10'!
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceableCollection ifFalse: [^ false].
	(size _ self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true! !

!SequenceableCollection methodsFor: 'private' stamp: 'jmv 9/2/2013 10:15'!
copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens
	"Answer a copy of the receiver in which all occurrences of
	oldSubstring have been replaced by newSubstring.
	ifTokens (valid for Strings only) specifies that the characters
	surrounding the recplacement must not be alphanumeric.
		Bruce Simth,  must be incremented by 1 and not 
	newSubstring if ifTokens is true.  See example below. "

	| aString startSearch currentIndex endIndex |
	(ifTokens and: [ self isString not ])
		ifTrue: [(self is: #Text) ifFalse: [
			self error: 'Token replacement only valid for Strings']].
	aString _ self.
	startSearch _ 1.
	[(currentIndex _ aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue: 
		[endIndex _ currentIndex + oldSubstring size - 1.
		(ifTokens not
			or: [(currentIndex = 1
					or: [(aString at: currentIndex-1) isGenerallyValidInIdentifiers not])
				and: [endIndex = aString size
					or: [(aString at: endIndex+1) isGenerallyValidInIdentifiers not]]])
			ifTrue: [aString _ aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newSubstring.
				startSearch _ currentIndex + newSubstring size]
			ifFalse: [
				ifTokens 
					ifTrue: [startSearch _ currentIndex + 1]
					ifFalse: [startSearch _ currentIndex + newSubstring size]]].
	^ aString

"Test case:
	'test te string' copyReplaceAll: 'te' with: 'longone' asTokens: true   "
! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 09:05'!
intern: aStringOrSymbol

	^(self lookup: aStringOrSymbol)
		ifNil:
			[
				NewSymbols add:
					(aStringOrSymbol isSymbol
						ifTrue: [ aStringOrSymbol ]
						ifFalse: [ (self new: aStringOrSymbol size) string: aStringOrSymbol ])
			]! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 09:01'!
initialFont: aStrikeFont stringOrText: aStringOrText
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	(aStringOrText is:# Text) ifTrue: [
		^ aStringOrText font: aStrikeFont ].
	
	^self string: aStringOrText attributes: {TextFontFamilyAndSize
		familyName: aStrikeFont familyName
		pointSize: aStrikeFont pointSize }! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 9/2/2013 08:53'!
obsoleteMethodReferences
	"Smalltalk obsoleteMethodReferences"
	"Smalltalk browseObsoleteMethodReferences"
	"Open a browser on all referenced behaviors that are obsolete"
	| obsClasses obsRefs references |
	references _ WriteStream on: Array new.
	obsClasses _ self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
		from: 1 to: obsClasses size during:[:bar|
	obsClasses keysAndValuesDo:[:index :each|
		bar value: index.
		obsRefs _ self pointersTo: each except: obsClasses.
		obsRefs do:[:ref|
			"Figure out if it may be a global"
			((ref isVariableBinding) and:[ ref key isString "or Symbol" ]) ifTrue:[
				(self pointersTo: ref) do:[:meth|
					(meth isKindOf: CompiledMethod) ifTrue:[
						meth methodReference ifNotNil: [ :mref|
							references nextPut: mref]]]]]].
	].
	^references contents! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 09:10'!
colorFrom: parm
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm is: #Color) ifTrue: [ ^ parm ].
	parm isSymbol ifTrue: [ ^ self perform: parm ].
	(parm isSequenceableCollection and: [ parm size > 0 ])
		ifTrue: [
			firstParm _ parm first.
			firstParm isNumber ifTrue: [
				^ self fromRgbTriplet: parm].
			aColor _ self colorFrom: firstParm.
			parm doWithIndex: [ :sym :ind |
				ind > 1 ifTrue: [
					aColor _ aColor perform: sym ]].
			^ aColor].
	^ parm

"
Color colorFrom: #(blue darker)
Color colorFrom: Color blue darker
Color colorFrom: #blue
Color colorFrom: #(0.0 0.0 1.0)
"! !

!Color class methodsFor: 'class initialization' stamp: 'jmv 9/2/2013 09:25'!
named: newName put: aColor
	"Add an access message and a class variable for aColor.  The newName should start with a lowercase letter.  (The class variable will start with an uppercase letter.)  (Color colorNames) returns a list of all color names.  "
	| str cap sym accessor csym |
	(aColor is: #Color) ifFalse: [^ self error: 'not a Color'].
	str _ newName asString.
	sym _ str asSymbol.
	cap _ str capitalized.
	csym _ cap asSymbol.
	(self class canUnderstand: sym) ifFalse: [
		"define access message"
		accessor _ str, String newLineString, String tab, '^', cap.
		self class compile: accessor
			classified: 'named colors'].
	(self classPool includesKey: csym) ifFalse: [
		self addClassVarName: cap].
	^ self classPool at: csym put: aColor! !


!Cursor class methodsFor: 'current cursor' stamp: 'jmv 9/2/2013 09:24'!
currentCursor: aCursor 
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor _ aCursor.
	Preferences biggerCursors
		ifTrue: [
			[ ^aCursor asBigCursor beCursor] 
				on: Error do: nil  "fall through "].
	aCursor beCursor! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 9/2/2013 09:20'!
- operand
	"operand is a DateAndTime or a Duration"

	(operand is: #DateAndTime)
		ifTrue: [
			| lticks rticks |
			lticks _ self asLocal ticks.
			rticks _ operand asLocal ticks.
			^Duration
 				seconds: (Time secondsInDay *(lticks first - rticks first)) + 
							(lticks second - rticks second)
 				nanoSeconds: (lticks third - rticks third) ].
	
	^self + operand negated! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 9/2/2013 09:20'!
< comparand
	"comparand conforms to protocol DateAndTime,
	or can be converted into something that conforms."
	| lticks rticks |
	self assert: (comparand is: #DateAndTime).
	offset = comparand offset
		ifTrue: [
			lticks _ self ticks.
			rticks _ comparand ticks]
		ifFalse: [
			lticks _ self asUTC ticks.
			rticks _ comparand asUTC ticks].
	^ lticks first < rticks first
		or: [
			lticks first > rticks first
				ifTrue: [ false ]
				ifFalse: [
					lticks second < rticks second
						or: [
							lticks second > rticks second
								ifTrue: [ false ]
								ifFalse: [ lticks third < rticks third ]]]]! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 9/2/2013 09:20'!
to: anEnd
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #DateAndTime).
	^ Timespan starting: self ending: anEnd! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 9/2/2013 09:20'!
to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #DateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: (Array with: aDuration);
		yourself.
! !


!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 9/2/2013 09:20'!
start: aDateAndTime
	"Store the start DateAndTime of this timespan"

	self assert: (aDateAndTime is: #DateAndTime).
	start _ aDateAndTime! !

!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 9/2/2013 09:20'!
to: anEnd
	"Answer an Timespan. anEnd must be aDateAndTime"

	self assert: (anEnd is: #DateAndTime).
	^ Timespan starting: self start ending: anEnd! !


!Timespan class methodsFor: 'squeak protocol' stamp: 'jmv 9/2/2013 09:20'!
starting: aDateAndTime duration: aDuration
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (aDateAndTime is: #DateAndTime).
	^ self basicNew
 		start: aDateAndTime;
		duration: aDuration;
		yourself! !

!Timespan class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 09:20'!
starting: startDateAndTime ending: endDateAndTime
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (startDateAndTime is: #DateAndTime).
	self assert: (endDateAndTime is: #DateAndTime).
	^ self 
		starting: startDateAndTime 
		duration: endDateAndTime - startDateAndTime! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/2/2013 09:40'!
closeWindowFor: aKeyboardEvent
	"If aKeyboardEvent is cmd/alt-w, close active window"
	(aKeyboardEvent keyValue = $w asciiValue and: [ aKeyboardEvent commandAltKeyPressed ])
		ifTrue: [
			self owningWindow ifNotNil: [ :w |
				w delete.
				^true ]].
	^ false! !

!Morph methodsFor: 'user interface' stamp: 'jmv 9/2/2013 09:40'!
activateWindowAndSendTopToBack: aBoolean

	self owningWindow ifNotNil: [ :w |
		w isTopWindow ifFalse: [
			w activateAndSendTopToBack: aBoolean]]! !


!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 9/2/2013 09:47'!
viewBox

	^ worldState
		ifNotNil: [
			worldState viewBox ]
		ifNil: [
			self world viewBox ]! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 9/2/2013 09:40'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| itemMorph |
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^self ].
	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [ autoDeselect ]) and: [ itemMorph == selectedMorph ])
		ifTrue:  [self setSelectedMorph: nil ]
		ifFalse: [ self setSelectedMorph: itemMorph ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/2/2013 09:41'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].  "work-around the no-mans-land bug"
	"No change if model is locked"
	((autoDeselect == nil or: [ autoDeselect ]) and: [ row == self selectionIndex ])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self changeModelSelection: 0 ]]
		ifFalse: [ self changeModelSelection: row ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 9/2/2013 09:41'!
arrowKey: aChar
	"Process the up and down arrows in a list pane.  Note that the listView tells us what index variable, how to get the list, and how to move the index."
	| keyEvent oldSelection nextSelection max min howMany answer w |
	answer _ false.
	keyEvent _ aChar asciiValue.
	oldSelection _ self getCurrentSelectionIndex.
	nextSelection _ oldSelection.
	max _ self maximumSelection.
	min _ self minimumSelection.
	howMany _ self numSelectionsInView.	"get this exactly??"

	keyEvent = 31 ifTrue: [
		"down-arrow; move down one, wrapping to top if needed"
		nextSelection _ oldSelection + 1.
		nextSelection > max ifTrue: [ nextSelection _ 1 ]].

	keyEvent = 30 ifTrue: [
		"up arrow; move up one, wrapping to bottom if needed"
		nextSelection _ oldSelection - 1.
		nextSelection < 1 ifTrue: [ nextSelection _ max ]].

	keyEvent = 1 ifTrue: [ nextSelection _ 1 ].			"home"
	keyEvent = 4 ifTrue: [ nextSelection _ max ].		"end"
	keyEvent = 11 ifTrue: [ nextSelection _ min max: oldSelection - howMany ].		"page up"
	keyEvent = 12 ifTrue: [ nextSelection _ oldSelection + howMany min: max ].		"page down"
	nextSelection = oldSelection ifFalse: [
		w _ self owningWindow.
		(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.

			"Update the model in next world cycle, so user gets the immediate feedback."
			WorldState addDeferredUIMessage: [
				self changeModelSelection: nextSelection ].
			answer _ true ]].
	^answer! !

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 9/2/2013 09:41'!
keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes , aChar asLowercase asString.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (self getList copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (self getList copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	nextSelection _ self getList findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self changeModelSelection: nextSelection! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/2/2013 09:41'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
	self listMorph highlightedRow: row.

	w _ self owningWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

		"Different from PluggableListMorph. There, we highlight on mouse down, and act on mouse up.
		Here, we act on mouse down, because we support dragging of selection, so mouse up will
		only happen after user is finished dragging. In order to get the highlight visible for the user,
		update the model on next world cycle."
		WorldState addDeferredUIMessage: [
			"Set or clear new primary selection (listIndex)"
			dragOnOrOff
				ifTrue: [self changeModelSelection: row]
				ifFalse: [self changeModelSelection: 0].
			"Need to restore the old one, due to how model works, and set new one."
			oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
			self listSelectionAt: row put: dragOnOrOff ].
	].
	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/2/2013 09:41'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (localEventPosition y < 0 and: [ scrollBar value > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: 0@0) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ scrollBar value < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [ self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self changeModelSelection: row ]
		ifFalse: [ self changeModelSelection: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !


!SystemWindow methodsFor: 'panes' stamp: 'jmv 9/2/2013 09:28'!
setUpdatablePanesFrom: getSelectors
	| aList aPane possibles |
	"Set my updatablePanes inst var to the list of panes which are list panes with the given get-list selectors.  Order is important here!!  Note that the method is robust in the face of panes not found, but a warning is printed in the transcript in each such case"

	aList _ OrderedCollection new.
	possibles _ OrderedCollection new.
	self allMorphsDo: [ :pane | 
		(pane is: #PluggableListMorph) ifTrue: [
			possibles add: pane.
		].
	].

	getSelectors do: [:sel | 
		aPane _ possibles detect: [ :pane | pane getListSelector == sel] ifNone: nil.
		aPane
			ifNotNil:
				[aList add: aPane]
			ifNil:
				[Transcript newLine; show: 'Warning: pane ', sel, ' not found.']].
	updatablePanes _ aList asArray! !


!CodeWindow methodsFor: 'accessing' stamp: 'jmv 9/2/2013 09:33'!
buttonWithSelector: aSelector 
	"If receiver has a control button with the given action selector answer it, else answer nil.  morphic only at this point"

	^self 
		findDeepSubmorphThat: [ :m |
			(m is: #PluggableButtonMorph) and: [ m actionSelector == aSelector ]]
		ifAbsent: [ ^nil ]! !


!ParseNode methodsFor: 'printing' stamp: 'jmv 9/2/2013 09:10'!
nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := WriteStream on: (String new: 500)].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString withBlanksCondensed.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; newLine.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		var isSequenceableCollection ifTrue: [
				var do: [:aNode | 
					(aNode respondsTo: #asReturnNode) ifTrue: [
						aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm
! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 9/2/2013 08:52'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skip: -1])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // Preferences standardMenuFont height - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset asStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 08:51'!
labelArray: labelArray lines: lineArray
	"Answer an instance of me whose items are in labelArray, with lines 
	drawn after each item indexed by anArray. 2/1/96 sw"

	labelArray isEmpty ifTrue: [self error: 'Menu must not be zero size'].
	^ self
		labels: (String streamContents: 
			[:stream |
			labelArray do: [:each | stream nextPutAll: each; newLine].
			stream skip: -1 "remove last newline"])
		lines: lineArray

"Example:
	(PopUpMenu labelArray: #('frog' 'and' 'toad') lines: #()) startUp"! !

!PopUpMenu class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 08:51'!
withCaption: cap chooseFrom: labels
	"Simply put up a menu.  Get the args in the right order with the caption first.  labels may be either an array of items or a string with CRs in it.  May use backslashes for returns."

	^ (labels isString
		ifTrue: [ self labels: labels withNewLines lines: nil ]
		ifFalse: [ self labelArray: labels lines: nil ])
			startUpWithCaption: cap withNewLines! !


!SelectionMenu class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 08:52'!
labels: labels lines: linesArray
	"Answer an instance of me whose items are in labels, with lines drawn  
	after each item indexed by linesArray. Labels can be either a string 
	with embedded newline, or a collection of strings."

	^labels isString
		ifTrue: [ super labels: labels lines: linesArray ]
		ifFalse: [ self labelArray: labels lines: linesArray ]! !


!Preference methodsFor: 'initialization' stamp: 'jmv 9/2/2013 09:04'!
name: aName defaultValue: aValue helpString: aString categoryList: aList changeInformee: informee changeSelector:  aChangeSelector
	"Initialize the preference from the given values.  There is an extra tolerence here for the symbols #true, #false, and #nil, which are interpreted, when appropriate, as meaning true, false, and nil"

	name _ aName asSymbol.
	defaultValue _ aValue.
	defaultValue = #true ifTrue: [ defaultValue _ true ].
	defaultValue = #false ifTrue: [ defaultValue _ false ].
	value _ defaultValue.
	helpString _ aString.
	categoryList _ (aList ifNil: [OrderedCollection with: #unclassified]) collect:
		[:elem | elem asSymbol].

	changeInformee _ (informee == nil or: [informee == #nil])
						ifTrue: [nil]
						ifFalse:	[(informee isSymbol)
							ifTrue: [
								Smalltalk at: informee]
							ifFalse: [
								informee]].
	changeSelector  _ aChangeSelector! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 9/2/2013 09:17'!
expandBy: delta 
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 9/2/2013 09:17'!
extendBy: delta 
	"Answer a Rectangle with the same origin as the receiver, but whose corner is offset by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin
				corner: corner + delta ]! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 9/2/2013 09:16'!
insetBy: delta 
	"Answer a Rectangle that is inset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin + delta origin 
				corner: corner - delta corner ]
		ifFalse: [
			Rectangle
				origin: origin + delta 
				corner: corner - delta ]! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 9/2/2013 09:17'!
outsetBy: delta 
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/2/2013 09:05'!
fillIn: aMenu from: dataForMenu
	"A menu constructor utility by RAA.  dataForMenu is a list of items which mean:
			nil								Indicates to add a line

			first element is symbol		Add updating item with the symbol as the wording selector
			second element is a list		second element has the receiver and selector

			first element is a string		Add menu item with the string as its wording
			second element is a list		second element has the receiver and selector

			a third element exists			Use it as the balloon text
			a fourth element exists		Use it as the enablement selector (updating case only)"

	| item |

	dataForMenu do: [ :itemData |
		itemData ifNil: [aMenu addLine] ifNotNil: [
			item _ itemData first isSymbol
				ifTrue: [
					aMenu 
						addUpdating: itemData first 
						target: self 
						selector: #doMenuItem:with: 
						argumentList: {itemData second}]
				 ifFalse: [
					aMenu 
						add: itemData first
						target: self 
						selector: #doMenuItem:with: 
						argumentList: {itemData second}].
			itemData size >= 3 ifTrue: [
				aMenu balloonTextForLastItem: itemData third.
			itemData size >= 4 ifTrue: [
				item enablementSelector: itemData fourth ]]]].

	^ aMenu! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 9/2/2013 08:58'!
eval: aString
	"Evaluate the string in a neutral context, and under certain circumstances print the 
	result in the transcript"

	| result |
	result _ Compiler new evaluate: aString in: nil to: nil.
	result isNumber | result isString
		ifTrue: [
			Transcript newLine; nextPutAll: result printString]! !

!methodRemoval: Morph #ownerThatIsA:!
Morph removeSelector: #ownerThatIsA:!
!methodRemoval: Morph #pasteUpMorph!
Morph removeSelector: #pasteUpMorph!
!methodRemoval: SequenceableCollection #asStringWithCr!
SequenceableCollection removeSelector: #asStringWithCr!
