'From Cuis6.3 [latest update: #6272] on 14 March 2024 at 10:51:56 am'!

!Browser methodsFor: 'breakpoints' stamp: 'jmv 3/14/2024 10:09:31'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ (BreakingMethodWrapper on: selectedMethod) install ].
	self
		changed: #messageList;
		changed: #annotation! !

!Browser methodsFor: 'message category functions' stamp: 'jmv 3/14/2024 10:32:52'!
addExtensionCategory
	"Present a choice package names and let the user enter a new category suffx.
	Take the suffix form the selected category."

	| packageNames packageNameIndex |
	
	selectedClassName ifNil: [ ^self ].

	packageNames := (CodePackage installedPackages collect: [ :package |
		package packageName ]) sorted.
	packageNames ifEmpty: [ ^ self ].
	
	packageNameIndex := PopUpMenu 
		withCaption: 'Which package to extend?'
		chooseFrom: packageNames.
	packageNameIndex = 0 ifTrue: [ ^ self ].

	self 
		request: 'Enter the category name'
		initialAnswer: 
			(String streamContents: [ :stream |
				stream
					nextPut: $*;
					nextPutAll: (packageNames at: packageNameIndex).
				selectedMessageCategory ifNotNil: [
					stream
						nextPut: $-;
						nextPutAll: selectedMessageCategory ] ])
		 verifying: [ :newCategoryName | 
			((newCategoryName at: 1) = $* or: [
				self confirm:
					"I know about with #withNewlines ..."
					`'The selected category does not start with an asterix (*).',
					String newLineString,
					'Do you want to use it anyway?'` ]) and: [
						(packageNames includes:				(newCategoryName copyUpTo: $-) allButFirst) or: [
							self confirm:
								`'The selected prefix does not match any package name.', 
								String newLineString,
								'Do you want to use it anyway?'` ] ] ]
		do: [ :newCategoryName | | oldMessageCategoryListIndex |
			oldMessageCategoryListIndex := self messageCategoryListIndex.
			self classOrMetaClassOrganizer
				addCategory: newCategoryName
				before: selectedMessageCategory.
			self changed: #messageCategoryList.
			self messageCategoryListIndex:
				(oldMessageCategoryListIndex = 0
					ifTrue: [ self classOrMetaClassOrganizer categories size + 1 ]
					ifFalse: [ oldMessageCategoryListIndex ]).
			self changed: #messageCategoryList ]! !


!SinglePackageBrowser methodsFor: 'system category functions' stamp: 'jmv 3/14/2024 10:09:42'!
newSystemCategoryNameInitialAnswer
	"Make a suggestion for a new sytems category.
	
	Provide something more usefull, e.g. the package name or a sensible derivate of it."

	| packageName | 
	^ (self systemCategoryList includes: (packageName := package packageName))
		ifTrue: [ packageName, ' - Sub-Category-Name' ]
		ifFalse: [ packageName ]
! !


!MethodSet methodsFor: 'breakpoints' stamp: 'jmv 3/14/2024 10:09:53'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ (BreakingMethodWrapper on: selectedMethod) install ].
	self
		changed: #messageList;
		changed: #annotation! !


!Debugger methodsFor: 'breakpoints' stamp: 'jmv 3/14/2024 10:10:01'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ (BreakingMethodWrapper on: selectedMethod) install ].
	self changed: #annotation! !


!FileList methodsFor: 'own services' stamp: 'jmv 3/14/2024 10:10:12'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"

	| fileEntry fileContents |
	fileEntry := directory // fileName.
	fileContents := fileEntry textContents.

	((Workspace new contents: fileContents) 
		openLabel: 'Workspace from ', fileName)
			setProperty: #lastFilenameUsedToSave
			toValue: fileEntry pathName! !


!Float methodsFor: 'comparing' stamp: 'jmv 3/14/2024 10:10:20'!
isWithin: anInteger floatsFrom: aNumber

	^(self floatsAwayFrom: aNumber) abs <= anInteger! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 3/14/2024 10:11:24'!
fromArray: colorDef

	| answer |
	colorDef size = 0 ifTrue: [^ `Color transparent` ].
	(colorDef size between: 3 and: 4) ifFalse: [self error: 'Undefined color definition'].
	answer := self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3).
	^colorDef size = 3
		ifTrue: [answer]
		ifFalse: [answer alpha: (colorDef at: 4)]! !


!Random methodsFor: 'accessing' stamp: 'jmv 3/14/2024 10:14:16'!
nextBits: anInteger
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| remainder answer |
	anInteger negative ifTrue: [self error: 'invalid interval'].
	remainder := anInteger \\ self nextChunkSize.
	answer := remainder > 0
		ifTrue: [self nextChunkBits bitShift: remainder - self nextChunkSize]
		ifFalse: [0].
	anInteger // self nextChunkSize timesRepeat:
		[answer := (answer bitShift: self nextChunkSize) bitXor: self nextChunkBits].
	^answer! !


!LaggedFibonacciRandom methodsFor: 'private' stamp: 'jmv 3/14/2024 10:14:27'!
nextChunkBits
	"This method generates random instances of Integer in the interval
	0 to 16r1FFFFFFFFFFFFF (53 bits)."
	
	^(self next timesTwoPower: self nextChunkSize) truncated! !


!ParkMiller88Random methodsFor: 'private' stamp: 'jmv 3/14/2024 10:15:42'!
privateNextChunk
	"This method generates random instances of Integer in the interval
	1 to 16r7FFFFFFF (almost 31 bits).  Note the calculations cannot
	result in seed = 0 because a is a primitive generator of the integers
	modulo m."

	seed := (seed asFloat * self a \\ self m) truncated.
	^seed! !


!SymbolSet methodsFor: 'lookup' stamp: 'jmv 3/14/2024 10:16:01'!
size

	^buckets inject: 0 into: [ :t :x | (x count: [:s | s notNil]) + t ]! !


!String class methodsFor: 'services' stamp: 'jmv 3/14/2024 10:19:02'!
string: aString lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| charSetLineEnders start end endWithoutDelimiters |
	charSetLineEnders := `(ByteArray new: 256) at: Character cr asciiValue + 1 put: 1; at: Character lf asciiValue + 1 put: 1; yourself`.
	start := 1.
	[
		end := aString indexOfAnyIn: charSetLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters := end := aString size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^aString ].

		"Account for CrLf sequence"
		endWithoutDelimiters := end - 1.
		(end < aString size
			and: [(aString basicAt: end + 1) = `Character lf asciiValue`
			and: [(aString basicAt: end) = `Character cr asciiValue` ]])
				ifTrue: [ end := end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start := end + 1 ] repeat! !

!String class methodsFor: 'services' stamp: 'jmv 3/14/2024 10:19:11'!
string: aByteStringOrByteArray withLineEndings: newLineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf charSetLineEnders inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr := Character cr.
	lf := Character lf.
	charSetLineEnders := `(ByteArray new: 256) at: Character cr asciiValue + 1 put: 1; at: Character lf asciiValue + 1 put: 1; yourself`.
	(aByteStringOrByteArray is: #ByteArray)
		ifTrue: [
			cr := cr asciiValue.
			lf := lf asciiValue ].

	inPos := 1.
	outPos := 1.
	lineEndingSize := newLineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString := aByteStringOrByteArray class new: aByteStringOrByteArray size * lineEndingSize.

	[
		lineEndPos := aByteStringOrByteArray indexOfAnyIn: charSetLineEnders startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos := outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: aByteStringOrByteArray startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (newLineEndingString at: i).
				newOutPos := newOutPos + 1 ].
			outPos := newOutPos.
			((aByteStringOrByteArray at: lineEndPos) = cr and: [
					lineEndPos < aByteStringOrByteArray size and: [ (aByteStringOrByteArray at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos := lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos := lineEndPos + 1 ]].

	"no more line endings.  copy the rest"
	newOutPos := outPos + (aByteStringOrByteArray size - inPos).
	outString replaceFrom: outPos to: newOutPos with: aByteStringOrByteArray startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !

!String class methodsFor: 'services' stamp: 'jmv 3/14/2024 10:19:29'!
substringsIn: aByteStringOrByteArray
	"Answer an array of the substrings that compose the argument.
	If argument is an UTF-8 encoded ByteArray, answer is an array of same."

	| end beginning charSetNonSeparators charSetSeparators |
	charSetSeparators := `(ByteArray new: 256)
		at: Character space asciiValue + 1 put: 1;
		at: Character tab asciiValue + 1 put: 1;
		at: Character lf asciiValue + 1 put: 1;
		at: Character cr asciiValue + 1 put: 1;
		at: Character newPage asciiValue + 1 put: 1;
		yourself`.
	charSetNonSeparators := `(ByteArray new: 256) += 1;
		at: Character space asciiValue + 1 put: 0;
		at: Character tab asciiValue + 1 put: 0;
		at: Character lf asciiValue + 1 put: 0;
		at: Character cr asciiValue + 1 put: 0;
		at: Character newPage asciiValue + 1 put: 0;
		yourself`.
	end := 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning := aByteStringOrByteArray indexOfAnyIn: charSetNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end := aByteStringOrByteArray indexOfAnyIn: charSetSeparators startingAt: beginning ifAbsent: [ aByteStringOrByteArray size + 1 ].
			end := end - 1.
			result nextPut: (aByteStringOrByteArray copyFrom: beginning to: end).
		]
	].! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 3/14/2024 10:21:34'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |
	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		(string at: index) = $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			(key size > 2 and: [ (key at: 1) = $+ ])
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := Character codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsability to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) = $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `Character codePoint: 16r300` ] -> [ '`' ].
				[ `Character codePoint: 16r301` ] -> [ '''' ].
				[ `Character codePoint: 16r302` ] -> [ '^' ].
				[ `Character codePoint: 16r303` ] -> [ '~' ].
				[ `Character codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !


!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'jmv 3/14/2024 10:21:46'!
temporaryToInstanceVariable

	self
		withNodeUnderCursorDo: [ :nodeUnderCursor |
			(nodeUnderCursor isTemp and: [nodeUnderCursor isArg not])
				ifTrue: [ (TemporaryToInstanceVariableApplier on: self for: nodeUnderCursor name) value ]
				ifFalse: [ morph flash ]]
		ifAbsent: [ morph flash ].! !


!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 3/14/2024 10:21:55'!
fileOutsDirectory
	^ (DirectoryEntry userBaseDirectory / 'FileOuts') assureExistence! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 3/14/2024 10:22:05'!
logsDirectory
	^ (DirectoryEntry userBaseDirectory / 'Logs') assureExistence! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 3/14/2024 10:22:13'!
newPackagesDirectory
	^ (DirectoryEntry userBaseDirectory / 'NewPackages') assureExistence! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 3/14/2024 10:22:20'!
savedChangeSetsDirectory
	^ (DirectoryEntry userBaseDirectory / 'ChangeSets') assureExistence! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 3/14/2024 10:22:28'!
userChangesDirectory
	^ (DirectoryEntry userBaseDirectory / 'UserChanges') assureExistence! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 3/14/2024 10:24:58'!
promptUserForSize: currentSize fontFamily: fontFamily
	| menu |
	menu := MenuMorph entitled: 'Font Sizes'.
	menu defaultTarget: menu.
	fontFamily pointSizes do: [ :pt | | itemText sizeToUse |
		itemText := pt printString.
		sizeToUse := pt.
		(menu
			add: (pt = currentSize) asMenuItemTextPrefix, itemText
			action: #modalSelection:
			argument: pt)
				font: (fontFamily atPointSize: sizeToUse) ].
	^ menu invokeModal! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 3/14/2024 10:25:22'!
iconsExample
	"
	LayoutMorph iconsExample
	"
	| lm ilm counter |
	lm := LayoutMorph newColumn.
	ilm := LayoutMorph newRow.
	counter := 1.
	Theme methodsDo: [ :method | | sel | 
		(sel := method selector) numArgs = 0 ifTrue: [
			(sel endsWith: 'Icon') ifTrue: [ | mayBeForm |
				mayBeForm := Theme current perform: sel.
				(mayBeForm isKindOf: Form) ifTrue: [ | imageMorph |
					counter \\ 10 = 0 ifTrue: [
						lm addMorph: ilm.
						ilm := LayoutMorph newRow ].
					counter := counter + 1.
					ilm addMorph: (imageMorph := ImageMorph new image: (mayBeForm magnifyBy: 2)).
					imageMorph 
						setBalloonText: 'Copy ''', sel storeString bold, ''' to the Clipboard';
						setProperty: #'handlesMouseDown:' toValue: true;
						setProperty: #mouseButton1Down:localPosition:  toValue: [ :event :pos |
							Clipboard storeObject: sel storeString ] ] ] ] ].
	lm 
		setProperty: #taskbarButtonIcon toValue: Theme current imageIcon;
		setBalloonText: 'Drag me here!!'.
	lm openInWorld
! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 3/14/2024 10:25:45'!
addTextPane
	"Add a text pane and two buttons."
	
	| controlsContainer |
	textMorph := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			textMorph emptyTextDisplayMessage: msg ].
	textMorph
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LayoutMorph newRow)
		addMorphUseAll: textMorph;
		addMorph: 
			((PluggableButtonMorph model: textMorph scroller action: #acceptContents)
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			((PluggableButtonMorph model: self action: #cancel)
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 3/14/2024 10:25:54'!
processKeystrokeEvent: evt
	| action |

	(acceptOn = #enter and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.
	
	"Return - check for special action"
	evt isReturnKey ifTrue: [	
		action := self crAction.
		action ifNotNil: [ ^action value ] ].
	
	"Esc - check for special action"
	evt isEsc ifTrue: [			
		action := self escAction.
		action ifNotNil: [ ^action value ] ].
	
	self handleInteraction: [ editor processKeystrokeEvent: evt ].	
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].

	"Only accept (on any) if we have unaccepted edits, otherwise the morph will flash."
	(hasUnacceptedEdits and: [ acceptOn == #any ]) ifTrue: [ self acceptContents ].! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 3/14/2024 10:26:23'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Create a copy of me or my submenu."

	aMouseButtonEvent shiftPressed ifTrue: [
		(subMenu
			ifNil: [ self duplicate ]
			ifNotNil: [
				(subMenu duplicate)
					addTitle: contents;
					addStayUpIcons;
					stayUp;
					adjustSubmorphsLayout;
					yourself ])
						openInHand ].
	^ super mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/14/2024 10:26:43'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
		
	deleteReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete')
							setBalloonText: 'Remove selected Feature requirement'.
	deleteReqButton color: self widgetsColor.
	updateReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update')
							setBalloonText: 'Update requirement to current Feature revision'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #leftOrTop);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #rightOrBottom);
		color: `Color transparent`;
		yourself
		! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 3/14/2024 10:27:51'!
directoryTreeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu := MenuMorph new defaultTarget: model.
	(aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon)
			setBalloonText: 'Delete the selected directory'.
	(aMenu
		add: model showHidden asMenuItemTextPrefix, 'show hidden folders and files'
		action: #toggleShowHidden)
			setBalloonText: 'show or hide files and directories that begin with a dot'.
	initialDirectoriesMenu := MenuMorph new.
	#(
		(cuis  		'Cuis top' 'Use the default Cuis top directory')
		(roots  		'OS roots' 'Use the host OS root directories. Drives on Windows; "/" on Unix')
	)
		do: [ :entry |
			(initialDirectoriesMenu
				add: entry second
				target: self
				action: #initialFileListDirectories:
				argument: entry first)
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 3/14/2024 10:28:20'!
drawDownArrowOn: aCanvas thickness: scrollbarThickness

	downButtonForm ifNil: [
		downButtonForm := (ScrollBar new instVarNamed: 'downButton') imageForm: 32 ].
	aCanvas
		image: downButtonForm
		at: self downButtonPosition.
! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 3/14/2024 10:28:38'!
drawUpArrowOn: aCanvas thickness: scrollbarThickness

	upButtonForm ifNil: [
		upButtonForm := (ScrollBar new instVarNamed: 'upButton') imageForm: 32 ].
	aCanvas
		image: upButtonForm
		at: self upButtonPosition.
! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 3/14/2024 10:29:18'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph ].

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^ #rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt := false.
	grabAMorph := false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler := aMorph.
					aMorphHandlesIt := true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler := aMorph.
				aMorphHandlesIt := true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph := true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner := false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner := true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [ aMorph fullIncludesPixel: position ] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: ((Preferences at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
						clkSel: ((Preferences at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					(Preferences at: #clickGrabsMorphs) ifFalse: [
						self shiftPressed
							ifTrue: [ (aMorph is: #SystemWindow) ifTrue: [ aMorph sendToBack ] ]
							ifFalse: [ aMorph activateWindow ] ].
					self wasHandled: true.
					^ self ]]].

	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 3/14/2024 10:29:28'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	"Try to get out quickly"
	| aMorphHandlesIt handledByInner |
	"FIXME - this works in all tested cases but one: when the window directly under the mouse doesn't have keyboard focus (i.e. a Transcript window)"
	(aMorph fullIncludesPixel: position) ifFalse: [ ^ #rejected ].
	"Install the prospective handler for the receiver"
	aMorphHandlesIt := false.
	(aMorph handlesMouseScroll: self) ifTrue: [
		eventHandler := aMorph.
		aMorphHandlesIt := true ].
	"Now give submorphs a chance to handle the event"
	handledByInner := false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner := true ]]].
	(handledByInner or: [
		 (aMorph rejectsEvent: self) not and: [aMorph fullIncludesPixel: position]]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self sendEventTo: aMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.! !


!TheWorldMenu class methodsFor: 'instance creation' stamp: 'jmv 3/14/2024 10:29:38'!
getMenuIn: aWorld at: anActiveHand
	^ (self new world: aWorld 	hand: anActiveHand)
		buildWorldMenu! !


!ObjectExplorerWrapper methodsFor: 'converting' stamp: 'jmv 3/14/2024 10:30:30'!
asString
	| explorerString |
	explorerString := [ item shortPrintString ]
		on: UnhandledError 
		do: [:ex | ex return: '<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	^(itemName , ': ' , explorerString) withBlanksCondensed! !


!SelectionMenu methodsFor: 'basic control sequence' stamp: 'jmv 3/14/2024 10:30:40'!
startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean
	"Overridden to return value returned by manageMarker.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

	| index |
	index := super startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
	selections ifNil: [ ^index ].  "If there are no selections defined, show the super class' behavior."
	(index between: 1 and: selections size) ifFalse: [ ^nil ].
	^ selections at: index! !


!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'jmv 3/14/2024 10:30:48'!
addInstanceVariable

	(AddInstanceVariable named: variable to: method methodClass) apply.! !


!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'jmv 3/14/2024 10:31:01'!
assertSubclassesOf: aClass haventGotInstanceVariableNamed: anInstanceVariableName
	
	aClass allSubclassesDo: [ :subclass | 
		(subclass instVarNames includes: anInstanceVariableName) ifTrue: [
			self refactoringError: self temporaryExistsAsInstVarInSubclassesErrorDescription ]].! !

