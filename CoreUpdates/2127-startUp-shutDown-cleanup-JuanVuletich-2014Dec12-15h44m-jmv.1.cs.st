'From Cuis 4.2 of 25 July 2013 [latest update: #2126] on 12 December 2014 at 4:06:49.483106 pm'!

!InputSensor methodsFor: 'initialization' stamp: 'jmv 12/12/2014 15:56'!
shutDownSensor
	InterruptWatcherProcess ifNotNil: [
		InterruptWatcherProcess terminate.
		InterruptWatcherProcess _ nil ]! !

!InputSensor methodsFor: 'initialization' stamp: 'jmv 12/12/2014 15:49'!
startUpSensor
	self installInterruptWatcher! !


!EventSensor methodsFor: 'initialization' stamp: 'jmv 12/12/2014 15:56'!
shutDownSensor
	super shutDownSensor.
	EventTicklerProcess ifNotNil: [
		EventTicklerProcess terminate.
		EventTicklerProcess _ nil. ].
	inputSemaphore ifNotNil: [Smalltalk unregisterExternalObject: inputSemaphore]! !

!EventSensor methodsFor: 'initialization' stamp: 'jmv 12/12/2014 15:50'!
startUpSensor
	"Run the I/O process"
	self initialize.
	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	super startUpSensor.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore _ false.
	inputSemaphore initSignals! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 12/12/2014 15:49'!
startUpMenu
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil! !


!CustomMenu methodsFor: 'invocation' stamp: 'jmv 12/12/2014 15:49'!
startUpMenu
	"Build and invoke this menu with no initial selection. Answer the selection associated with the menu item chosen by the user or nil if none is chosen."

	^ self startUpWithCaption: title! !


!Browser methodsFor: 'class list' stamp: 'jmv 12/12/2014 15:51'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ RecentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUpMenu.
	className
		ifNil: [^ self].
	class := Smalltalk at: className.
	self selectCategoryForClass: class.
	self selectClass: class! !


!VersionsBrowser methodsFor: 'menu' stamp: 'jmv 12/12/2014 15:54'!
compareToOtherVersion
	"Prompt the user for a reference version, then spawn a window 
	showing the diffs between the older and the newer of the current 
	version and the reference version as text."

	| change1 change2 s1 s2 differDesc diffWords targetChanges labels label1 |
	change1 _ changeList at: listIndex ifAbsent: [ ^self ].
	label1 _ list at: listIndex.

	labels _ list reject: [ :any | any = label1 ].
	targetChanges _ changeList reject: [ :any | any = change1 ].
	change2 _ ((SelectionMenu
				labels: labels
				selections: targetChanges) startUpMenu) ifNil: [ ^self ].
	
	"compare earlier -> later"
	"change1 timeStamp < change2 timeStamp
		ifFalse: [ | temp | temp _ change1. change1 _ change2. change2 _ temp ]."

	s1 _ change1 string.
	s2 _ change2 string.
	s1 = s2
		ifTrue: [^ self inform: 'Exact Match'].

	diffWords _ self shouldDiffWords.
	differDesc _ diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].

	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: s1 to: s2
			tryWords: diffWords
			prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [classOfMethod])))
		openLabel: 
			(('Comparison from {1} to {2}: ', differDesc, 
				(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])) 
					format: { change1 stamp. change2 stamp })! !


!Behavior methodsFor: 'system startup' stamp: 'jmv 12/12/2014 15:45'!
startUp: resuming
	"This message is sent to registered classes, with resuming = true when the system is coming up,
	and with resumint = false after a snapshot (image save, no quit)."
	^ self startUp! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 12/12/2014 15:51'!
browseClassVarRefs 
	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable"

	| lines labelStream allVars index owningClasses |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	owningClasses _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var.
			owningClasses add: class].
		vars isEmpty ifFalse: [ lines add: allVars size ]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ self].
	Smalltalk browseAllCallsOn:
		((owningClasses at: index) classPool associationAt: (allVars at: index))! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 12/12/2014 15:51'!
chooseClassVarName 
	"Present the user with a list of class variable names and answer the one selected, or nil if none"

	| lines labelStream  allVars index |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ nil].
	^ allVars at: index! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 12/12/2014 15:51'!
chooseRecentClipping
	"
	Clipboard chooseRecentClipping
	"
	"Choose by menu from among the recent clippings"
	recent isEmpty ifTrue: [ ^ nil ].
	^ (SelectionMenu
		labelList:
			(recent collect: [ :txt |
				((txt asString contractTo: 50)
					withLineEndings: '\') withBlanksCondensed ])
		selections: recent) startUpMenu! !


!WeakArray class methodsFor: 'system startup' stamp: 'jmv 12/12/2014 15:46'!
startUp: resuming

	"If not a system startup (just a snapshot), no need."
	resuming ifFalse: [ ^self ].

	self restartFinalizationProcess! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 12/12/2014 15:53'!
showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption
	"Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters.  Use aCaption as the menu title, if it is not nil.  Evaluate choiceBlock if a message is chosen."
	| index menuLabels sortedList aMenu |
	sortedList _ selectorCollection asArray sort.
	menuLabels _ String streamContents: [ :strm |
		strm nextPutAll: (firstItem contractTo: 40).
		sortedList do: [ :sel |
			strm
				 newLine;
				 nextPutAll: (sel contractTo: 40) ]].
	aMenu _ PopUpMenu
		labels: menuLabels
		lines: #(1 ).
	index _ aCaption
		ifNil: [ aMenu startUpMenu ]
		ifNotNil: [ aMenu startUpWithCaption: aCaption ].
	index = 1 ifTrue: [ choiceBlock value: firstItem ].
	index > 1 ifTrue: [ choiceBlock value: (sortedList at: index - 1) ].! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 12/12/2014 09:30'!
shutDown 
	"Minimize Display memory saved in image"
	Display setExtent: 240@120 depth: Display nativeDepth! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 12/12/2014 15:53'!
offerColorMenu
	"Present a menu of available colors, and if one is chosen, apply it to the current selection."
	"This is a user command, and generates undo"

	| attribute colors index thisSel |
	thisSel _ self selection.
	colors _ #(#black #magenta #red #yellow #green #blue #cyan #white ).
	index _ (PopUpMenu
		labelArray: colors , #('choose color...' )
		lines: (Array with: colors size + 1)) startUpMenu.
	index = 0 ifTrue: [ ^ true ].
	index <= colors size
		ifTrue: [ attribute _ TextColor color: (Color perform: (colors at: index)) ]
		ifFalse: [
			index _ index - colors size - 1.
			"Re-number!!!!!!"
			index = 0 ifTrue: [ attribute _ self chooseColor ].
			thisSel ifNil: [ ^ true ]].
	attribute ifNotNil: [ self applyAttribute: attribute ].
	^ true.! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 12/12/2014 15:53'!
chooseAlignment
	"This is a user command, and generates undo"

	| aList reply  |
	aList _ #(leftFlush centered justified rightFlush).
	reply _ (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNil: [ ^self ].
	self applyAttribute: (TextAlignment perform: reply).
	morph updateFromTextComposition.
	^ true! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 12/12/2014 15:53'!
offerUndoHistory
	| index labels current |
	current _ model undoRedoCommandsPosition.
	labels _ model undoRedoCommands collectWithIndex: [ :each :i | 
		(i = current ifTrue: [ '<on>' ] ifFalse: [ '<off>' ]), each printString ].
	labels isEmpty ifFalse: [
		index _ (PopUpMenu
			labelArray: labels
			lines: #()) startUpMenu.
		index = current ifTrue: [ ^self ].
		index = 0 ifTrue: [ ^self ].
		index < current
			ifTrue: [ current - index timesRepeat: [ self undo ]]
			ifFalse: [ index - current timesRepeat: [ self redo ]]]! !


!InputSensor class methodsFor: 'system startup' stamp: 'jmv 12/12/2014 15:56'!
shutDown
	self default shutDownSensor! !

!InputSensor class methodsFor: 'system startup' stamp: 'jmv 12/12/2014 15:53'!
startUp
	
	self installMouseDecodeTable.
	self default startUpSensor! !


!EventSensor class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 15:56'!
install	"EventSensor install"
	"Install an EventSensor in place of the current Sensor."
	| newSensor |
	Sensor shutDownSensor.
	newSensor _ self new.
	newSensor startUpSensor.
	"Note: We must use #become: here to replace all references to the old sensor with the new one, since Sensor is referenced from all the existing controllers."
	Sensor becomeForward: newSensor. "done"! !


!BrowserWindow methodsFor: 'commands' stamp: 'jmv 12/12/2014 15:51'!
findClass
	"Search for a class by name."
	| pattern foundClass classNames index toMatch exactMatch potentialClassNames |

	self okToChange ifFalse: [ ^self flash ].
	pattern _ FillInTheBlankMorph request: 'Class name or fragment?'.
	pattern isEmpty ifTrue: [^ self flash].
	toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	potentialClassNames _ model potentialClassNames asOrderedCollection.
	classNames _ (pattern last = $. or: [pattern last = $ ])
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ self flash].
	exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpMenu]].
	index = 0 ifTrue: [^ self flash].
	foundClass _ Smalltalk at: (classNames at: index) asSymbol.
 	model selectCategoryForClass: foundClass.
	model selectClass: foundClass
! !


!CodeFileBrowserWindow methodsFor: 'commands' stamp: 'jmv 12/12/2014 15:51'!
findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	pattern _ (FillInTheBlankMorph request: 'Class Name?') asLowercase.
	pattern isEmpty ifTrue: [^ self].
	classNames := Set new.
	classNames addAll: model selectedCodeFile classes keys.
	classNames := classNames asArray select: 
		[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
	classNames isEmpty ifTrue: [^ self].
	index _ classNames size = 1
				ifTrue:	[1]
				ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
	index = 0 ifTrue: [^ self].
	foundCodeFile := nil.
	foundClass := nil.
		(model selectedCodeFile classes includesKey: (classNames at: index)) ifTrue:[
			foundClass := model selectedCodeFile classes at: (classNames at: index).
			foundCodeFile := model selectedCodeFile ].
	foundClass ifNotNil: [
	 	model systemCategoryListIndex: (model systemCategoryList indexOf: foundCodeFile name asSymbol).
		model classListIndex: (model classList indexOf: foundClass name) ]! !


!ChangeSorterWindow class methodsFor: 'services' stamp: 'jmv 12/12/2014 15:51'!
browseChangeSetsWithClass: class selector: selector
	"Put up a menu comprising a list of change sets that hold changes for the given class and selector.  If the user selects one, open a single change-sorter onto it"

	| hits index |
	hits _ ChangeSet allChangeSets select: [ :cs |
		(cs atSelector: selector class: class) ~~ #none].
	hits isEmpty ifTrue: [ ^ self inform: class name, '.', selector , '
is not in any change set'].
	index _ hits size = 1
		ifTrue:	[1]
		ifFalse:	[
			(PopUpMenu
				labelArray: (hits collect: [ :cs | cs name ])
				lines: #()) 
					startUpMenu ].
	index = 0 ifTrue: [ ^ self ].
	ChangeSorterWindow open: (ChangeSorter new myChangeSet: (hits at: index)) label: nil! !


!StringMorph methodsFor: 'menu' stamp: 'jmv 12/12/2014 15:53'!
changeEmphasis

	| reply aList |
	aList _ #(normal bold italic underlined struckThrough).
	reply _ (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNotNil:[
		self emphasis: (TextEmphasis perform: reply) emphasisCode.
	].
! !

!methodRemoval: CustomMenu #startUp!
CustomMenu removeSelector: #startUp!
!methodRemoval: PopUpMenu #startUp!
PopUpMenu removeSelector: #startUp!
!methodRemoval: EventSensor #shutDown!
EventSensor removeSelector: #shutDown!
!methodRemoval: EventSensor #startUp!
EventSensor removeSelector: #startUp!
!methodRemoval: InputSensor #shutDown!
InputSensor removeSelector: #shutDown!
!methodRemoval: InputSensor #startUp!
InputSensor removeSelector: #startUp!
!methodRemoval: DisplayScreen #shutDown!
DisplayScreen removeSelector: #shutDown!
!methodRemoval: Form class #shutDown!
Form class removeSelector: #shutDown!
!methodRemoval: Form #shutDown!
Form removeSelector: #shutDown!
