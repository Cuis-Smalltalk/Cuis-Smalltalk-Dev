'From Cuis 6.0 [latest update: #5335] on 30 June 2022 at 4:14:23 pm'!
Smalltalk renameClassNamed: #PreferenceNG as: #Preference!

!PreferenceSet commentStamp: '<historical>' prior: 0!
I hold a set of preferences. An application may want to instanciate me to hold its related preferences.
- contents :  a Dictionary whose keys are preference names (symbol) and value a Preference instance.!


!PluggableTextModel methodsFor: 'misc' stamp: 'jmv 6/30/2022 16:12:30'!
refetch
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ].
			^false ]
		ifNotNil: [
			self actualContents: (Text
				initialFont: (Preference at: #standardCodeFont)
				stringOrText: (textProvider perform: textGetter)).
			self changed: #refetched.
			^true ]! !


!Workspace methodsFor: 'shout styling' stamp: 'jmv 6/30/2022 16:12:30'!
shouldStyle

	^shouldStyle ifNil: [ Preference at: #shoutInWorkspaces]! !

!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/30/2022 16:12:30'!
allowStylingWithEmphasis
	"Disabled by default for faster styling of large contents, as text metrics are not affected by styling."

	^ Preference at: #stylingWithEmphasisInWorkspaces! !

!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/30/2022 16:12:30'!
fullPrintIt

	^fullPrintIt ifNil: [ Preference at: #fullPrintItInWorkspaces]! !


!TextProvider methodsFor: 'contents' stamp: 'jmv 6/30/2022 16:12:30'!
acceptedContents
	^ Text
		initialFont: (Preference at: #standardCodeFont)
		stringOrText: self acceptedStringOrText! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 6/30/2022 16:12:30'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name.
		(Preference at: #classAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#instanceMethodsCount] -> [
					strm
						print: (aClass theNonMetaClass selectors size);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (aClass theMetaClass selectors size);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (aClass theNonMetaClass linesOfCode);
						nextPutAll: ' total lines of code' ]
			}]].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 6/30/2022 16:12:31'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	^ String streamContents: [ :strm |
		(Preference at: #methodAnnotations)
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp _ self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						strm
							print: ((aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm | cm linesOfCode]);
							nextPutAll: ' lines of code' ].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount _ Smalltalk numberOfSendersOf: aSelector.
						sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
							(CodePackage packageOfMethod: cm methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#changeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ] ].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 6/30/2022 16:12:30'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		(Preference at: #systemCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#classCount] -> [
					strm
						print: (SystemOrganization listAtCategoryNamed: aCategory) size;
						nextPutAll: ' classes' ].
				[#instanceMethodsCount] -> [
					strm
						print: (SystemOrganization instanceMethodCountOf: aCategory);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (SystemOrganization classMethodCountOf: aCategory);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (SystemOrganization linesOfCodeOf: aCategory);
						nextPutAll: ' total lines of code' ]
			}]].! !

!CodeProvider methodsFor: 'contents' stamp: 'jmv 6/30/2022 16:12:30'!
contentsSymbol
	"Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source.  A nil value in the contentsSymbol slot will be set to #source by this method"

	^ contentsSymbol ifNil: [
		contentsSymbol _ (Preference at: #browseWithPrettyPrint)
					ifTrue:
						[#prettyPrint]
					ifFalse:
						[#source]]! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 6/30/2022 16:12:30'!
defaultDiffsSymbol
	"Answer the code symbol to use when generically switching to diffing"

	^ (Preference at: #diffsWithPrettyPrint)
		ifTrue: [
			#prettyLineDiffs]
		ifFalse: [
			#lineDiffs]! !


!Browser methodsFor: 'class functions' stamp: 'jmv 6/30/2022 16:12:30'!
classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	^ Text
		initialFont: (Preference at: #standardCodeFont)
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass comment ]
				ifFalse: [ '' ]).! !

!Browser methodsFor: 'class functions' stamp: 'jmv 6/30/2022 16:12:30'!
createInstVarAccessors
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage _ aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ].
			(aClass canUnderstand: (setter _ aName , ':') asSymbol) ifFalse: [
				newMessage _ setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' ' ,
					((Preference at: #leftArrowAssignmentsInGeneratedCode)
						ifTrue: [ '_' ]
						ifFalse: [ ':=' ]) , ' anObject'.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ]]]! !


!Browser class methodsFor: 'class list' stamp: 'jmv 6/30/2022 16:12:30'!
disableListClassesHierarchically
	
	^Preference at: #listClassesHierarchically put: false! !

!Browser class methodsFor: 'class list' stamp: 'jmv 6/30/2022 16:12:31'!
enableListClassesHierarchically
	
	^Preference at: #listClassesHierarchically put: true! !

!Browser class methodsFor: 'class list' stamp: 'jmv 6/30/2022 16:12:29'!
listClassesHierarchically
	
	^Preference at: #listClassesHierarchically! !


!CodeFileBrowser methodsFor: 'edit pane' stamp: 'jmv 6/30/2022 16:12:30'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	answer _ class sourceCodeAt: selector.
	(self classOrMetaClassOrganizer isRemoved: selector) ifTrue: [
		^ Text
			string: answer
			attribute: TextColor red ].
	(Preference at: #browseWithPrettyPrint) ifTrue: [
		answer _ class compilerClass new
						format: answer in: class notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		answer _ self
			methodDiffFor: answer
			selector: self selectedMessageName ].
	^ answer! !


!ChangeList methodsFor: 'initialization-release' stamp: 'jmv 6/30/2022 16:12:30'!
initialize
	"Initialize a blank ChangeList.  Set the contentsSymbol to reflect whether diffs will initally be shown or not"

	contentsSymbol _ (Preference at: #diffsInChangeList)
		ifTrue:
			[self defaultDiffsSymbol]
		ifFalse:
			[#source].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	super initialize! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/30/2022 16:12:30'!
compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change class s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifTrue: [
			s1 _ (class sourceCodeAt: change methodSelector) asPlainString.
			s2 _ change string.
			s1 = s2
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords _ self shouldDiffWords.
			differDesc _ diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: s1 to: s2
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: (Preference at: #standardCodeFont)))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !


!Debugger class methodsFor: 'class initialization' stamp: 'jmv 6/30/2022 16:12:30'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	<primitive: 19> "Simulation guard"

	(self errorRecursion not and: [Preference at: #logDebuggerStackToFile]) ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug'].
	ErrorRecursion ifTrue: [
		ErrorRecursion _ false.
		contentsStringOrNil
			ifNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString]
			ifNotNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString, String newLineString, contentsStringOrNil ]].
	ErrorRecursion _ true.
	[self informExistingDebugger: aContext label: aString.
	(Debugger context: aContext)
		openNotifierContents: contentsStringOrNil
		label: aString.] ensure: [ ErrorRecursion _ false ].
	Processor activeProcess suspend.
! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 6/30/2022 16:12:30'!
openInterrupt: aString onProcess: interruptedProcess
	"Open a notifier in response to an interrupt. An interrupt occurs when the user types the interrupt key (cmd-. on Macs, ctrl-c or alt-. on other systems) or when the low-space watcher detects that memory is low."
	| debugger |
	<primitive: 19> "Simulation guard"
	debugger _ self new.
	debugger
		process: interruptedProcess
		context: interruptedProcess suspendedContext.
	debugger externalInterrupt: true.

	(Preference at: #logDebuggerStackToFile) ifTrue:
		[(aString includesSubString: 'Space') & 
			(aString includesSubString: 'low') ifTrue: [
				Smalltalk logError: aString inContext: debugger interruptedContext to: 'LowSpaceDebug']].

	^ debugger
		openNotifierContents: nil
		label: aString
! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 6/30/2022 16:12:30'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| w |
	(Preference at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	w := UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger := self new
			process: process
			context: context.
		debugger interruptedProcessUI: w.
		bool
			ifTrue: [ debugger openFullMorphicLabel: title ]
			ifFalse: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]]
			on: UnhandledError
			do: [ :exOuter | | errorDescription |
				errorDescription := 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
				self primitiveError: errorDescription ]].
	process suspend.! !


!FileList methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:31'!
initialDirectoryList

	| initialDirectoryListFromPreferences wrapperCreator |
	wrapperCreator _ [ :directoryEntry |
		FileDirectoryWrapper
			with: directoryEntry
			name: (directoryEntry name ifNil: [ '/' ])
			model: self ].
	(initialDirectoryListFromPreferences _ Preference at: #initialFileListDirectories) 
		caseOf: {
			[ #roots ] -> [ 	| dirList |
				dirList _ DirectoryEntry roots collect: wrapperCreator.
				dirList isEmpty ifTrue: [
					dirList _ Array with: (FileDirectoryWrapper 
						with: directory
						name: directory localName 
						model: self) ].
				^ dirList ].
			[ #image ] -> [ 
				^ { wrapperCreator value: DirectoryEntry smalltalkImageDirectory } ].
			[ #vm  ] -> [ 
				^ { wrapperCreator value: DirectoryEntry vmDirectory } ].
			[ #current ] -> [
				^ { wrapperCreator value: DirectoryEntry currentDirectory } ] }
		otherwise: [ ^ initialDirectoryListFromPreferences collect: wrapperCreator ]! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 6/30/2022 16:12:30'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preference at: #standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize printStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd _ spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	"
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].
	"
	^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' .! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 6/30/2022 16:12:30'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	selected _ Set new.
	patternsThatSelect do: [ :pat |
		directory childrenDo: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList _ selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry name]) ifTrue: [
				newList remove: entry ]]].
		
	newList _ newList asArray sort: self sortBlock.
	font _ Preference at: #standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !


!Color class methodsFor: 'colormaps' stamp: 'jmv 6/30/2022 16:12:31'!
computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix
	"Builds a colormap intended to convert from subpixelAA black values to targetColor values.
	keepSubPix
		ifTrue: [ Answer colors that also include subpixelAA ]
		ifFalse: [ 
			Take fullpixel luminance level. Apply it to targetColor.
			I.e. answer colors with NO subpixelAA ]"

	| mask map c bitsPerColor r g b f v |

	destDepth > 8
		ifTrue: [bitsPerColor _ 5]  "retain maximum color resolution"
		ifFalse: [bitsPerColor _ 4].
	"Usually a bit less is enough, but make it configurable"
	bitsPerColor _ bitsPerColor min: (Preference at: #aaFontsColormapDepth).
	mask _ (1 bitShift: bitsPerColor) - 1.
	map _ Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		r _ (i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask.
		g _ (i bitShift: 0 - bitsPerColor) bitAnd: mask.
		b _ (i bitShift: 0) bitAnd: mask.
		f _ 1.0 - (r + g + b / 3.0 / mask).
		c _ targetColor
			ifNotNil: [
				(keepSubPix and: [destDepth > 8]) ifTrue: [
						Color
							r: 1.0 - (r asFloat/mask) * targetColor red
							g: 1.0 - (g asFloat/mask) * targetColor green
							b: 1.0 - (b asFloat/mask) * targetColor blue
							alpha: f	* targetColor alpha "alpha will be ignored below, in #pixelValueForDepth: if destDepth ~= 32" ]
				ifFalse: [
					destDepth = 32
						ifTrue: [ targetColor * f alpha: f * targetColor alpha ]
						ifFalse: [ targetColor alphaMixed: f*1.5 with: `Color white` ]]]
			ifNil: [ Color r: r g: g b: b range: mask].	"This is currently used only to keep some SubPixelAA on destDepth = 8, using a single pass of rule 25"
		v _ destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f < 0.1
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]].
		map at: i + 1 put: v ].
	^ map! !


!Behavior methodsFor: 'testing method dictionary' stamp: 'jmv 6/30/2022 16:12:30'!
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."

	| who |

	(Preference at: #thoroughSenders)
		ifTrue: [ who _ self thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte ]
		ifFalse: [ 
			who _ Set new.
			self selectorsAndMethodsDo: [:sel :method |
				((method hasLiteral: literal) or: [specialFlag and: [method scanFor: specialByte]]) ifTrue: [
					((literal isVariableBinding) not or: [method sendsToSuper not
					"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
					Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
						or: [(method indexOfLiteral: literal) ~= 0]]) ifTrue: [who add: sel]]]].
		
	^self rejectSelectorsFrom: who thatReferenceTo: literal byte: specialByte ! !


!Message methodsFor: 'stub creation' stamp: 'jmv 6/30/2022 16:12:31'!
addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ((Preference at: #leftArrowAssignmentsInGeneratedCode)
			ifTrue: [ ' _ ' ]
			ifFalse: [ ' := ' ]);
		nextPutAll: self arguments first argumentName ! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 6/30/2022 16:12:31'!
defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), 
		(Preference at: #userChangesFileNameExtension)! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/30/2022 16:12:30'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	p _ UISupervisor newUIProcessIfNeeded.
	p ifNil: [
		p _ Sensor shiftPressed | (Preference at: #cmdDotInterruptTakesStatistics)
			ifTrue: [Utilities reportCPUandRAM]
			ifFalse: [Utilities processTakingMostCPU]].
	(Preference at: #cmdDotEnabled) ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		p == Processor backgroundProcess ifTrue: [
			'Can not interrupt backgroundProcess' print.
			^self ].
		(p name beginsWith: '[system]') ifTrue: [
			 ('Process {', p printString, '} is critical for system stability. Can not interrupt it.') print.
			^self ].
		[
			EventSensor install.
			p isTerminated
				ifTrue: [ ('Process {', p printString, '} isTerminated. Can not interrupt it.') print ]
				ifFalse: [ UISupervisor userInterrupt: p ]] fork
		]! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/30/2022 16:12:30'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	| localFilename file |
	localFilename _ (Preference at: #debugLogTimestamp)
		ifTrue: [ baseFilename, '-', Utilities dateTimeSuffix, '.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file _ DirectoryEntry smalltalkImageDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/30/2022 16:12:31'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		Smalltalk allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount _ bCount + 1).
			cl selectors do: [:selector |
				m _ cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	Smalltalk allBehaviorsDo: [:b | b zapOrganization].
	Smalltalk closeSourceFiles.
	Preference at: #warnIfNoChangesFile put: false.
	Preference at: #warnIfNoSourcesFile put: false! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/30/2022 16:12:30'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preference at: #wantsMenuIcons put: false.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/30/2022 16:12:30'!
askConfirmationOnQuit
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk askConfirmationOnQuit
	"
	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	"dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ]."
	dirtyPackages _ ChangeSet allChangeSets anySatisfy: [ :any | any codePackage notNil and: [ any hasUnsavedChanges ]].
	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, all unsaved changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, their changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	(Preference at: #askConfirmationOnQuit) ifTrue: [
		^self confirm: 'Do you really want to exit Cuis without saving the image?' ].
	^true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/30/2022 16:12:30'!
setPlatformPreferences
	"Set some platform specific preferences on system startup"
	| platform specs |
	(Preference at: #automaticPlatformSettings) ifFalse:[^self].
	platform _ self platformName.
	specs _ 	#(	
					(soundStopWhenDone false)
					(soundQuickStart false)
			).
	platform = 'Win32' ifTrue:[
		specs _ #(	
					(soundStopWhenDone true)
					(soundQuickStart false)
				)].
	platform = 'Mac OS' ifTrue:[
		specs _ #(	
					(soundStopWhenDone false)
					(soundQuickStart true)
				)].
	specs do: [:tuple |
		Preference name: tuple first category: #system value: (tuple last == #true) 	]
! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 6/30/2022 16:12:30'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preference at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory _ oldUserChanges parent.
			oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine ].
	StartupStamp _ nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 6/30/2022 16:12:30'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg entry |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preference at: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry appendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [Preference at: #warnIfNoChangesFile])
		ifTrue: [self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].
	ChangesInitialFileSize _ changes ifNotNil: [ changes position ].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 6/30/2022 16:12:30'!
beep
	"
	Smalltalk beep
	"
	Preference soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 6/30/2022 16:12:31'!
hasToRestoreChanges

	^(Preference at: #checkLostChangesOnStartUp) and: [ 
		self withChangesFileDo: [ :changesFile | self hasToRestoreChangesFrom: changesFile ]].
	! !


!Exception methodsFor: 'priv handling' stamp: 'jmv 6/30/2022 16:12:29'!
evaluateHandlerBlock: aBlock

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer _ [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx _ ex.  ex pass].
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			handlerBlockNotCurtailed _ true.
			answer
	] ifCurtailed:
		[
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [
					"Please see
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000800.html
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000809.html
					Also see the rest of the tread in detail.
					This is work in progress."
					(Preference at: #allowNonLocalReturnsInExceptionHandlers)
						ifFalse: [ self error: 'Exception handler blocks must not do non local returns' ]
						ifTrue: [
							(Preference at: #warnAboutNonLocalReturnsInExceptionHandlers)
								ifTrue: [ 'It is advisable to avoid method returns (non local returns) in exception handler blocks' print ].
							handlerBlockNotCurtailed _ true ].
					]
				ifFalse: [handlerBlockNotCurtailed _ true]
		]! !


!NegativePowerError methodsFor: 'exceptionDescription' stamp: 'jmv 6/30/2022 16:12:30'!
defaultAction

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	(Preference at: #askToInstallComplexPackage) ifTrue: [
		answer _ PopUpMenu
			withCaption:
'Square (or even) Root of a negative Number:
Complex number support is not loaded
Would you like me to load it for you now?'
			chooseFrom: #(
				'Load Complex package'
				'Do not load Complex package'
				'Do not load Complex package and don''t ask again').
		answer = 1 ifTrue: [
			Feature require: #'Complex'.
			Smalltalk at: #Complex ifPresent: [ :cplx |
				^ (cplx basicReal: receiver imaginary: 0) perform: selector withArguments: arguments ]].
		answer = 3 ifTrue: [
			(Preference at: #askToInstallComplexPackage put: false)]].
	^ super defaultAction! !


!Parser methodsFor: 'scanning' stamp: 'jmv 6/30/2022 16:13:25'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a series of consecutive vertical bars, up arrows, colons and regular binary selector characters.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain.
	Another special case is binary selectors ending in $-, like in
	1@-2
	This could be interpreted as `1 @ -2` or `1 @- 2`. Preference #atMinusDigitMeaning tells what to do."

	| toMakeBinary |
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	"Note: Also include #binary, to allow stuff like #+| where a special character comes after after a regular binary operator character. "
	toMakeBinary _ #(binary verticalBar upArrow colon).
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here _ here asPlainString.
	hereType _ #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			(token == #- and: [source peekBack isDigit])
				ifTrue: [
					(Preference at: #atMinusDigitMeaning) == #disabled
						"Disallow `1@-2`. Insists on some whitespace to disambiguate."
						ifTrue: [ ^self expected: 'A space character after selector' ].
					(Preference at: #atMinusDigitMeaning) == #st80
						ifTrue: [ ^self ].				"interpret `1@-2` like `1 @ -2`"
					"Assume (Preference at: #atMinusDigitMeaning) == #ansiSmalltalk
					interpret `1@-2` like `1 @- 2`, i.e. consider #@- a valid binary selector"
					].
			here _ here , token asPlainString.
			hereEnd _ hereEnd + 1.
			self scanToken ].! !


!BlockNode methodsFor: 'printing' stamp: 'jmv 6/30/2022 16:12:31'!
printOn: aStream indent: level
	| separateLines |
	aStream nextPut: $[.
	self
		printArgumentsOn: aStream
		indent: level.
	separateLines _ (self
		printTemporaries: temporaries
		on: aStream
		doPrior: [ aStream space ]) or: [arguments notNil and: [arguments notEmpty] ].
	(Preference at: #prettyPrintRectangularBlocks)
		ifTrue: [
			"If args+temps > 0 and statements > 1 (or just one complex statement),
			put all statements on separate lines"
			separateLines
				ifTrue: [
					(statements size > 1 or: [
						statements size = 1 and: [ statements first isComplex ]])
							ifTrue: [ aStream newLineTab: (1 max: level) ]
							ifFalse: [ aStream space ] ]
				ifFalse: [
					(statements size = 1 and: [ statements first isComplex not ])
						ifTrue: [ aStream space ]]]
		ifFalse: [
			self isComplex
				ifTrue: [ aStream newLineTab: (1 max: level) ]
				ifFalse: [ aStream space ] ].
	((self printStatementsOn: aStream indent: level) > 0 and: [ aStream peekLast ~= $] ])
		ifTrue: [ aStream space ].
	aStream nextPut: $]! !

!BlockNode methodsFor: 'testing' stamp: 'jmv 6/30/2022 16:12:30'!
printsInNewLine
	"Used for pretty printing to determine whether to start a new line"

	(Preference at: #prettyPrintRectangularBlocks) ifFalse: [ ^false ].
	^super printsInNewLine! !


!TempVariableNode methodsFor: 'testing' stamp: 'jmv 6/30/2022 16:12:30'!
assignmentCheck: encoder at: location
	^((self isBlockArg and: [(Preference at: #allowBlockArgumentAssignment) not])
	    or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]! !


!Editor methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
setIndices: shiftPressed forward: forward
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices _ Dictionary new.
	(shiftPressed and:[Preference at: #selectionsMayShrink])
		ifTrue: [
			indices at: #moving put: self pointIndex.
			indices at: #fixed put: self markIndex
		] ifFalse: [
			forward
				ifTrue:[
					indices at: #moving put: self stopIndex.
					indices at: #fixed put: self startIndex.
				] ifFalse: [
					indices at: #moving put: self startIndex.
					indices at: #fixed put: self stopIndex.
				]
		].
	^indices! !


!TextEditor methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: self class findText
		startingAt: self stopIndex
		caseSensitive: ((self class changeText ~~ self class findText) or: [Preference at: #caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 6/30/2022 16:12:31'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preference at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol _ self selectedSymbol ifNil: [
		self
			evaluateSelectionAndDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
			profiled: false].

	aSymbol first isUppercase
		ifTrue: [
			anEntry _ (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry _ anEntry class].
			BrowserWindow fullOnClass: anEntry selector: nil]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]! !


!Preference class methodsFor: 'event handlers' stamp: 'jmv 6/30/2022 16:12:30'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize _ self fontSizes at: guiElementSizePref value.
	font _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preference name: #standardListFont category: #font value: font.
	Preference name: #standardMenuFont category: #font value: font.
	Preference name: #standardCodeFont category: #font value: font.
	Preference name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preference name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preference at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font
" Copied to PreferenceSet "! !

!Preference class methodsFor: 'event handlers' stamp: 'jmv 6/30/2022 16:12:31'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray |
	prefArray _ self perform: (#machine, machinePref value capitalized) asSymbol.
	prefArray do: [:array | 		Preference at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil]
" Copied to PreferenceSet "! !

!Preference class methodsFor: 'event handlers' stamp: 'jmv 6/30/2022 16:12:30'!
pointer: pointerPref
	| enabled |
	enabled _ pointerPref value == #touch.
	#(tapAndHoldEmulatesButton2 clickGrabsMorphs) do: [:aPref | 		Preference at: aPref put: enabled]
" Copied to PreferenceSet "! !

!Preference class methodsFor: 'fonts' stamp: 'jmv 6/30/2022 16:12:30'!
setDefaultFont: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	Preference
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#standardListFont. (Preference at: #standardListFont) pointSize.}.
				{#standardMenuFont. (Preference at: #standardMenuFont) pointSize.}.
				{#windowTitleFont. (Preference at: #windowTitleFont) pointSize.}.
				{#standardCodeFont. (Preference at: #standardCodeFont) pointSize.}.
				{#standardButtonFont. (Preference at: #standardButtonFont) pointSize.}.
			}.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
" Copied to PreferenceSet "! !

!Preference class methodsFor: 'fonts' stamp: 'jmv 6/30/2022 16:12:30'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [			font _ font emphasized: triplet third ].
		Preference at: triplet first put: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
" Copied to PreferenceSet "! !


!PreferenceSet methodsFor: 'instance creation' stamp: 'jmv 6/30/2022 16:12:30'!
name: nameSymbol description: aString category: categorySymbol type: aType value: aValue
"If I exist, I am updated with the newer sent data "
	(nameSymbol isSymbol or: [categorySymbol isSymbol]) ifFalse: 
		[self error: 'Preference Name & Category are not valid symbol.'].
	^ contents 
		at: nameSymbol
		ifPresent: [:thePref |
			thePref 
				description: aString;
				category: categorySymbol;
				value: aValue;
				yourself]
		ifAbsent: [ | newPref |
			newPref _ Preference new ::
				name: nameSymbol 
				description: aString 
				category: categorySymbol 
				type: aType 
				value: aValue.
			contents at: nameSymbol put: newPref ].
	! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 6/30/2022 16:12:30'!
installDefault: anArray in: category
" Description of preference in Array: name - value - type (optional) - event handler (optional) "
	| myPref |
	myPref _ self sysPreferences 
		name: anArray first 
		description: '' 
		category: category 
		type: (anArray at: 3 ifAbsent: [Preference detectType: anArray second]) value: anArray second.
		
	anArray size = 4 ifTrue: [ "fourth record is an event listener hooked to Preference"
		myPref when: #preferenceChanged send: anArray fourth to: PreferenceSet.
		myPref triggerEvent: #preferenceChanged with: myPref]
	! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 6/30/2022 16:12:30'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences 

		name: #haloHandleSize 
		category: #gui 
		value: [(Preference at: #standardListFont) pointSize * 5 // 3 max: 16];
	
		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preference at: #standardListFont) pointSize * 8 // 14];
		
		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preference at: #standardListFont) pointSize] ;
		
		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preference at: #windowTitleFont) pointSize + 2];
		
		name: #classFinder 
		category: #programming 
		value: [ BrowserWindow findClass ];
		
		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName].! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 6/30/2022 16:12:30'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize _ self fontSizes at: guiElementSizePref value.
	font _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preference name: #standardListFont category: #font value: font.
	Preference name: #standardMenuFont category: #font value: font.
	Preference name: #standardCodeFont category: #font value: font.
	Preference name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preference name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preference at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 6/30/2022 16:12:30'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| grid allowedArea maxLevel |
	"NOTE: following copied from strictlyStaggeredInitialFrameFor:"
	allowedArea _ self maximumUsableArea insetBy: (
		self scrollBarSetback @ self screenTopSetback extent: `0@0`
	).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
	^ ((allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) 
		min: `52@40` * (Preference at: #standardCodeFont) lineSpacing) rounded! !


!Cursor class methodsFor: 'accessing' stamp: 'jmv 6/30/2022 16:12:31'!
defaultCursor
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ (Preference at: #biggerCursors)
		ifTrue: [ CursorWithAlpha biggerNormal ]
		ifFalse: [ self cursorAt: #normalCursorWithMask ].! !

!Cursor class methodsFor: 'current cursor' stamp: 'jmv 6/30/2022 16:12:30'!
currentCursor: aCursor 
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor _ aCursor.
	(Preference at: #biggerCursors)
		ifTrue: [
			[ ^aCursor asBigCursor installCursor] 
				on: Error do: nil  "fall through "].
	aCursor installCursor! !


!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

	| targetColor destDepth |
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: `Color transparent`).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			(Preference at: #subPixelRenderFonts) and: [ foregroundColor = `Color black` or: [ 
				(Preference at: #subPixelRenderColorFonts) and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= `Color black` or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preference at: #properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = `Color black` ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display."
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]! !


!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 6/30/2022 16:12:30'!
syntheticST80Glyphs
	"Build and answer a derivative that includes ST-80 glyphs:
	a left arrow instead of the underscore, and an up arrow instead of the caret."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'ST80';
		perform: (Preference at: #assignmentGlyphSelector).
	^ derivative! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 6/30/2022 16:12:29'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	tabCount _ 0.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			x1 _ destX.
			((Preference at: #backgroundColorFillsAllBackground) and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			((Preference at: #backgroundColorFillsAllBackground) and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 6/30/2022 16:12:30'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| fontFamily |
	self promptUserFolders ifNotNil: [ :selectedNameOrDirectory |
		(Feature require: 'VectorGraphics') ifTrue: [
			(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
				Feature require: 'VectorEngineInSmalltalk' ].
			UISupervisor whenUIinSafeState: [
				fontFamily _ self readIfNeeded: selectedNameOrDirectory.
				fontFamily includesAscii
					ifTrue: [Preference setDefaultFont: fontFamily familyName ]
					ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]]]].! !


!Morph methodsFor: 'events' stamp: 'jmv 6/30/2022 16:12:30'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	(Preference at: #focusFollowsMouse)
		ifTrue: [evt hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:30'!
displayBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ (Preference at: #haloEnclosesFullBounds)
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 6/30/2022 16:12:31'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	(Preference at: #haloSpecifications) do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 6/30/2022 16:12:30'!
haloShowsCoordinateSystem

	^Preference at: #halosShowCoordinateSystem! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 6/30/2022 16:12:31'!
wantsBalloon
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText notNil) and: [Preference at: #balloonHelpEnabled]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 6/30/2022 16:12:30'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].

	(Preference at: #selectiveHalos) ifFalse: [
		^true ].

	(#(#addDismissHandle: ) includes: aSelector)
		ifTrue: [ ^ self resistsRemoval not ].
	(#(#addDragHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToBrownDragEasily ].
	(#(#addResizeHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily | self okayToScaleEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addScaleHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToScaleEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true! !


!HandMorph methodsFor: 'double click support' stamp: 'jmv 6/30/2022 16:12:30'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel

	"Wait until the difference between click, or drag gesture is known, then inform the given morph what transpired." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: (Preference at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 6/30/2022 16:12:29'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (Preference at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 6/30/2022 16:12:31'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel
	
	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (Preference at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:31'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware
			ifTrue: [
				(Preference at: #cacheDisplayContentWhenMovingMorphs)
					ifTrue: [
						"We are caching whatever is in the Display below us. Thefore, there's no need
						to do an invalidation that would trigger the redraw of everything below us."
						self needsRedraw: true ]
					ifFalse:  [
						"No caching of stuff below us. Just invalidate and redraw."
						self redrawNeeded ]]
			ifFalse: [
				lastPosition _ nil.		"Not nil if carrying morphs at that moment"
				prevFullBounds _ nil "Any saved patch is no longer relevant"]].! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 6/30/2022 16:12:30'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp _ Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	(evtBuf sixth <= 0 or: [ (keyValue _ Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth) isNil ]) ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	Sensor peekEvent ifNotNil: [ :nxt |
		"start: Combining diacritical marks (i.e. accents in the Linux VM)"
		(nxt fourth = EventSensor eventKeyDown and: [ nxt third > 255 ]) ifTrue: [
			keyValue _ ((Character numericValue: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
			Sensor
				nextEvent;
				nextEvent;
				nextEvent ].
		"end: Combining diacritical marks (i.e. accents in the Linux VM)"
		"start: Spurious LF after CR on Ctrl-Enter on Windows VM"
		((evtBuf fourth = EventSensor eventKeyChar and: [ evtBuf third = 13 ]) and: [
			nxt fourth = EventSensor eventKeyChar and: [ nxt third = 10 ]]) ifTrue: [ Sensor nextEvent
			"print " ]].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [
		(keyValue = 9 and: [(modifiers anyMask: 1) and: [Smalltalk platformName = 'unix']])
			ifTrue: [
				"Linux VMs don't generate shift-tab keystroke. Turn #keyUp into #keystroke"
				pressType _ EventSensor eventKeyChar ]
			ifFalse: [type _ #keyUp ]].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [ 											"Control key pressed"
			keyValue < 27 ifTrue: [ 										"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [   									"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [ 			"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96   						"shift not pressed: conver to lowercase letter" ]]]].
			"On Windows, ctrl-backSpace is reported as ctrl-forwardDelete. But keyDown is ok, so we can know and fix."
			(keyValue = 127 and: [ lastKeyDownValue = 8 ])
				ifTrue: [ keyValue _ 8 ].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers _ modifiers bitOr: 8 ]].
		(modifiers anyMask: 8) ifTrue: [ 									"CmdAlt key pressed (or Control key pressed, and #shouldControlEmulateAltFor: just answered true)"
			(modifiers anyMask: 1) ifTrue: [								"Shift pressed"
				| i |
				"It seems that for ctrl-shifted keys and cmd-shifted keys, the VM incorrectly reports the UNSHIFTED character.
				Correct this, at least for common cmd-shortcuts, and for the US keyboard... Sigh...
				(This has only been observed on Mac VMs, but seems harmless if proper shifted character is reported (as in Linux), as this wil be NOP)
				(On Windows, the situation is even worse: ctrl-{ is not even reported as a keystroke event. Only keyDown and keyUp.)"
				"#($' $, $. $9 $0 $[ $]) -> #($'' $< $> $( $) ${) $}"
				i _ #[39 44 46 57 48 91 93 ] indexOf: keyValue.
				i > 0 ifTrue: [
					keyValue _ #[34 60 62 40 41 123 125] at: i ]]]].
	buttons _ modifiers bitShift: 3.
	"Linux and Windows VM send keyboard ctrl-upArrow and ctrl-downArrow when the user tries to scroll using the mouse wheel
	Mac VM sends cmd-option-ctrl-shift-upArrow and cmd-option-ctrl-shift-downArrow for trackpad vertical scroll gestures,
		and cmd-option-ctrl-shift-leftArrow and cmd-option-ctrl-shift-rightArrow for horizontal scroll gestures.
	This way of reporting scroll events by the VM also enables scrolling using the keyboard (actually, we can't tell if user gesture was on Mouse, Trackpad or Keyboard).
	But ctrl-shift and cmdAlt-shift are needed used for selecting while moving by word, line, etc.
	Additionally, #ctrlArrowsScrollHorizontally allows chosing between keyboard horizontal scroll and moving word by word in text editors."
	mouseScrollDirection _ nil.
	"Ctrl for Keyboard or Mouse wheel gestures. All modifiers for Trackpad gestures."
	(buttons = InputSensor controlKey or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 30
			ifTrue: [mouseScrollDirection _ #up]
		ifFalse: [keyValue = 31
			ifTrue: [mouseScrollDirection _ #down]]].
	"Ctrl for Keyboard or Mouse wheel gestures, only if preference is set. All modifiers for Trackpad gestures."
	((buttons = InputSensor controlKey and: [Preference at: #ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 28
			ifTrue: [mouseScrollDirection _ #left]
		ifFalse: [keyValue = 29
			ifTrue: [mouseScrollDirection _ #right]]].
	mouseScrollDirection ifNotNil: [
		^ MouseScrollEvent new
			setType: #mouseScroll
			position: self morphPosition
			direction: mouseScrollDirection
			buttons: buttons
			hand: self
			stamp: stamp ].
	^ KeyboardEvent new
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:30'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	(Preference at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage _ canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(Preference at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 6/30/2022 16:12:30'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause _ 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait _ 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause _ 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause _ pause * 21//20 ] ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].
			wait _ waitUntil - Time localMillisecondClock max: 0 ].
	(Preference at: #serverMode)
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	canvas ensureCurrentMorphIsWorld.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow.! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 6/30/2022 16:12:29'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preference desktopMenuTitle.
	menu popUpInWorld: self! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 6/30/2022 16:12:31'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		(Preference at: #cheapWindowReframe) or: [millisecondSinceLast > 200]]) ifTrue: [
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner morphExtentInWorld ]].! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:30'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ BoxedMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: (Preference at: #standardMenuFont) bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphWidth! !

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w _ self world.
	w isNil ifTrue: [^ response asString].
	done _ false.
	canceled _ false.
	(Preference at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock _ [:aString| done _ true].
	cancelBlock _ [done _ true. canceled _ true].
	delay _ Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 6/30/2022 16:12:30'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	| answer |
	answer _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	(Preference at: #focusFollowsMouse) ifFalse: [answer textBox focusText].
	^ answer! !

!StringRequestMorph class methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e _ (Preference at: #windowTitleFont) lineSpacing.
	^ (0)@(0.5 * e)! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:30'!
defaultHeight

	^ ((Preference at: #windowTitleFont) lineSpacing * 2 * self scale) asInteger! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 6/30/2022 16:12:30'!
addDebugHandle: handleSpec

	(Preference at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doDebug:with: ]
! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 6/30/2022 16:12:31'!
addExploreHandle: handleSpec

	(Preference at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doExplore:with: ]
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle."

	| handle aPoint colorToUse form icon e |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	e _ (Preference at: #haloHandleSize) asPoint.
	handle morphPosition: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				form extent = e ifFalse: [ 
					": Non default size, scale that bugger!!"
					form _ form  ": Be as smooth as possible, these images are small."
						magnify: form boundingBox
						to: e
						smoothing: 2 ].
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground _ BoxedMorph new noBorder
		color: ((target is: #SystemWindow)
			ifTrue: [target windowColor]
			ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph _ LabelMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	verticalNamePosition _ haloBox bottom + (Preference at: #haloHandleSize).
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
handlesBox
	"handlesBox is in local coordinates.
	We assume we are direct submorph of the world, without any scaling or rotation."

	| minSide hs c e box |
	hs _ Preference at: #haloHandleSize.
	minSide _ 4 * hs.
	e _ extent + (hs*2) max: minSide@minSide.
	c _ extent // 2 + self morphPosition.
	box _ Rectangle center: c extent: e.
	self world ifNotNil: [ :w | box _ box intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	"Make it local"
	^box translatedBy: self morphPosition negated.
! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:29'!
drawOn: aCanvas

	(Preference at: #showLinesInHierarchyViews) ifTrue:[
		self drawLinesOn: aCanvas ]! !


!InnerListMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:29'!
initialize
	super initialize.
	self color: `Color black`.
	font _ Preference at: #standardListFont.
	listItems _ #().
	selectedRow _ nil.
	highlightedRow _ nil! !

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:30'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preference at: #standardListFont).! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e _ model editorClass new morph: self.
	e model: model.
	tc _ TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor _ e.
		textComposition _ tc.
		tc
			setModel: model;
			extentForComposing: self extentForComposing.
		e textComposition: tc.
		tc editor: e ].
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		((Preference at: #syntaxHighlightingAsYouType) ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !


!LabelMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:30'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preference at: #standardListFont).
	self fitContents.! !


!IndentingListItemMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:30'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preference at: #standardListFont).! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:30'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: (Preference at: #standardListFont) emphasis: nil.
	indentLevel _ 0.
	isExpanded _ false.
 	nextSibling _ firstChild _ nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o _ anObject withoutListWrapper.
	icon _ o ifNotNil: [ (o respondsTo: #icon) ifTrue: [ o icon ] ].
	icon isSymbol ifTrue: [ icon _ Theme current perform: icon ].
	indentLevel _ newLevel.
! !


!UpdatingLabelMorph class methodsFor: 'new-morph participation' stamp: 'jmv 6/30/2022 16:12:30'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (Preference at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:30'!
initialize
	"initialize the state of the receiver"
	super initialize.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preference at: #standardMenuFont.
	self contents: ''.! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:31'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [Preference at: #standardButtonFont].
	"
	Could add emphasis...
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]
	"
	^fontToUse! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:31'!
minimumExtent
	
	| unit |
	unit :=  (Preference at: #windowTitleFont) pointSize.
	^label 
		ifNil: [ (unit+2) @ (unit+2) ] "Assume title button"
		ifNotNil: [ (4 * unit) @ (3 * unit) ] "Assure some space for text."
! !


!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 6/30/2022 16:12:29'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ Preference at: #drawKeyboardFocusIndicator.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.
	self updateScrollBarsBounds.
	self innerMorphClass ifNotNil: [ :contentsClass |
		self scroller: contentsClass new ].! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:29'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: (Preference at: #focusIndicatorWidth)
			color: Theme current focusIndicator ]! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 6/30/2022 16:12:30'!
mouseEnter: event
	super mouseEnter: event.
	(Preference at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !


!PluggableListMorph methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:30'!
drawOn: aCanvas
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: (Preference at: #focusIndicatorWidth)
			color: Theme current focusIndicator ].! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 6/30/2022 16:12:30'!
mouseEnter: event
	super mouseEnter: event.
	(Preference at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2022 16:12:31'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preference at: #standardListFont).! !


!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:30'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ Preference at: #focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan ]
			ifFalse: [
				scroller hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						scroller hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ scroller hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc _ Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc _ bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 6/30/2022 16:12:30'!
mouseEnter: event
	super mouseEnter: event.
	(Preference at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: scroller ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 6/30/2022 16:12:30'!
drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f _ Preference at: #windowTitleFont.
	x0 _  f lineSpacing * 5 + borderWidth.
	y0 _ borderWidth * 6 // 10.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

!SystemWindow methodsFor: 'events' stamp: 'jmv 6/30/2022 16:12:29'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph"
	^aMorph isWorldMorph or:[Preference at: #systemWindowEmbedOK]! !

!SystemWindow methodsFor: 'label' stamp: 'jmv 6/30/2022 16:12:30'!
labelHeight
	"Answer the height for the window label."
	^ (Preference at: #windowTitleFont) lineSpacing+1! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 6/30/2022 16:12:31'!
addTileResizerMenuTo: aMenu
	"We can look at preferences here to decide what too do"
	(Preference at: #tileResizerInWindowMenu) ifFalse: [
		aMenu add: 'resize full' 		action: #resizeFull icon: #resizeFullIcon;
		add: 'resize top' 				action: #resizeTop icon: #resizeTopIcon;
		add: 'resize left' 				action: #resizeLeft icon: #resizeLeftIcon;
		add: 'resize bottom' 			action: #resizeBottom icon: #resizeBottomIcon;
		add: 'resize right' 				action: #resizeRight icon: #resizeRightIcon;
		add: 'resize top left' 			action: #resizeTopLeft icon: #resizeTopLeftIcon;
		add: 'resize top right' 		action: #resizeTopRight icon: #resizeTopRightIcon;
		add: 'resize bottom left' 		action: #resizeBottomLeft icon: #resizeBottomLeftIcon;
		add: 'resize bottom right' 	action: #resizeBottomRight icon: #resizeBottomRightIcon]
	ifTrue: [ |resizeMorph|
		"Use embedded resize morph"
		resizeMorph _ TileResizeMorph new
							selectionColor: (self widgetsColor adjustSaturation: -0.2 brightness: 0.25) ;
							action: [:resize | |resizeMsg|
								resizeMsg _ ('resize', resize asPlainString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].
	^aMenu.! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 6/30/2022 16:12:29'!
closeBoxHit
	"The user clicked on the close-box control in the window title.  For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down."

	(Preference at: #dismissAllOnOptionClose) ifTrue:
		[Sensor rawMacOptionKeyPressed ifTrue:
			[^ self world closeUnchangedWindows]].
	self delete
! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
initialExtent

	^`540@400` * (Preference at: #standardCodeFont) lineSpacing // 14! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(Preference at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preference at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	(Preference at: #shiftClickShowsImplementors)
		ifTrue: [ self addShiftClickEventHandlerFor: codePane ].
		
	^codePane! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor _ [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList _ #(
		(10	'browse'			browseMethodFull							'view this method in a browser')
		(11	'senders' 			browseSendersOfMessages							'browse senders of...' 						browseSendersOfMethod)
		(16	'implementors'			browseMessages							'browse implementors of...' 						browseImplementors)
		(12	'versions'			browseVersions							'browse versions')), 

		((Preference at: #decorateBrowserButtons)
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'}}]
			ifFalse: [
				{#(13	'inheritance'		 methodInheritance	 'browse method inheritance')}]),

		#(
		(12	'hierarchy'			browseHierarchy							'browse class hierarchy')
		(10	'inst vars'			browseInstVarRefs							'inst var refs...')
		(11	'class vars'			browseClassVarRefs							'class var refs...')
		(10	'show...'			offerWhatToShowMenu							'menu of what to show in lower pane')).

	^ aList! !

!CodeWindow methodsFor: 'updating' stamp: 'jmv 6/30/2022 16:12:30'!
decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	(Preference at: #decorateBrowserButtons)
		ifFalse: [ ^aButton color: buttonColor ].
	cm _ model currentCompiledMethod.
	(cm is: #CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags _ 0.
	model isThisAnOverride ifTrue: [ flags _ flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags _ flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags _ flags bitOr: 1 ].
	aColor _ {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		`Color tan`.							"no sends to super. there is an override in some subclass"
		`Color red`.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		`Color red`.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		`Color red muchLighter`.			"doesn't have sub; has super but doesn't call it"
		`Color r: 0.94 g: 0.823 b: 0.673`.		"has sub; has super but doesn't call it"
		`Color green muchLighter`.			"doesn't have sub; has super and callsl it"
		`Color blue muchLighter`.			"has sub; has super and callsl it"

	} at: flags + 1.
	Theme current useUniformColors
		ifTrue: [
			aButton color: (self buttonColor mixed: 0.8 with: aColor) ]
		ifFalse: [
			aButton color: aColor ]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code _ self buildMorphicCodePane.
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons _ LayoutMorph newColumn.
	(Preference at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	(Preference at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classList _ self buildMorphicClassList.
	classColumn _ self buildMorphicClassColumnWith: classList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (Preference at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model setSelectedSystemCategory: model systemCategoryList first.
	sysCatList _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (Preference at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:31'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(Preference at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preference at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:31'!
initialExtent
	^`540@300` * (Preference at: #standardCodeFont) lineSpacing // 14! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	(Preference at: #optionalButtons) ifTrue: [
		h _ self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preference at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
initialExtent
	^ `640 @ 320` * (Preference at: #standardCodeFont) lineSpacing // 14! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 6/30/2022 16:12:31'!
open: model label: aString message: messageString
	
	((Preference at: #usePreDebugWindow) or: [messageString notNil])
		ifTrue: [self forceOpen: model label: aString message: messageString]
		ifFalse: [model openFullMorphicLabel: aString ]! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 6/30/2022 16:12:30'!
volumeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	model currentDirectorySelected
		ifNil: [ aMenu add: 'initial directory' action: #yourself :: isEnabled: false ]
		ifNotNil: [ :selectedWrapper |
	aMenu 
		add: (Preference isInitialFileListDirectory: selectedWrapper item)
			asMenuItemTextPrefix, 'initial directory'
		action: #toggleInitialDirectory ::
			setBalloonText: 'The selected directory is an initial director for new file list windows' ].
	initialDirectoriesMenu _ MenuMorph new.
	#(
		(roots  'default roots' 'Use the usual root directories. Drives on Windows; "/" on Unix')
	   	(image 'image directory' 'Use the directory with Smalltalk image')
		(vm 'VM directory' 'Use the virtual machine directory')
		(current 'current directory' 'Use the current directory; usually the directory the VM was started in')
	)
		do: [ :entry |
			initialDirectoriesMenu
				add: entry second
				target: Preference
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
initialExtent

	^`600@325` * (Preference at: #standardCodeFont) lineSpacing // 14! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 16:12:30'!
initialExtent

	^`300@500` * (Preference at: #standardCodeFont) lineSpacing // 14! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/30/2022 16:12:30'!
addItemsFromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target.
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item wantsIcons |
	wantsIcons _ Preference at: #wantsMenuIcons.
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ self addLine ]
			ifNotNil: [ | realTarget |
				realTarget _ dict at: #object ifAbsent: [defaultTarget].
				realTarget isSymbol ifTrue: [ realTarget _ defaultTarget perform: realTarget ].
				item _ (dict at: #label) isSymbol
					ifTrue: [
						self
							addUpdating: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						self
							add: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				wantsIcons ifTrue: [
					dict
						at: #icon
						ifPresent: [ :symbolOrFormOrNil |
							item setIcon: symbolOrFormOrNil ]].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ].
			]]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/30/2022 16:12:30'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	(Preference at: #optionalButtons) ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW _ Theme current titleBarButtonsExtent x.
	closeButton _ PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; color: `Color transparent`.
	pinButton _ PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; color: `Color transparent`.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW);
			color: `Color transparent`;
			addMorph: closeButton fixedWidth: buttonHW;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/30/2022 16:12:30'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph _ BoxedMorph new noBorder.
	newMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line | | font |
		font _ Preference at: #standardMenuFont.
		s _ LabelMorph new
			contents: line;
			font: (titleMorph
				ifNil: [ font bold ]
				ifNotNil: [ font italic ])..
		newMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph _ newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/30/2022 16:12:29'!
popUpAt: aPoint forHand: hand in: aWorld
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self 
		popUpAt: aPoint 
		forHand: hand 
		in: aWorld 
		allowKeyboard: (Preference at: #menuKeyboardControl)! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/30/2022 16:12:30'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or:
		[Preference at: #systemWindowEmbedOK]! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 6/30/2022 16:12:30'!
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: (Preference at: #menuKeyboardControl)

	"
	| menu sub entry |
	menu _ MenuMorph new.
	1 to: 3 do: [:i |
		entry _ 'Line', i printString.
		sub _ MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				action: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
	"! !


!AutoCompleterMorph class methodsFor: 'preferences' stamp: 'jmv 6/30/2022 16:12:30'!
listFont

	^Preference at: #standardListFont! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 6/30/2022 16:12:29'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut
			ifTrue: [ ^ (Preference at: #classFinder) value ].
		self isCloseWindowShortcut
			ifTrue: [ ^ self closeCurrentWindowOf: aMorph ].
		^ aMorph processKeystroke: self ].
	type == #keyDown ifTrue: [
		^ aMorph processKeyDown: self ].
	type == #keyUp ifTrue: [ 
		^ aMorph processKeyUp: self ].
	^ super sendEventTo: aMorph.! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 6/30/2022 16:12:29'!
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
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [ aMorph fullIncludesPixel: position ] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: ((Preference at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
						clkSel: ((Preference at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					(Preference at: #clickGrabsMorphs) ifFalse: [
						self shiftPressed
							ifTrue: [ aMorph is: #SystemWindow :: ifTrue: [ aMorph sendToBack ] ]
							ifFalse: [ aMorph activateWindow ] ].
					self wasHandled: true.
					^ self ]]].

	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/30/2022 16:12:30'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preference.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preference.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preference.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 6/30/2022 16:12:30'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"
	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			(Preference at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !


!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:30'!
buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^(Preference at: #standardButtonFont) lineSpacing * 14 // 8! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:30'!
fullScreenDeskMargin
	^ (Preference at: #fullScreenLeavesDeskMargins) ifTrue: [48] ifFalse: [0]! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:31'!
layoutAdjusterThickness

	self flag: #todo.  "Revisit this; consider moving proportional stuff out of Theme entirely."
	
	^ (Preference at: #standardListFont) pointSize // 3! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:30'!
roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^ Preference at: #roundedButtonRadius! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:31'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^Preference at: #roundedWindowRadius! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:30'!
scrollbarThickness
	^ Preference at: #scrollbarThickness! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:30'!
titleBarButtonsExtent
	"Answer the extent to use for close & other title bar buttons.  
	The label height is used to be proportional to the fonts preferences."
	| e |
	e _ (Preference at: #windowTitleFont) pointSize * 13 // 10.
	^e@e! !

!Theme methodsFor: 'other options' stamp: 'jmv 6/30/2022 16:12:29'!
windowBorderWidth
	| w |
	w _ (Preference at: #standardListFont) pointSize / 11.
	w _ w * (self roundWindowCorners ifTrue: [4] ifFalse: [2]).
	^ w rounded max: 1! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/30/2022 16:12:29'!
italic
	^(Preference at: #italicsInShout)
		ifTrue: [ #italic ]
		ifFalse: [ #normal ]! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 6/30/2022 16:12:30'!
changeFontSizes

	| options menu preference  |
	preference _ Preference instanceAt: #guiElementsSize.
	options _ #(
		#('Huge' #hugeFonts)
		#('Very Large' #veryLargeFonts)
		#('Large' #largeFonts)
		#('Default Size' #standardFonts)
		#('Small' #smallFonts)
		#('Very Small' #verySmallFonts)
		#('Tiny' #tinyFonts)).
	menu _ MenuMorph new.
	menu
		addTitle: 'Make GUI elements';
		addStayUpIcons;
		stayUp: true.
	options do: [ :pair |
		(menu add: pair first target: preference action: #value: argument: pair second)
			isSelected: preference value == pair second  ].
	menu popUpInWorld: self runningWorld.! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 6/30/2022 16:12:31'!
useMenuIcons
	Preference at: #wantsMenuIcons put: true! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 6/30/2022 16:12:30'!
useNoMenuIcons
	Preference at: #wantsMenuIcons put: false! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 6/30/2022 16:12:30'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse _ (Preference at: #changeSetVersionNumbers)
				ifTrue: [
					DirectoryEntry currentDirectory
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].
		
	nameToUse asFileEntry writeStreamDo: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	(Preference at: #checkForSlips) 		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 6/30/2022 16:12:30'!
preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: (Preference at: #defaultAuthorName).
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 6/30/2022 16:12:31'!
install: aFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: aFileEntry name do: [ self fileIn: aFileEntry ].
	(Preference at: #transcriptLogVerbose) ifTrue: [
		('Installed ChangeSet: ', aFileEntry name) print]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/30/2022 16:12:31'!
fileIn
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		(Preference at: #transcriptLogVerbose) ifTrue: [
			Transcript newLine; show:'Filing in ', cls name].
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 6/30/2022 16:12:31'!
install
	"Create, install and answer a (sub)instance of CodePackage
	Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS _ ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS _ nil ].
		pckClass _ CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				((self packageName asIdentifier: true), 'Package') = ee name ifTrue: [
					ee fileInDefinitionAndMetaclass.
					pckClass _ Smalltalk at: ee name ]]].
		newCodePackage _ pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.

		fullName asFileEntry readStreamDo: [ :stream | stream fileInAnnouncing: 'Installing ', localName, '...' ].
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].

	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	(Preference at: #transcriptLogVerbose) ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage! !


!TestResult methodsFor: 'logging' stamp: 'jmv 6/30/2022 16:12:31'!
reportAboutToRun: aTestCase
	(Preference at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'Will run: '; print: aTestCase; newLine]! !

!TestResult methodsFor: 'logging' stamp: 'jmv 6/30/2022 16:12:30'!
reportFailed: aTestCase because: anException
	(Preference at: #transcriptLogVerbose) ifTrue: [
		Transcript print: anException; newLine].! !

!TestResult methodsFor: 'logging' stamp: 'jmv 6/30/2022 16:12:30'!
reportPassed: aTestCase
	(Preference at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'finished.'; newLine]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
privateFormatAndConvert
	"Perform any formatting of formattedText necessary and store or a formatted copy in formattedText"
	(Preference at: #syntaxHighlightingAsYouTypeAnsiAssignment) ifTrue: [
		self convertAssignmentsToAnsi ].
	(Preference at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self convertAssignmentsToLeftArrow ]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/30/2022 16:12:30'!
privateStyle

	| alpha end start count startIndexes c hue |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(Preference at: #highlightBlockNesting) ifTrue: [
		startIndexes _ parser blockDepthsStartIndexes.
		count _ startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start _ startIndexes at: idx.
			end _ idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha _ depth / 10.0 min: 1.0.
			hue _ depth * 60.
			c _ Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]]! !


!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'jmv 6/30/2022 16:12:29'!
ansiAssignmentPreferenceChanged
	"the user has changed the syntaxHighlightingAsYouTypeAnsiAssignment setting.
	If they have turned it on then force syntaxHighlightingAsYouTypeLeftArrowAssignment
	to be turned off"
	(Preference at: #syntaxHighlightingAsYouTypeAnsiAssignment) 		ifTrue: [
		Preference at: #syntaxHighlightingAsYouTypeLeftArrowAssignment put: false]! !

!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'jmv 6/30/2022 16:12:30'!
leftArrowAssignmentPreferenceChanged
	"the user has changed the syntaxHighlightingAsYouTypeLeftArrowAssignment setting.
	If they have turned it on then force syntaxHighlightingAsYouTypeAnsiAssignment
	to be turned off"
	(Preference at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) 		ifTrue:[
		Preference at: #syntaxHighlightingAsYouTypeAnsiAssignment put: false]! !

!SHTextStylerST80 class methodsFor: 'class initialization' stamp: 'jmv 6/30/2022 16:12:30'!
initialize  
	"Clear styleTable and textAttributesByPixelSize cache so that they will 
	reinitialize.	 

		SHTextStylerST80 initialize
	" 
	
	styleTable := nil.
	textAttributes := nil.	
	"We want to be  informed at preference changes"
	(Preference instanceAt: #syntaxHighlightingAsYouTypeAnsiAssignment)
		when: #preferenceChanged
		send: #ansiAssignmentPreferenceChanged
		to: self.
	(Preference instanceAt: #syntaxHighlightingAsYouTypeLeftArrowAssignment)
		when: #preferenceChanged
		send: #leftArrowAssignmentPreferenceChanged
		to: self! !


!PopUpMenu methodsFor: 'accessing' stamp: 'jmv 6/30/2022 16:12:30'!
frameHeight
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems _ 1 + labelString lineCount.
	^ (nItems * (Preference at: #standardMenuFont) lineSpacing) + 4 "border width"! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 6/30/2022 16:12:31'!
startUpNonModalWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpNonModalWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (Preference at: #menuKeyboardControl)! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 6/30/2022 16:12:30'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skipBack])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // (Preference at: #standardMenuFont) lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 6/30/2022 16:12:30'!
startUpWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (Preference at: #menuKeyboardControl)! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'jmv 6/30/2022 16:12:30'!
preferredAssignmentOperator

	^ (Preference at: #leftArrowAssignmentsInGeneratedCode)
		ifTrue: [ '_' ]
		ifFalse: [ ':=' ]! !


!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch releaseCachedState)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shouldStyle:with:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('binding' bindingOf:)
!


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording nameForObject: toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shouldStyle:with: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' allowStylingWithEmphasis autoCompleterClass editorClass formatAndStyleIfNeededWith: fullPrintIt textStylerClass toggleFullPrintIt toggleFullPrintItLabel)
('auto complete' classOfBindingOf: classOfWorkspaceVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('testing' canBindVariables is: styleByParagraphs)
('accessing' methodNodeOf:ifErrorsParsing:)
!


!TextProvider reorganize!
('contents' acceptedContents acceptedContentsChanged acceptedStringOrText)
('accessing' contentsSelection methodNodeOf:ifErrorsParsing:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
!


!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForSelector:ofClass: annotationForSystemCategory: annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol: currentMethodRefactored methodNodeOf:ifErrorsParsing:)
('commands' hierarchyBrowser removeClass)
('controls' contentsSymbolQuints sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion timeStamp unusedMethods)
('message category functions' canShowMultipleMessageCategories)
('testing' is: isEditingMethod)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' isModeStyleable shouldStyle:with:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
!


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: originalSelectedClassName recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategory:class: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: newSystemCategoryNameInitialAnswer potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectedSystemCategory setSelectedSystemCategory: setSelectedSystemCategoryTreeItem: systemCategoryList systemCategoryRoots systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' insertSuperclass removeClass renameClass renameGlobal safelyRemoveClass)
('testing' is: isEditingClass isEditingExistingClass isEditingMethod isEditingNewClass)
('self-updating' updateIfNeeded)
!


!Browser class reorganize!
('instance creation' new)
('cached state access' recentClasses releaseClassCachedState)
('class list' disableListClassesHierarchically enableListClassesHierarchically listClassesHierarchically)
!


!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText baseCodeSource baseCodeSource: caseCodeSource caseCodeSource: contents:notifying: contentsSymbol)
('class list' classList renameClass selectedClass)
('diffs' methodDiffFor:selector: modifiedClassDefinition)
('edit pane' selectedBytecodes selectedMessage)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('menus' contentsSymbolQuints)
('message list' messageList)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('shout styling' shouldStyle:with:)
('private' pvtBaseClassOrMetaclass pvtBaseSelectedMessageSourceCodeFor: pvtCaseSelectedMessageSourceCodeFor:)
!


!ChangeList reorganize!
('initialization-release' addItem:text: initialize wantsPrettyDiffOption)
('scanning' classDefinitionRecordFrom: itemIsRecognized: notSeparatorChar scanAndIgnore:in: scanCategory:class:meta:stamp:prior:overrides: scanClassComment: scanClassDefinition: scanClassRemoval: scanClassRenamed: scanDoIt scanFeatureProvision: scanFeatureRequirement: scanFile:from:to: scanMethodDefinition: scanMethodRemoval: scanSpecificChangeRecordType scanSpecificChangeRecordTypeIfNotAtEnd: scanUpTo:informing:)
('menu actions' compareToCurrentVersion currentVersionsOfSelections deselectAll destroyCurrentCodeOfSelections fileInSelections fileOutCurrentVersionsOfSelections fileOutSelections invertSelections removeDoIts removeEmptyClassComments removeNonSelections removeOlderMethodVersions removeSelections removeUpToDate selectAll selectAllForAbsentClasses selectAllForThisClass selectEquivalentMethods selectMethodsOlderThanCurrent selectNewMethods selectRemovalsOfSent selectSuchThat: selectUnchangedMethods xtraVersion)
('viewing access' acceptedStringOrText annotation contentsDiffedFromCurrent contentsSymbolQuints diffedVersionContents list listIndex listSelectionAt: listSelectionAt:put: selectedClass selectedClassOrMetaClass selectedMessageName toggleListIndex: undiffedContents)
('accessing' changeList contentsSymbol currentChange file showsVersions)
('shout styling' shouldStyle:with:)
('private' clearSelections)
('testing' isNewMethod:)
!


!Debugger class reorganize!
('class initialization' openContext:label:contents: releaseClassCachedState)
('instance creation' context: informExistingDebugger:label:)
('opening' interruptProcess:label: openInterrupt:onProcess: openOn:context:label:fullView:)
('cached state access' errorRecursion)
('constants' defaultDebugStackSize)
!


!FileList reorganize!
('file list' fileList fileListIndex fileListIndex: selectedFileEntry)
('user interface support' autoCompleterClassFor: editorClassFor:)
('file list menu' itemsForAnyFile1 itemsForAnyFile2 itemsForFileEntry: itemsForNoFile)
('file menu action' addNew:byEvaluating: addNewDirectory addNewFile deleteFile get getHex renameFile sortByDate sortByName sortBySize)
('initialization' buttonSelectorsToSuppress directory: dynamicButtonServices initialDirectoryList initialize labelString universalButtonServices updateDirectory)
('menu messages' copyName)
('own services' serviceAddNewDirectory serviceAddNewFile serviceCopyName serviceDeleteFile serviceGet serviceGetHex serviceRenameFile serviceSortByDate serviceSortByName serviceSortBySize serviceViewContentsInWorkspace viewContentsInWorkspace)
('volume list and pattern' deleteDirectory directory doesPattern:allow: fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad: listForSelectingPatterns:rejectingPatterns: pattern pattern:)
('private' acceptedStringOrText currentDirectorySelected defaultContents fileNameFromFormattedItem: folderString fullName isFileSmalltalkCode postOpen put: readContentsBrief: readContentsHex: resort: setSelectedDirectoryTo: sortBlock sortingByDate sortingByName sortingBySize updateFileList)
('accessing' brevityState)
('file name list' fileName)
('user interface' toggleInitialDirectory)
!


!Color class reorganize!
('instance creation' clipR:g:b: colorFrom: colorFromPixelValue:depth: fromArray: fromHexString: fromRgbOrRgba: fromRgbTriplet: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: jet: numSlots r:g:b: r:g:b:alpha: r:g:b:range: random random2 random2: random:)
('class initialization' initializeGrayToIndexMap initializeIndexedColors newFromStream: oneBitColors)
('examples' colorRampForDepth:extent: experimentsTowardsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' black blue brown cyan darkGray gray green lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightRed lightYellow magenta orange pink purple red tan transparent veryDarkGray veryLightGray veryVeryDarkGray veryVeryLightGray white yellow)
('colormaps' cachedColormapForGrayFrom: cachedColormapFrom:to: cachedColormapFromGrayTo: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapForGray8bppFrom: computeColormapFrom:to: computeColormapFromGray8bppForDepth: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor: computeRGBColormapForGray8)
('other' maskingMap:)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper:)
('color space conversions' applySimpleGamma:to: linearTosRGBGamma: sRGBGammaToLinear:)
('cached state access' grayToIndexMap indexedColors releaseClassState)
!


!Behavior reorganize!
('accessing' classDepth decompilerClass format name parserClass sourceCodeTemplate theNonMetaClass typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary hasChangedComparedTo: instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesPreviousTo: allSuperclassesUpTo: subclasses subclassesDo: subclassesDoGently: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesPreviousTo: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!


!Message reorganize!
('accessing' argument arguments fullName lookupClass numArgs selector sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('as yet unclassified' pushReceiver)
('testing' hasArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
!


!SystemDictionary reorganize!
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToCharacterConstants browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs:clearAllClassState: saveAsNewReleaseAndQuit saveAsNewReleaseAndStay saveImage send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMessageListUnsorted:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive specialSelectors)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit isSpur maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' doStartUp: processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!


!Exception reorganize!
('Camp Smalltalk' sunitExitWith:)
('defaults' defaultResumeValue)
('exceptionBuilder' messageText: tag:)
('exceptionDescription' defaultAction description messageText noHandler tag)
('handling' isNested outer pass resignalAs: resume resume: resumeUnchecked: retry retryUsing: return return:)
('debug support' canSearchForSignalerContext)
('printing' printOn: receiver signalerContext)
('priv handling' evaluateHandlerBlock: handlerBlockNotCurtailed handlerContexts handlerContexts: initialize isResumable popHandlerContext pushHandlerContext: topHandlerContext)
('signaling' signal signal: signalIn:)
!


!NegativePowerError reorganize!
('signaling' floatErrorValue)
('exceptionDescription' defaultAction)
!


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken matchReturn startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression isLiteralExpression isLiteralNumberExpression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:doIt:context: newMethodNode parenthesisExpression pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('interactive checks')
('backtick' addToSentInLiterals: backtickExpression)
('comparing' match: matchToken:)
('temporaries' bindTemp:)
('as yet unclassified' performInteractiveChecks:)
!


!BlockNode reorganize!
('accessing' addArgument: arguments arguments: block closureCreationNode firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: temporariesDeclaration temporariesDeclaration: tempsMark tempsMark:)
('code generation' code)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForClosureValue:encoder: emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForClosureValue: sizeCodeForEvaluatedClosureValue:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: emitCodeForValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue: sizeCodeForValue:)
('enumerating' statementsDo:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: statements:returns:)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: hasArgumentOrTemporaryNamed: isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine referencesSelf referencesSuper returns)
('visiting' accept:)
('private' hasEquivalentStatementsWith:)
!


!TempVariableNode reorganize!
('visiting' accept:)
('code generation (closures)' addReadWithin:at: addWriteWithin:at: analyseClosure: analyseTempsWithin:rootNode:assignmentPools: beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools: definingScope definingScope: index: isDefinedWithinBlockExtent: isIndirectTempVector isReferencedWithinBlockExtent: referenceScopesAndIndicesDo:)
('testing' assignmentCheck:at: beBlockArg beMethodArg isArg isBlockArg isDeclaredAtMethodLevel isMethodArg isRemote isTemp isTempOrArg isUndefTemp isUnusedTemp remoteNode scope)
('printing' printDefinitionForClosureAnalysisOn: printWithClosureAnalysisOn:indent:)
('decompiler' remoteNode:)
('code generation' emitCodeForLoad:forValue:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad:forValue: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('initialization' name:index:type:scope: nowHasDef nowHasRef scope:)
!


!Editor reorganize!
('accessing' currentAttributes help morph morph: userHasEdited)
('accessing-selection' selectionInterval)
('editing keys' copySelection: cut: noop: paste:)
('menu messages' clipboardStringOrText clipboardTextPut: lineSelectAndEmptyCheck: paste pasteString wordSelectAndEmptyCheck:)
('new selection' deselectAndPlaceCursorAt: selectInterval: selectInvisiblyFrom:to: selectInvisiblyMark:point: selectMark:point: selectWord selectWordLeftDelimiters:rightDelimiters: selectWordOrDelimitedText wordLeftDelimiters wordRangeLeftDelimiters:rightDelimiters: wordRangeLeftDelimiters:rightDelimiters:startingAt: wordRangeUnder: wordRightDelimiters)
('nonediting/nontyping keys' cursorDown: cursorLeft: cursorPageDown: cursorPageUp: cursorRight: cursorUp: selectWord:)
('typing/selecting keys' backspace: cursorTopHome: enter: newLine: normalCharacter: returnKey: selectAll selectAll:)
('private' beginningOfLine: beginningOfNextParagraph: beginningOfParagraph: beginningOfText endOfLine: endOfParagraph: endOfText moveCursor:forward:event: nextWordEnd: nextWordEndFrom:goingForwards: nextWordStart: nextWordStartFrom:goingForwards: previousWordStart: sameColumn:newLine:forward: setIndices:forward: stringClassToUse)
('menu commands' offerMenuFromEsc:)
('typing support' cmdShortcuts shortcuts)
!


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: selectionRectangle setSearch: text totalTextHeight visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selectedString selection startBlock startIndex stopBlock stopIndex)
('attributes' clearFont offerColorMenu offerDefaultFontMenu offerFontMenu offerFontStyleMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordUnder: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: insertAndSelect:at: lineSelectAndEmptyCheck: messageSendsRanges: selectFrom:to: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('typing support' addString: backTo: dispatchOn: processKeystrokeEvent: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete:)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: shouldDeleteAWordForward: shouldHandleUsingCmdShortcuts: storeSelectionInComposition stringClassToUse unapplyAttributes:)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' addCutAndPasteMenuSectionTo: addFinderMenuSectionTo: addStyleMenuSectionTo: addUndoMenuSectionTo: getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!


!SmalltalkEditor reorganize!
('accessing-selection')
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualExtractAsParameter: contextualRemoveParameter contextualRemoveParameter: extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: doItSourceCodeFor:in: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu: openSmalltalkEditorRefactoringMenu refactoringMenu)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethod: contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable inlineTemporaryVariable:)
!


!Preference class reorganize!
('accessing' allPreferences at: at:put: categories instanceAt: selectCategory:)
('instance creation' name:category:value: name:description:category:type:value:)
('error handling' doesNotUnderstand:)
('helpers' detectType:)
('event handlers' assignmentGlyph: defaultFontSize: machine: menuIcon: pointer:)
('fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('obsolete' desktopMenuTitle)
('as yet unclassified' openPreferencesInspector)
('toBeRemoved' isInitialFileListDirectory: soundsEnabled)
!


!PreferenceSet reorganize!
('instance creation' name:category:value: name:description:category:type:value:)
('accessing' allPreferences at: at:put: categories instanceAt: openPreferencesInspector selectCategory:)
('error handling' doesNotUnderstand:)
('fileIn/Out' loadFrom: save:to: saveAllTo: saveCategory:to:)
!


!PreferenceSet class reorganize!
('sys data' iconicHaloSpecifications machineDefault machineSlow machineSmalltalk80 prefFont prefGui prefProgramming prefSystem)
('sys preference' init installDefault:in: installHaloPreferencesWith: installMiscPreferences sysPreferences)
('sys fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('sys obsolete' desktopMenuTitle)
('sys event handlers' assignmentGlyph: defaultFontSize: machine: menuIcon: pointer:)
!


!Cursor class reorganize!
('accessing' cursorAt: cursorAt:ifAbsent: cursorAt:put: defaultCursor)
('class initialization' initialize startUp)
('current cursor' currentCursor currentCursor:)
('instance creation' blankCursor bottomLeftCursor bottomRightCursor cornerCursor crossHairCursor downCursor executeCursor markerCursor menuCursor moveCursor new normalCursor normalCursorWithMask originCursor readCursor resizeBottomCursor resizeBottomLeftCursor resizeBottomRightCursor resizeForEdge: resizeLeftCursor resizeRightCursor resizeTopCursor resizeTopLeftCursor resizeTopRightCursor rightArrowCursor squareCursor targetCursor topLeftCursor topRightCursor upCursor waitCursor webLinkCursor writeCursor)
!


!BitBltCanvasEngine reorganize!
('copying' clippedBy:)
('drawing support' fillOval: fillRect: frameOval:borderWidth: frameRect:borderWidth: frameRectBottom:height: frameRectRight:width: frameRectTopLeft:borderWidth: image:at:sourceRect:rule: stencil:at:sourceRect:)
('text' basicDisplayString:from:to:at:strikeFont: displayString:from:to:at:strikeFont:color:)
('private' cachedFontColormapFrom1BitTo: colorConvertingMap:from:to:keepSubPixelAA: installStrikeFont:foregroundColor: setRuleAndMapFor:foregroundColor:)
!


!StrikeFont reorganize!
('accessing' ascent baseFont baseFont: baseKern characterToGlyphMap descent familyName glyphs lineGap lineSpacing name name: pointSize subscript superscript widthOf: xTable)
('testing' isBaseFont isBold isItalic isStruckThrough isSubscript isSuperscript isUnderlined)
('emphasis' bonk:with: emphasis emphasis: emphasized: isSynthetic isSynthetic: makeBoldGlyphs makeItalicGlyphs makeStruckThroughGlyphs makeUnderlinedGlyphs releaseCachedState reset)
('file in/out' printOn:)
('glyphs' ensureCleanBold extendMaxAsciiTo: fillZeroWidthSlots glyphAt: glyphAt:put: makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useAlwaysLeftArrow useLeftArrow useShortUnderscore useUnderscore)
('private' createCharacterToGlyphMap)
('building' setGlyphsDepthAtMost: stripHighGlyphs)
('objects from disk' objectForDataStream:)
('synthetic derivatives' syntheticBold syntheticItalic syntheticST80Glyphs syntheticStruckThrough syntheticSubscript syntheticSuperscript syntheticUnderlined)
('copying' postCopy)
('BitBltCanvas support' onBitBltCanvasEngine:displayString:from:to:at:color:)
!


!MorphicScanner reorganize!
('scanning' displayBulletIfAppropriateFor:textLeft: displayBulletTextLeft:number: displayLine:textTopLeft:leftInRun: placeEmbeddedObject:)
('private' backgroundColor: setFont text:foreground: textColor:)
('stop conditions' crossedX doNewLine endOfRun paddedSpace setStopConditions tab)
('accessing' canvas:)
!


!FontFamily class reorganize!
('accessing' availableFamilies defaultFamilyName defaultPointSize familyNames)
('initialization' addFamily: defaultFamilyName: defaultFamilyName:defaultPointSize: defaultPointSize:)
('accessing fonts' defaultFamilyAndPointSize defaultFamilyPointSize: defaultOrTrueTypeFamilyAndPointSize enableTrueTypeFontsOnly familyName:pointSize: familyNamed: pointSizesFor:)
('file read write' readAdditionalTrueTypeFonts)
('cached state access' releaseClassCachedState)
('defaults' defaultLineSpacing)
('ui' promptUser promptUserAlt promptUserAndSetDefault promptUserFolders readIfNeeded:)
!


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky boundsFinderCanvas canvas color layerNumber location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' drawOn: drawingFails drawingFailsNot fullyCovers: hide icon imageForm: imageForm:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange: keyboardFocused)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayFullBounds extentChanged: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: fullIncludesPixel: includesPixel: isCloserThan:toPixel: on:ifCovered:uncoveredPartsDo:else: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: showBalloon:hand:doWordWrap: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!HandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('classification' is:)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:dragSel: waitingForMoreClicks)
('drawing' drawOn: fullDrawHandOn: hideHardwareCursor isDrawnBySoftware restoreSavedPatchOn: savePatchFrom: showHardwareCursor)
('event handling' createEventFrom:ofType: flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startDropFilesEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusNextWithinWindow keyboardFocusPrevious keyboardFocusPreviousWithinWindow mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('geometry' basicDisplayBounds displayFullBoundsForPatch morphPosition:)
('geometry testing' submorphsMightProtrude)
('grabbing/dropping' attachMorph: attachMorphBeside: dropMorph:event: dropMorphs: grabMorph: grabMorph:delta: grabMorph:moveUnderHand:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('testing' isIncludedInTaskbar)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('private' forgetGrabMorphDataFor: grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateDropFilesEvent: generateKeyboardEvent: generateMouseEvent: generateMouseScrollEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!


!WorldMorph reorganize!
('accessing' handlesKeyboard)
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' canvas clearCanvas ensureNonDisplayCanvas recordDamagedRect:for: resetCanvas setCanvas: setMainCanvas)
('change reporting' addedMorph: invalidateDisplayRect:for: removedMorph:)
('classification' isWorldMorph)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('drawing' displayWorld drawOn:)
('dropping/grabbing' allowsFilesDrop dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('event handling' mouseButton2Activity wantsWindowEvent:)
('events' click:localPosition: keyStroke: windowEvent:)
('geometry' basicDisplayBounds extentChanged: externalizeBoundsToWorld: externalizeDistanceToWorld: externalizeToWorld: internalizeDistanceFromWorld: internalizeFromWorld: morphPositionInWorld viewBox)
('halos and balloon help' deleteBalloonTarget: wantsHaloHandleWithSelector:inHalo:)
('hands' activeHand hands handsDo: handsReverseDo: handsToDrawForDamage:do:)
('initialization' clearWaitDelay initialize stepListSortBlock)
('interaction loop' mainLoop runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand:)
('misc' fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runLocalStepMethods: runStepMethods startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' topmostWorld world)
('submorphs-accessing' allMorphsDo: firstHand)
('submorphs-add/remove' addMorph:centeredNear:)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel:)
('update cycle' checkForNewScreenSize checkIfUpdateNeeded displayWorldSafely doOneCycle doOneCycleNow doOneMinimalCycleNow handleFatalDrawingError: tryDeferredUpdatingAndSetCanvas whenUIinSafeState:)
('windows' findATranscript)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphsDo:)
!


!WindowEdgeAdjustingMorph reorganize!
('initialization' initializeBottom initializeBottomLeft initializeBottomRight initializeLeft initializeRight initializeTop initializeTopLeft initializeTopRight)
('adjusting' adjustOwnerAt:millisecondSinceLast: basicAdjustOwnerAt:)
('accessing' cursor)
('drawing' drawOn:)
('geometry' minimumExtent)
('geometry services' coversLocalPoint:)
!


!StringRequestMorph reorganize!
('accessing' emptyTextDisplayMessage: layerNumber response response:)
('drawing' drawOn:)
('initialization' acceptBlock: addTextPane addTitle: cancelBlock: defaultBorderWidth defaultColor initialize setQuery:initialAnswer: validationBlock:)
('private' cancel getUserResponseOrCancel: selectionInterval textBox)
('testing')
!


!StringRequestMorph class reorganize!
('instance creation' defaultInitialAnswer request:centeredAt:initialAnswer:validationBlock:acceptBlock:cancelBlock: request:initialAnswer:do: request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel: request:orCancel:)
('private' deltaToTextPane)
('new-morph participation' categoryInNewMorphMenu initializedInstance)
!


!TaskbarMorph reorganize!
('accessing' buttonFor: scale)
('events' screenSizeChanged)
('initialization' balloonText defaultHeight defaultScale initialize)
('testing' is: isSticky)
('services' aboutToCollapse: addButtonFor: refreshTaskbarFor: removeButtonFor: restoreAll wasDeleted: wasOpened:)
('submorphs-add/remove' delete noteNewOwner:)
('menus' handlesMouseDown: mouseButton2Activity)
('resize' scale: scaleNormal scaleSmall scaleX2 scaleX4)
('notifications' fontPreferenceChanged)
!


!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawCoordinateSystemOn: drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:handPosition:hand: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' rejectsEvent: rejectsEventFully:)
('geometry services')
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addExploreHandle: addGrabHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addResizeHandle: addRotateHandle: addScaleHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor delete)
('testing' is: isIncludedInTaskbar wantsHalo)
('private' addHandle: addHandles addHandles: addNameString: doDebug:with: doDrag:with: doDup:with: doExplore:with: doGrab:with: doMenu:with: doRecolor:with: doResize:with: doRot:with: doScale:with: endInteraction handlesBox maybeCollapse:with: maybeDismiss:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startResize:with: startRot:with: startScale:with:)
('forward to target' deleteBalloon mouseDownOnHelpHandle:)
('stepping' step stepTime wantsSteps)
('geometry' morphLocalBounds)
!


!InnerHierarchicalListMorph reorganize!
('geometry' adjustExtent desiredWidth itemFromPoint:)
('drawing' drawLineToggleToTextFor:on:lineColor:hasToggle: drawLinesFor:on:lineColor: drawLinesOn: drawLinesToFirstChildFor:on:lineColor: drawLinesToNextSiblingFor:on:lineColor:hasToggle: drawOn:)
!


!InnerListMorph reorganize!
('initialization' initialize)
('list management' drawBoundsForRow: drawYForRow: highlightedRow: listChanged rowAtLocation: rowAtLocation:ifNone: selectedRow selectedRow:)
('drawing' bottomVisibleRowForCanvas: draw:atRow:on: drawBackgroundForMulti:on: drawHighlightOn: drawOn: drawSelectionOn: font font: topVisibleRowForCanvas:)
('list access' getListItem: getListSize item:)
('scroll range' desiredWidth widthToDisplayItem:)
('private' noSelection)
('geometry' adjustExtent fontPreferenceChanged)
!


!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped keystrokeAction: model: model:wrappedTo: positionInTextOf: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeystrokeEvent:)
('events-processing' processKeystroke: processMouseMove:localPosition:)
('geometry' adjustExtent extentChanged: minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnAny: acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
('copying' okayToDuplicate)
('hover help')
!


!LabelMorph reorganize!
('accessing' contents contents: contentsFromPrintString: font font:emphasis: fontToUse)
('drawing' drawOn:)
('font' emphasis:)
('halos and balloon help' addOptionalHandlesTo:box:)
('initialization' defaultBorderWidth defaultColor initWithContents:font:emphasis: initialize)
('menu' addCustomMenuItems:hand: changeEmphasis changeFont)
('printing' font: printOn:)
('geometry' fitContents fontPreferenceChanged measureContents minimumExtent)
!


!IndentingListItemMorph reorganize!
('accessing' balloonText beExpanded beFullyExpanded beFullyExpandedAsPer: canExpand firstChild indentLevel isExpanded isFirstItem isHighlighted: isSelected: isSoleItem nextSibling nextSibling:)
('converting' withoutListWrapper)
('drawing' drawOn:)
('geometry' desiredWidth fontPreferenceChanged)
('initialization' initWithContents:prior:forList:indentLevel: initialize)
('mouse events' inToggleArea:)
('private' hasToggle toggleRectangle withSiblingsDo:)
('private-container protocol' addChildrenForList:addingTo:withExpandedItems: complexContents openPath:adaptor:compare: recursiveAddTo: toggleExpandedState)
('events' mouseEnter: mouseLeave:)
('event handling testing' handlesMouseOver:)
!


!UpdatingLabelMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu initializedInstance)
!


!MenuItemMorph reorganize!
('accessing' contents: contentsWithMarkers:inverse: hasIcon hasMarker hasSubMenu isEnabled isEnabled: setBlankIcon setIcon: subMenu subMenu: target:selector:arguments:)
('drawing' drawOn:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('events' activateOwnerMenu: activateSubmenu: invokeWithEvent: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter:)
('events-processing' processMouseEnter:localPosition: processMouseLeave:localPosition:)
('grabbing' aboutToBeGrabbedBy: duplicateMorph:)
('initialization' deleteIfPopUp: initialize)
('layout' magnifiedIcon measureContents minItemWidth)
('selecting' deselect isSelected: select)
('private' offImage onImage)
('testing' is:)
!


!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: icon: iconDrawSelector iconDrawSelector: iconName iconName: label: label:font: magnifiedIcon: mouseOverColor: performAction roundButtonStyle:)
('drawing' draw3DLookOn: drawEmbossedLabelOn: drawOn: drawRegularLabelOn: drawRoundLookOn: enableColorWith: fontToUse)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition: mouseEnter: mouseLeave: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' actionArgument: defaultBorderColor defaultBorderWidth enableSelector: initialize model:stateGetter:action:label: model:stateGetter:stateArgument:action:actionArgument:label: secondaryActionSelector: stateArgument:)
('updating' update:)
('private' getEnabled getState magnifiedIcon)
('testing' is: isDisabled isEnabled isPressed isRoundButton mouseIsOver)
('geometry' extentChanged: minimumExtent)
('geometry testing')
('scrollbar button')
('enabling' disable enable)
!


!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: getMenu verticalScrollBar)
('access options' alwaysHideVerticalScrollbar alwaysShowVerticalScrollbar hideScrollBarsIndefinitely)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: mouseScroll:localPosition: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseScroll:)
('geometry' borderWidth: extentChanged: focusIndicatorBottom focusIndicatorExtent focusIndicatorLeft focusIndicatorRectangle focusIndicatorRight focusIndicatorTop fontPreferenceChanged hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar minimumExtent scrollDeltaHeight scrollDeltaHeight: scrollDeltaWidth scrollDeltaWidth: scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableArea viewableAreaBottom viewableAreaLeft viewableAreaRight viewableAreaTop viewableAreaTopLeft viewableExtent viewableHeight viewableWidth xtraBorder)
('geometry testing' clipsSubmorphs hIsScrollbarShowing vIsScrollbarShowing)
('initialization' initialize innerMorphClass scroller:)
('scroll bar events' mouseButton2Activity)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollSelectionIntoView scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('testing' drawsKeyboardFocusIndicator is:)
!


!HierarchicalListMorph reorganize!
('accessing' autoExpand rejectDrops roots selectedMorph)
('commands' expandAll expandAllAsPer: toggleExpandedState:event:)
('drawing' drawOn:)
('event handling' itemFromPoint: keyboardFocusChange: mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter:)
('events-processing' processMouseMove:localPosition:)
('geometry' scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: autoExpand: currentlyExpanded doubleClickSelector: indentingItemClass innerMorphClass list: listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: navigateDown navigateLeft navigateOnePageDown navigateOnePageUp navigateRight navigateToBottom navigateToTop navigateUp setSelectionIndex: toggleExpandedState:)
('menu' getMenu)
('model access' getList)
('selection' getCurrentSelectionItem highlightedMorph: listMorphAt: maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selectedMorph: selection: selectionIndex: setSelectedMorph: visualSelectionIndex)
('updating' update:)
('private' addMorphsTo:from:allowSorting:withExpandedItems:atLevel: addSubmorphsAfter:fromCollection:allowSorting: changeSelectionTo: indexForItem: insertNewMorphs: noteRemovalOfAll:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
!


!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' rejectDrops)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: keyboardFocused mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateOnePageDown navigateOnePageUp navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' allowsMultipleSelection is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo:)
('siblings')
('scrolling' scrollBy: scrollSelectionIntoView scrollSiblings: scrollToShow: updateSiblingsScroller vScrollBarValue:)
!


!TextModelMorph reorganize!
('accessing' disableEditing editor emptyTextDisplayMessage: enableEditing innerTextMorph wrapFlag:)
('dependents access' canDiscardEdits)
('drawing' drawOn: wantsFrameAdornments)
('editor access' scrollSelectionIntoView selectAll startIndex)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseEnter:)
('event handling' mouseButton2Activity)
('focus handling' focusText)
('geometry' innerHeight: scrollDeltaHeight)
('initialization' acceptOnCR: defaultColor escAction: innerMorphClass keystrokeAction: model:)
('interactive error protocol' correctFrom:to:with: nextTokenFrom:direction: notify:at:in: selectFrom:to: selectInvisiblyFrom:to: selectionInterval)
('model access' setSelection: setTextColor: text)
('notifications' possiblyChanged)
('scrolling' mightNeedHorizontalScrollBar)
('unaccepted edits' askBeforeDiscardingEdits: hasUnacceptedEdits:)
('updating' selectMessage selectString update: updateAcceptedContents updateActualContents updateAutoSelect updateClearUserEdits updateInitialSelection updateRefetched updateShoutStyled)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' fontPreferenceChanged justDroppedInto:event: minimumExtent rescaleButtons)
('geometry services' on:ifCovered:uncoveredPartsDo:else:)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeLabelArea openInWorld)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph layoutSubmorphs)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor: windowFrameColor)
('printing' printOn:)
('resize/collapse' allowedArea expandBoxHit resize resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-accessing')
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!


!CodePackageListWindow reorganize!
('GUI building' buildButtonPane buildMorphicWindow buildRequirementsPane initialExtent windowColor)
('commands' addRequirement browse browseChanges createPackage deletePackage)
('geometry' fontPreferenceChanged)
!


!CodeWindow reorganize!
('GUI building' addShiftClickEventHandlerFor: buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane createCodePaneMorph defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classAdded: classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode updateListsAndCodeNow)
('top window' activateAndSendTopToBack:)
('menu commands' browseAllMessages browseClassRefs browseClassVarRefs browseClassVariables browseFullProtocol browseHierarchy browseImplementors browseInstVarDefs browseInstVarRefs browseLocalImplementors browseLocalSendersOfMessages browseMessages browseMethodFull browseProtocol browseSenders browseSendersOfMessages browseSendersOfMethod browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodInheritance openSingleMessageBrowser shiftClickPressedOn:at: showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerClassListMenu offerClassListMenu2 offerWhatToShowMenu openMessageListMenu openMessageListMenu2)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('keyboard shortcuts' classListKey:from: messageCatListKey:from: messageListKey:from: systemCatListKey:from:)
('initialization' model:)
('notification actions' initializeNotificationActions registerNotificationActions registerNotificationActionsIfModelNotNil removeNotificationActions)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' messageListKey:from: systemCatListKey:from: systemCatSingletonKey:from:)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter changeKeywordOrder classRefactoringMenu inlineMethod messageRefactoringMenu moveToInstanceOrClassMethod openClassRefactoringMenu openMessageRefactoringMenu pushDownInstanceVariable pushDownSelector pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('commands' findClass)
!


!CodeFileBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalButtonTuples windowColor)
('menu building' classListMenu codeFileListMenu messageCategoryMenu messageListMenu)
('menu commands' browseMethodFull browseVersions methodInheritance)
('keyboard shortcuts' classListKey:from: codeFileListKey:from: messageListKey:from:)
('commands' findClass)
!


!MessageSetWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow windowColor)
('menu building' addExtraMenu2ItemsTo:)
('menu commands' filterMessageList)
!


!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu building' changeSetMenu classListMenu messageMenu)
('menu commands' browseMethodConflicts browseVersions)
('keyboard shortcuts' changeSetListKey:from: classListKey:from: messageListKey:from:)
!


!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow createCodePaneMorph customButtonRow customButtonSpecs initialExtent optionalButtonTuples windowColor)
('menu building' contextFieldListMenu contextStackMenu receiverFieldListMenu)
('menu commands' contextClassHierarchy exploreContextSelection exploreReceiverSelection receiverClassHierarchy returnValue shiftClickPressedOn:at:)
('keyboard shortcuts' browseClassRefsIn: browseFullProtocolIn: browseHierarchyIn: browseMethodFullIn: contextStackKey:from: exploreSelectionIn: inspectSelectionIn: inspectorKey:from:)
('open/close' closeView delete)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack peelToFirst proceed restart runToCursor send stepIntoBlock where)
('notification actions' registerNotificationActions)
!


!PreDebugWindow reorganize!
('GUI building' buildMorphicWindowMessage: buttonRowForPreDebugWindow: initialExtent preDebugButtonSpec windowColor)
('button actions' abandon createMethod debug delete proceed)
('open/close' closeView)
('notification actions' registerNotificationActions)
!


!PreDebugWindow class reorganize!
('instance creation' forceOpen:label:message: open:label:message:)
!


!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('menu building' fileListMenu fileSelectedMenu noFileSelectedMenu volumeMenu)
('updating' updateButtonRow)
('keyboard shortcuts' fileListKey:from:)
!


!InspectorWindow reorganize!
('GUI building' buildContentsText buildEvaluatorText buildList buildMorphicWindow initialExtent)
('menu building' basicMenuOptions fieldListMenu menuOptionsForBrowsing menuOptionsForDictionary menuOptionsForSet)
('menu commands' addEntry browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelectedKey inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('keyboard shortcuts' inspectorKey:from:)
('accessing')
!


!ObjectExplorerWindow reorganize!
('GUI building' buildMorphicWindow expandAll initialExtent)
('building menus' genericMenu)
('keyboard shortcuts' explorerKey:from:)
('menu commands' browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelection monitor: openWeightExplorer stopMonitoring)
('menu commands - reference finder' objectsToSkipDuringRescan openReferencesFinder rescan)
('stepping' stepAt:)
!


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items itemsDo: label lastItem layerNumber stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('geometry' fontPreferenceChanged)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' isIncludedInTaskbar)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!


!AutoCompleterMorph class reorganize!
('preferences' itemHeight listFont)
('instance creation' completer:position: initializedInstance)
('new-morph participation' categoryInNewMorphMenu)
!


!KeyboardEvent reorganize!
('comparing' = hash)
('actions' closeCurrentWindowOf:)
('dispatching' sendEventTo: startDispatchFrom:)
('keyboard' keyCharacter keyValue)
('testing' isAlphaNumeric isArrowDown isArrowLeft isArrowRight isArrowUp isBackspace isCloseWindowShortcut isColon isCtrlSpace isDelete isEnd isEsc isFindClassShortcut isHome isKeyDown isKeyUp isKeyboard isKeystroke isPageDown isPageUp isQuestionMark isReturnKey isSpace isTab)
('private' setType:buttons:position:keyValue:hand:stamp:)
('platform peculiarities' isCmdAltGreaterThan isCmdAltLessThan)
!


!MouseButtonEvent reorganize!
('accessing' mouseButton1Changed mouseButton2Changed mouseButton3Changed)
('dispatching' dispatchWith: sendEventTo:)
('private' setType:position:which:buttons:hand:stamp:)
!


!TheWorldMenu reorganize!
('commands' cleanUpWorld fullScreenOff fullScreenOn garbageCollect newMorphOfClass:event: quitSession saveWorldInFile splitNewMorphList:depth: startMessageTally vmStatistics)
('construction' alphabeticalMorphMenu buildWorldMenu changesMenu debugMenu helpMenu newMorph openMenu preferencesMenu)
('mechanics' menu: myWorld world:hand:)
('menu' openCommentGuide openPackageDownloader openTerseGuide)
('popups' doPopUp: openChangesMenu openDebugMenu openHelpMenu openOpenMenu openPreferencesMenu openWindowsMenu)
('windows & flaps menu' windowsMenu)
('private' pvtAlphaSplitListDepth pvtCategorySplitListDepth pvtMaxItemsPerCategorySubMenu pvtNewMenuForSplitLists:)
!


!MorphicCanvas reorganize!
('accessing' boundsFinderCanvas clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistCenter:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: drawTextComposition:at:extent:color:selectionColor: drawTextComposition:at:extent:color:selectionColor:avoidOverhang: textComposition:bounds:color:selectionColor: textComposition:bounds:color:selectionColor:avoidOverhang:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawExpandedAt: drawLeftIcon drawMenuIcon drawNotExpandedAt: drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-widgets' drawButtonIconFromCurrentMorph)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight:)
('initialization' ensureCurrentMorphIsWorld initializeWithTranslation: resetCanvas world:)
('morphic' clipCurrentMorph: contourClearOnCurrentMorph currentMorphDrawingFails displayBoundsSetOnCurrentMorph displayBoundsUpdateOnCurrentMorph drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawHand drawHighlight: fullDraw: fullDrawHand: into: outOfMorph)
('other' showAt:invalidRect:)
('printing' printOn:)
('testing' canDoVectorGraphics drawsOnDisplay isBoundsFinderCanvas usesVectorEnginePlugin)
('private' canvasOrigin canvasToUse clippingByCurrentMorphDo: fontToUse: setClipRect:)
('private-framework' privateFrameAndFillGlobalRect:fillColor:borderWidth:color: privateFrameGlobalRect:borderWidth:color: privateFrameReverseGlobalRect:borderWidth:)
('morphic world' boundingRectOfCurrentMorphAfterDraw computeDamage:repair:rootMorphs:rootMorphsDamage: drawWorld:repair: drawWorld:rootMorphs:rootMorphsDamage:backgroundDamage: drawWorldBackground:rects:)
!


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel errorColor failureColor focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarColor scrollbarSliderShadowColor successColor text textCursor textEmptyDisplayMessage textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' autoCompleter autoCompleterDefaultBorderColor autoCompleterMaybeInvalid browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageDownloader packageInstaller packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton hoverHelp listHighlightFocused: listMultiHighlightFocused: textPane)
('other options' buttonBorderWidth buttonPaneHeight embossedButtonLabels embossedTitles fullScreenDeskMargin layoutAdjusterThickness menuBorderWidth roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarShowButtons scrollbarThickness titleBarButtonsExtent titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useTaskbar useUniformColors useWindowTitleGradient windowBorderWidth)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelOne blockLevelSeven blockLevelSix blockLevelThree blockLevelTwo blockLevelZero blockTemps defaults generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloResizeIcon haloRotateIcon haloScaleIcon helpIcon imageIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon resizeBottomIcon resizeBottomLeftIcon resizeBottomRightIcon resizeFullIcon resizeLeftIcon resizeRightIcon resizeTopIcon resizeTopLeftIcon resizeTopRightIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('shout' shout)
!


!Theme class reorganize!
('instance creation' beCurrent)
('accessing' content content: current)
('importing/exporting' bootstrap)
('class initialization' currentTheme:)
('user interface' changeFontSizes changeIcons changeTheme useMenuIcons useNoMenuIcons)
('consistency verification' verifyShoutConfig)
!


!ChangeSet reorganize!
('change logging' noteAddClass: noteChangeClass:from: noteClassMoveToOtherPackage: noteMethodMoveToOtherPackage:forClass: noteNewMethod:forClass:selector:priorMethod: noteRecategorizationOfClass: noteRemoveSelector:class:priorMethod:lastMethodInfo: noteRenameClass:as:)
('accessing' author codePackage codePackage: editPostscript hasPostscript hasUnsavedChanges hasUnsavedChanges: isForBaseSystem isForBaseSystem: methodChanges methodInfoFromRemoval: movedClassRecords name name: printOn: removePostscript removedClassRecords sortedClassRecords: sortedMovedClassesToOtherPackage sortedRemovedClassRecords)
('testing' canHavePreambleAndPostscript isEmpty isMoribund isWithClass: methodsWithoutClassifications okayToRemove)
('method changes' changedMessageList changedMessageListAugmented messageListForChangesWhich:ifNone: methodChangesAtClass: removeSelectorChanges:class: selectorsInClass:)
('class changes' changedClassCategories changedClassNames changedClasses classChangeAt: fatDefForClass: noteClassForgotten: noteClassStructure: noteCommentClass: noteRemovalOf:fromCategory: noteReorganizeClass: trimHistory)
('moving changes' hasPreamble methodsWithAnyInitialsOtherThan: methodsWithInitialsOtherThan: methodsWithoutComments removeClassChanges: removePreamble)
('fileIn/Out' askAddedInstVars: askRemovedInstVars: askRenames:addTo:using: assurePostscriptExists assurePreambleExists checkForSlips fileOut fileOutClassDefinitionsOf:on: fileOutClassDeletionFrom:doItOnlyIfInBaseSystem:on: fileOutClassInitializationsOf:on: fileOutMethodAdditionsFor:on: fileOutMethodChangesFor:on: fileOutMethodMovedToOtherPackagesFor:on: fileOutMethodRemovalOf:for:movedToOtherPackage:on:stamp: fileOutMethodRemovalsFor:on: fileOutMethodRemovalsOf:movedToOtherPackage:for:on: fileOutMethodsAdditionsOf:on: fileOutMethodsChangesOf:on: fileOutMovedClassRecord:on: fileOutMovedClassesOn: fileOutOn: fileOutPSFor:on: fileOutPostscriptOn: fileOutPreambleOn: fileOutRemovedAndMovedMethodsOf:on: fileOutRemovedClassRecord:on: fileOutRemovedClassesOn: objectForDataStream: postscriptString postscriptString: preambleString preambleString: preambleTemplate)
('private' addCoherency: atClass:add: atClass:includes: atSelector:class: changeRecorderFor: fileOutClassDefinition:on: oldNameFor:)
('system change notifications' aboutToRenameClass:from:to:inCategory: classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classReorganized: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('initialization' clear initialize wither)
!


!ChangeSet class reorganize!
('class initialization' initialize initializeNotificationActions registerNotificationActions removeNotificationActions)
('instance creation' named: new)
('system change notifications' aboutToRenameClass:from:to:inCategory: classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classReorganized: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('change set to use' baseSystemNameFor: changeSetForBaseSystem changeSetForPackage: currentBaseCSNumber incrementCurrentBaseCSNumber installing:do: installingString isInstalling: notInstallOrTestRun runningTest:do:)
('file list services' fileReaderServicesForFileEntry:suffix: serviceFileIn serviceInstall)
('services' fileIn: install: installNewUpdates installNewUpdates: isNewUpdate: newUpdates:)
('enumerating' allChangeSets allChangeSetsWithClass: allChangeSetsWithClass:selector: changeSetNamed: countOfChangeSetsWithClass:andSelector: doesAnyChangeSetHaveClass:andSelector: existingOrNewChangeSetForPackage: existingOrNewChangeSetNamed:forBaseSystem:)
('removing' removeChangeSet: zapAllChangeSets)
!


!CodeFile reorganize!
('accessing' classDictionary classNamed: classes description fullName fullName: name organization removeClass: removeDoIts renameClass:to: sourceSystem summary)
('change record types' classComment: classDefinition: classRemoval: doIt: method: preamble:)
('fileIn/fileOut' askForDoits fileIn fileInDoits fileOut fileOutDoits: fileOutOn:)
('initialization' initialize)
('reading' buildFrom: buildFrom:informingTo:)
('testing' isLiveSmalltalkImage)
('xtras' allMethodReferences)
('private' baseLabel classDefinition:with: getClass: metaClassDefinition:with: msgClassComment:with: possibleSystemSource: removedMethod:with:)
!


!CodePackageFile reorganize!
('services' install)
('accessing' classesToRemove description featureSpec methodsToRemove packageName provides requires summary)
('change record types' doIt: possibleDescription: provides: requires:)
('initialization' buildFileStream:packageName:fullName: initialize)
!


!TestResult reorganize!
('accessing' correctCount defects errorCount errors failureCount failures passed passedCount removeFromDefectsAndAddToPassed: runCount tests)
('testing' hasErrors hasFailures hasPassed isError: isFailure: isPassed:)
('printing' printOn:)
('running' runCase:)
('logging' reportAboutToRun: reportError:because: reportFailed:because: reportPassed:)
!


!SHTextStylerST80 reorganize!
('styling' formatWorkspace: styleWorkspaceFrom:to:allowEmphasis:)
('private - show subscript' formatAsSubscript: hasToShowSubscriptOf: showAsSubscriptIfAppropriate:)
('private' applySmalltalkStylingIn:using: attributesFor: convertAssignmentsToAnsi convertAssignmentsToLeftArrow optimizeForMutationSpeed: parseSetWorkspace: privateFormatAndConvert privateStyle replaceStringForRangesWithType:with: setAttributesFromRanges:)
('accessing' classOrMetaClass: disableFormatAndConvert workspace:)
('initialization' initialize)
!

SHTextStylerST80 initialize!

!SHTextStylerST80 class reorganize!
('preferences' ansiAssignmentPreferenceChanged leftArrowAssignmentPreferenceChanged)
('style table' attributeArrayForColor:emphasis: attributesFor: defaultStyleTable initialTextAttributes styleTable)
('class initialization' initialize releaseClassCachedState)
!


!PopUpMenu reorganize!
('accessing' frameHeight iconAt: labelString lineArray)
('basic control sequence' startUpMenu startUpNonModalWithCaption: startUpNonModalWithCaption:at:allowKeyboard: startUpSegmented:withCaption:at:allowKeyboard: startUpWithCaption: startUpWithCaption:at:allowKeyboard:)
('private' labels:lines: labels:lines:icons:)
!


!ExtractToTemporary reorganize!
('applying' apply)
('initialization' initializeNamed:extractingCodeAt:from:declaringTempIn:)
('private - applying steps ' addAssignmentToCurrentStatement addNewTemporaryVariableToExistingDeclarationStatement declareNewTemporaryVariable extractingAnEntireStatement formattedNewVariableAssignment hasTemporariesDeclarationBlock insertAt:newCodeWith: insertNewTemporaryDeclarationWithNewVariable positionToInsertNewTemporaryVariableAssignment positionToInsertNewVariableDeclaration reflectSourceCodeChanges replaceExtractedCodeWithNewTemporaryVariable resolveNewVariableAssignment siblingStatementsOfTemporaryAssignment statementNodeIncludingCodeToExtract writeAssignmentStatementOfNewTemporaryVariable)
('private - applying steps' preferredAssignmentOperator)
!

