'From Cuis 6.0 [latest update: #5337] on 30 June 2022 at 6:10:34 pm'!

!Preference commentStamp: 'jmv 6/30/2022 18:00:11' prior: 0!
My instance is a Preference whose value is of a given class (type), or follow the description of the type instance

- name, category: symbol
- description: string
- type: a class (Boolean, String, Color, BlockClosure, etc.) or an instance (collection, intervale, etc.)
- value: an object whose class match type or the instance description of the type

Modus Operendi
- To create a Preference, invoke the instance creation class method (Preference name:description:category:type:value:). The new preference is automatically remembered. If a preference with same name already exist, its instance is updated with the new provided data and its instance returned.
- To access, invoke as dictionary (Preferences at: #biggerCursors), a short cut exist (Preference biggerCursors)
- When a preference value is changed with its #value message, an event #preferenceChanged is triggered.
- Tools can listen to a specific #preferenceChanged event: 
	(Preference instanceAt: #guiElementSize) when: #preferenceChanged send: #shakeScreen: to: myTool
By default the Preference class listen to a few event to adjust the behavior of the image to some preference changes.	
!


!PluggableTextModel methodsFor: 'misc' stamp: 'hlsf 6/23/2022 11:21:51'!
refetch
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ].
			^false ]
		ifNotNil: [
			self actualContents: (Text
				initialFont: (Preferences at: #standardCodeFont)
				stringOrText: (textProvider perform: textGetter)).
			self changed: #refetched.
			^true ]! !


!Workspace methodsFor: 'shout styling' stamp: 'hlsf 6/23/2022 11:21:59'!
shouldStyle

	^shouldStyle ifNil: [ Preferences at: #shoutInWorkspaces]! !

!Workspace methodsFor: 'user interface support' stamp: 'hlsf 6/23/2022 11:21:54'!
allowStylingWithEmphasis
	"Disabled by default for faster styling of large contents, as text metrics are not affected by styling."

	^ Preferences at: #stylingWithEmphasisInWorkspaces! !

!Workspace methodsFor: 'user interface support' stamp: 'hlsf 6/23/2022 11:21:57'!
fullPrintIt

	^fullPrintIt ifNil: [ Preferences at: #fullPrintItInWorkspaces]! !


!TextProvider methodsFor: 'contents' stamp: 'hlsf 6/23/2022 11:22:01'!
acceptedContents
	^ Text
		initialFont: (Preferences at: #standardCodeFont)
		stringOrText: self acceptedStringOrText! !


!CodeProvider methodsFor: 'annotation' stamp: 'hlsf 6/23/2022 11:22:21'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name.
		(Preferences at: #classAnnotations) do: [ :each |
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

!CodeProvider methodsFor: 'annotation' stamp: 'hlsf 6/23/2022 11:22:27'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	^ String streamContents: [ :strm |
		(Preferences at: #methodAnnotations)
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

!CodeProvider methodsFor: 'annotation' stamp: 'hlsf 6/23/2022 11:22:31'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		(Preferences at: #systemCategoryAnnotations) do: [ :each |
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

!CodeProvider methodsFor: 'contents' stamp: 'hlsf 6/23/2022 11:22:33'!
contentsSymbol
	"Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source.  A nil value in the contentsSymbol slot will be set to #source by this method"

	^ contentsSymbol ifNil: [
		contentsSymbol _ (Preferences at: #browseWithPrettyPrint)
					ifTrue:
						[#prettyPrint]
					ifFalse:
						[#source]]! !

!CodeProvider methodsFor: 'diffs' stamp: 'hlsf 6/23/2022 11:22:35'!
defaultDiffsSymbol
	"Answer the code symbol to use when generically switching to diffing"

	^ (Preferences at: #diffsWithPrettyPrint)
		ifTrue: [
			#prettyLineDiffs]
		ifFalse: [
			#lineDiffs]! !


!Browser methodsFor: 'class functions' stamp: 'hlsf 6/23/2022 11:22:39'!
classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	^ Text
		initialFont: (Preferences at: #standardCodeFont)
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass comment ]
				ifFalse: [ '' ]).! !

!Browser methodsFor: 'class functions' stamp: 'hlsf 6/23/2022 11:22:42'!
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
					((Preferences at: #leftArrowAssignmentsInGeneratedCode)
						ifTrue: [ '_' ]
						ifFalse: [ ':=' ]) , ' anObject'.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ]]]! !


!Browser class methodsFor: 'class list' stamp: 'hlsf 6/23/2022 11:22:44'!
disableListClassesHierarchically
	
	^Preferences at: #listClassesHierarchically put: false! !

!Browser class methodsFor: 'class list' stamp: 'hlsf 6/23/2022 11:22:46'!
enableListClassesHierarchically
	
	^Preferences at: #listClassesHierarchically put: true! !

!Browser class methodsFor: 'class list' stamp: 'hlsf 6/23/2022 11:22:48'!
listClassesHierarchically
	
	^Preferences at: #listClassesHierarchically! !


!CodeFileBrowser methodsFor: 'edit pane' stamp: 'hlsf 6/23/2022 11:22:50'!
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
	(Preferences at: #browseWithPrettyPrint) ifTrue: [
		answer _ class compilerClass new
						format: answer in: class notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		answer _ self
			methodDiffFor: answer
			selector: self selectedMessageName ].
	^ answer! !


!ChangeList methodsFor: 'initialization-release' stamp: 'hlsf 6/23/2022 11:22:56'!
initialize
	"Initialize a blank ChangeList.  Set the contentsSymbol to reflect whether diffs will initally be shown or not"

	contentsSymbol _ (Preferences at: #diffsInChangeList)
		ifTrue:
			[self defaultDiffsSymbol]
		ifFalse:
			[#source].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	super initialize! !

!ChangeList methodsFor: 'menu actions' stamp: 'hlsf 6/23/2022 11:22:53'!
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
							font: (Preferences at: #standardCodeFont)))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !


!Debugger class methodsFor: 'class initialization' stamp: 'hlsf 6/23/2022 11:22:58'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	<primitive: 19> "Simulation guard"

	(self errorRecursion not and: [Preferences at: #logDebuggerStackToFile]) ifTrue:
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

!Debugger class methodsFor: 'opening' stamp: 'hlsf 6/23/2022 11:23:01'!
openInterrupt: aString onProcess: interruptedProcess
	"Open a notifier in response to an interrupt. An interrupt occurs when the user types the interrupt key (cmd-. on Macs, ctrl-c or alt-. on other systems) or when the low-space watcher detects that memory is low."
	| debugger |
	<primitive: 19> "Simulation guard"
	debugger _ self new.
	debugger
		process: interruptedProcess
		context: interruptedProcess suspendedContext.
	debugger externalInterrupt: true.

	(Preferences at: #logDebuggerStackToFile) ifTrue:
		[(aString includesSubString: 'Space') & 
			(aString includesSubString: 'low') ifTrue: [
				Smalltalk logError: aString inContext: debugger interruptedContext to: 'LowSpaceDebug']].

	^ debugger
		openNotifierContents: nil
		label: aString
! !

!Debugger class methodsFor: 'opening' stamp: 'hlsf 6/23/2022 11:23:03'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| w |
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
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


!FileList methodsFor: 'initialization' stamp: 'hlsf 6/23/2022 11:23:09'!
initialDirectoryList

	| initialDirectoryListFromPreferences wrapperCreator |
	wrapperCreator _ [ :directoryEntry |
		FileDirectoryWrapper
			with: directoryEntry
			name: (directoryEntry name ifNil: [ '/' ])
			model: self ].
	(initialDirectoryListFromPreferences _ Preferences at: #initialFileListDirectories) 
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

!FileList methodsFor: 'volume list and pattern' stamp: 'hlsf 6/23/2022 11:23:05'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences at: #standardListFont.
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

!FileList methodsFor: 'volume list and pattern' stamp: 'hlsf 6/23/2022 11:23:11'!
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
	font _ Preferences at: #standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !


!Color class methodsFor: 'colormaps' stamp: 'hlsf 6/23/2022 11:23:15'!
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
	bitsPerColor _ bitsPerColor min: (Preferences at: #aaFontsColormapDepth).
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


!Behavior methodsFor: 'testing method dictionary' stamp: 'hlsf 6/23/2022 11:23:19'!
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."

	| who |

	(Preferences at: #thoroughSenders)
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


!Message methodsFor: 'stub creation' stamp: 'hlsf 6/23/2022 11:23:21'!
addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ((Preferences at: #leftArrowAssignmentsInGeneratedCode)
			ifTrue: [ ' _ ' ]
			ifFalse: [ ' := ' ]);
		nextPutAll: self arguments first argumentName ! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'hlsf 6/23/2022 11:25:26'!
defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), 
		(Preferences at: #userChangesFileNameExtension)! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'hlsf 6/24/2022 09:45:41'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	p _ UISupervisor newUIProcessIfNeeded.
	p ifNil: [
		p _ Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
			ifTrue: [Utilities reportCPUandRAM]
			ifFalse: [Utilities processTakingMostCPU]].
	(Preferences at: #cmdDotEnabled) ifTrue: [
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

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'hlsf 6/23/2022 11:25:37'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	| localFilename file |
	localFilename _ (Preferences at: #debugLogTimestamp)
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

!SystemDictionary methodsFor: 'shrinking' stamp: 'hlsf 6/23/2022 11:23:28'!
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
	Preferences at: #warnIfNoChangesFile put: false.
	Preferences at: #warnIfNoSourcesFile put: false! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'hlsf 6/23/2022 11:25:51'!
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
	Preferences at: #wantsMenuIcons put: false.
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

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'hlsf 6/23/2022 11:25:12'!
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
	(Preferences at: #askConfirmationOnQuit) ifTrue: [
		^self confirm: 'Do you really want to exit Cuis without saving the image?' ].
	^true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'hlsf 6/24/2022 09:44:43'!
setPlatformPreferences
	"Set some platform specific preferences on system startup"
	| platform specs |	

	(Preferences at: #automaticPlatformSettings) ifFalse:[^self].
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
		Preferences name: tuple first category: #system value: (tuple last == #true) 	]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'hlsf 6/23/2022 11:25:23'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preferences at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory _ oldUserChanges parent.
			oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine ].
	StartupStamp _ nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'hlsf 6/24/2022 09:45:48'!
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
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry appendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [Preferences at: #warnIfNoChangesFile])
		ifTrue: [self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].
	ChangesInitialFileSize _ changes ifNotNil: [ changes position ].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'hlsf 6/23/2022 11:25:32'!
hasToRestoreChanges

	^(Preferences at: #checkLostChangesOnStartUp) and: [ 
		self withChangesFileDo: [ :changesFile | self hasToRestoreChangesFrom: changesFile ]].
	! !


!Exception methodsFor: 'priv handling' stamp: 'hlsf 6/23/2022 11:26:45'!
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
					(Preferences at: #allowNonLocalReturnsInExceptionHandlers)
						ifFalse: [ self error: 'Exception handler blocks must not do non local returns' ]
						ifTrue: [
							(Preferences at: #warnAboutNonLocalReturnsInExceptionHandlers)
								ifTrue: [ 'It is advisable to avoid method returns (non local returns) in exception handler blocks' print ].
							handlerBlockNotCurtailed _ true ].
					]
				ifFalse: [handlerBlockNotCurtailed _ true]
		]! !


!NegativePowerError methodsFor: 'exceptionDescription' stamp: 'hlsf 6/23/2022 11:26:55'!
defaultAction

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	(Preferences at: #askToInstallComplexPackage) ifTrue: [
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
			(Preferences at: #askToInstallComplexPackage put: false)]].
	^ super defaultAction! !


!Parser methodsFor: 'scanning' stamp: 'hlsf 6/24/2022 09:29:19'!
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
					(Preferences at: #atMinusDigitMeaning) == #disabled
						"Disallow `1@-2`. Insists on some whitespace to disambiguate."
						ifTrue: [ ^self expected: 'A space character after selector' ].
					(Preferences at: #atMinusDigitMeaning) == #st80
						ifTrue: [ ^self ].				"interpret `1@-2` like `1 @ -2`"
					"Assume (Preferences at: #atMinusDigitMeaning) == #ansiSmalltalk
					interpret `1@-2` like `1 @- 2`, i.e. consider #@- a valid binary selector"
					].
			here _ here , token asPlainString.
			hereEnd _ hereEnd + 1.
			self scanToken ].! !


!BlockNode methodsFor: 'printing' stamp: 'hlsf 6/24/2022 09:29:25'!
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
	(Preferences at: #prettyPrintRectangularBlocks)
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

!BlockNode methodsFor: 'testing' stamp: 'hlsf 6/24/2022 09:29:28'!
printsInNewLine
	"Used for pretty printing to determine whether to start a new line"

	(Preferences at: #prettyPrintRectangularBlocks) ifFalse: [ ^false ].
	^super printsInNewLine! !


!TempVariableNode methodsFor: 'testing' stamp: 'hlsf 6/24/2022 09:29:31'!
assignmentCheck: encoder at: location
	^((self isBlockArg and: [(Preferences at: #allowBlockArgumentAssignment) not])
	    or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]! !


!Editor methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:29:36'!
setIndices: shiftPressed forward: forward
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices _ Dictionary new.
	(shiftPressed and:[Preferences at: #selectionsMayShrink])
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


!TextEditor methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:29:40'!
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
		caseSensitive: ((self class changeText ~~ self class findText) or: [Preferences at: #caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'hlsf 6/24/2022 09:29:42'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preferences at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

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


!Preference class methodsFor: 'event handlers' stamp: 'jmv 6/30/2022 18:00:13'!
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
	Preferences at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font
" Copied to PreferenceSet "! !

!Preference class methodsFor: 'fonts' stamp: 'jmv 6/30/2022 18:05:27'!
setDefaultFont: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	Preference
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#standardListFont. (Preferences at: #standardListFont) pointSize.}.
				{#standardMenuFont. (Preferences at: #standardMenuFont) pointSize.}.
				{#windowTitleFont. (Preferences at: #windowTitleFont) pointSize.}.
				{#standardCodeFont. (Preferences at: #standardCodeFont) pointSize.}.
				{#standardButtonFont. (Preferences at: #standardButtonFont) pointSize.}.
			}.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
" Copied to PreferenceSet "! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 6/30/2022 18:05:16'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences 

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 3 max: 16];
	
		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];
		
		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;
		
		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];
		
		name: #classFinder 
		category: #programming 
		value: [ BrowserWindow findClass ];
		
		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName].! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 6/30/2022 18:00:31'!
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
	Preferences at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'hlsf 6/24/2022 09:30:12'!
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
		min: `52@40` * (Preferences at: #standardCodeFont) lineSpacing) rounded! !


!Cursor class methodsFor: 'accessing' stamp: 'hlsf 6/24/2022 09:30:19'!
defaultCursor
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ (Preferences at: #biggerCursors)
		ifTrue: [ CursorWithAlpha biggerNormal ]
		ifFalse: [ self cursorAt: #normalCursorWithMask ].! !

!Cursor class methodsFor: 'current cursor' stamp: 'hlsf 6/24/2022 09:30:15'!
currentCursor: aCursor 
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor _ aCursor.
	(Preferences at: #biggerCursors)
		ifTrue: [
			[ ^aCursor asBigCursor installCursor] 
				on: Error do: nil  "fall through "].
	aCursor installCursor! !


!BitBltCanvasEngine methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:30:28'!
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
			(Preferences at: #subPixelRenderFonts) and: [ foregroundColor = `Color black` or: [ 
				(Preferences at: #subPixelRenderColorFonts) and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= `Color black` or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences at: #properDisplayAlphaForFonts] ]]) ifTrue: [
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


!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'hlsf 6/24/2022 09:30:30'!
syntheticST80Glyphs
	"Build and answer a derivative that includes ST-80 glyphs:
	a left arrow instead of the underscore, and an up arrow instead of the caret."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'ST80';
		perform: (Preferences at: #assignmentGlyphSelector).
	^ derivative! !


!MorphicScanner methodsFor: 'scanning' stamp: 'hlsf 6/24/2022 09:30:54'!
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
			((Preferences at: #backgroundColorFillsAllBackground) and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			((Preferences at: #backgroundColorFillsAllBackground) and: [stopCondition = #tab]) ifTrue: [
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


!FontFamily class methodsFor: 'ui' stamp: 'jmv 6/30/2022 18:07:35'!
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
					ifTrue: [PreferenceSet setDefaultFont: fontFamily familyName ]
					ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]]]].! !


!Morph methodsFor: 'events' stamp: 'hlsf 6/24/2022 09:31:13'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	(Preferences at: #focusFollowsMouse)
		ifTrue: [evt hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ].! !

!Morph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:31:06'!
displayBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ (Preferences at: #haloEnclosesFullBounds)
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 6/24/2022 09:31:02'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	(Preferences at: #haloSpecifications) do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 6/24/2022 09:31:10'!
haloShowsCoordinateSystem

	^Preferences at: #halosShowCoordinateSystem! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 6/24/2022 09:31:15'!
wantsBalloon
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText notNil) and: [Preferences at: #balloonHelpEnabled]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 6/24/2022 09:31:20'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].

	(Preferences at: #selectiveHalos) ifFalse: [
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


!HandMorph methodsFor: 'double click support' stamp: 'hlsf 6/24/2022 09:31:47'!
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
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'hlsf 6/24/2022 09:31:50'!
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
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'hlsf 6/24/2022 09:31:54'!
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
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:31:43'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware
			ifTrue: [
				(Preferences at: #cacheDisplayContentWhenMovingMorphs)
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

!HandMorph methodsFor: 'private events' stamp: 'hlsf 6/24/2022 09:31:30'!
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
	((buttons = InputSensor controlKey and: [Preferences at: #ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
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


!WorldMorph methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:46:04'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
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
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
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

!WorldMorph methodsFor: 'update cycle' stamp: 'hlsf 6/24/2022 09:32:01'!
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
	(Preferences at: #serverMode)
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

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 6/30/2022 18:07:48'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: PreferenceSet desktopMenuTitle.
	menu popUpInWorld: self! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'hlsf 6/24/2022 09:32:07'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		(Preferences at: #cheapWindowReframe) or: [millisecondSinceLast > 200]]) ifTrue: [
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner morphExtentInWorld ]].! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'hlsf 6/24/2022 09:32:10'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ BoxedMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: (Preferences at: #standardMenuFont) bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphWidth! !

!StringRequestMorph methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:32:16'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w _ self world.
	w isNil ifTrue: [^ response asString].
	done _ false.
	canceled _ false.
	(Preferences at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock _ [:aString| done _ true].
	cancelBlock _ [done _ true. canceled _ true].
	delay _ Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'hlsf 6/24/2022 09:32:19'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	| answer |
	answer _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	(Preferences at: #focusFollowsMouse) ifFalse: [answer textBox focusText].
	^ answer! !

!StringRequestMorph class methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:32:13'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e _ (Preferences at: #windowTitleFont) lineSpacing.
	^ (0)@(0.5 * e)! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'hlsf 6/24/2022 09:32:22'!
defaultHeight

	^ ((Preferences at: #windowTitleFont) lineSpacing * 2 * self scale) asInteger! !


!HaloMorph methodsFor: 'handles' stamp: 'hlsf 6/24/2022 09:32:29'!
addDebugHandle: handleSpec

	(Preferences at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doDebug:with: ]
! !

!HaloMorph methodsFor: 'handles' stamp: 'hlsf 6/24/2022 09:32:31'!
addExploreHandle: handleSpec

	(Preferences at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doExplore:with: ]
! !

!HaloMorph methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:32:36'!
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
	e _ (Preferences at: #haloHandleSize) asPoint.
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

!HaloMorph methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:32:39'!
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
	verticalNamePosition _ haloBox bottom + (Preferences at: #haloHandleSize).
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:32:43'!
handlesBox
	"handlesBox is in local coordinates.
	We assume we are direct submorph of the world, without any scaling or rotation."

	| minSide hs c e box |
	hs _ Preferences at: #haloHandleSize.
	minSide _ 4 * hs.
	e _ extent + (hs*2) max: minSide@minSide.
	c _ extent // 2 + self morphPosition.
	box _ Rectangle center: c extent: e.
	self world ifNotNil: [ :w | box _ box intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	"Make it local"
	^box translatedBy: self morphPosition negated.
! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:32:45'!
drawOn: aCanvas

	(Preferences at: #showLinesInHierarchyViews) ifTrue:[
		self drawLinesOn: aCanvas ]! !


!InnerListMorph methodsFor: 'initialization' stamp: 'hlsf 6/24/2022 09:32:49'!
initialize
	super initialize.
	self color: `Color black`.
	font _ Preferences at: #standardListFont.
	listItems _ #().
	selectedRow _ nil.
	highlightedRow _ nil! !

!InnerListMorph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:32:47'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).! !


!InnerTextMorph methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:32:52'!
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
		((Preferences at: #syntaxHighlightingAsYouType) ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !


!LabelMorph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:32:56'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).
	self fitContents.! !


!IndentingListItemMorph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:32:58'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'hlsf 6/24/2022 09:33:01'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: (Preferences at: #standardListFont) emphasis: nil.
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


!UpdatingLabelMorph class methodsFor: 'new-morph participation' stamp: 'hlsf 6/24/2022 09:33:04'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (Preferences at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'hlsf 6/24/2022 09:33:06'!
initialize
	"initialize the state of the receiver"
	super initialize.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preferences at: #standardMenuFont.
	self contents: ''.! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:33:08'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [Preferences at: #standardButtonFont].
	"
	Could add emphasis...
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]
	"
	^fontToUse! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:33:11'!
minimumExtent
	
	| unit |
	unit :=  (Preferences at: #windowTitleFont) pointSize.
	^label 
		ifNil: [ (unit+2) @ (unit+2) ] "Assume title button"
		ifNotNil: [ (4 * unit) @ (3 * unit) ] "Assure some space for text."
! !


!PluggableScrollPane methodsFor: 'initialization' stamp: 'hlsf 6/24/2022 09:33:14'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ Preferences at: #drawKeyboardFocusIndicator.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.
	self updateScrollBarsBounds.
	self innerMorphClass ifNotNil: [ :contentsClass |
		self scroller: contentsClass new ].! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:33:16'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: (Preferences at: #focusIndicatorWidth)
			color: Theme current focusIndicator ]! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'hlsf 6/24/2022 09:33:18'!
mouseEnter: event
	super mouseEnter: event.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !


!PluggableListMorph methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:33:20'!
drawOn: aCanvas
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: (Preferences at: #focusIndicatorWidth)
			color: Theme current focusIndicator ].! !

!PluggableListMorph methodsFor: 'events' stamp: 'hlsf 6/24/2022 09:33:24'!
mouseEnter: event
	super mouseEnter: event.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!PluggableListMorph methodsFor: 'geometry' stamp: 'hlsf 6/24/2022 09:33:21'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).! !


!TextModelMorph methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:33:28'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ Preferences at: #focusIndicatorWidth.
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

!TextModelMorph methodsFor: 'events' stamp: 'hlsf 6/24/2022 09:33:30'!
mouseEnter: event
	super mouseEnter: event.
	(Preferences at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: scroller ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'hlsf 6/24/2022 09:33:41'!
drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f _ Preferences at: #windowTitleFont.
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

!SystemWindow methodsFor: 'events' stamp: 'hlsf 6/24/2022 09:33:46'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph"
	^aMorph isWorldMorph or:[Preferences at: #systemWindowEmbedOK]! !

!SystemWindow methodsFor: 'label' stamp: 'hlsf 6/24/2022 09:33:44'!
labelHeight
	"Answer the height for the window label."
	^ (Preferences at: #windowTitleFont) lineSpacing+1! !

!SystemWindow methodsFor: 'menu' stamp: 'hlsf 6/24/2022 09:33:37'!
addTileResizerMenuTo: aMenu
	"We can look at preferences here to decide what too do"
	(Preferences at: #tileResizerInWindowMenu) ifFalse: [
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

!SystemWindow methodsFor: 'open/close' stamp: 'hlsf 6/24/2022 09:33:39'!
closeBoxHit
	"The user clicked on the close-box control in the window title.  For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down."

	(Preferences at: #dismissAllOnOptionClose) ifTrue:
		[Sensor rawMacOptionKeyPressed ifTrue:
			[^ self world closeUnchangedWindows]].
	self delete
! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:33:48'!
initialExtent

	^`540@400` * (Preferences at: #standardCodeFont) lineSpacing // 14! !


!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:33:54'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:33:56'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	(Preferences at: #shiftClickShowsImplementors)
		ifTrue: [ self addShiftClickEventHandlerFor: codePane ].
		
	^codePane! !

!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:34:06'!
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

		((Preferences at: #decorateBrowserButtons)
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

!CodeWindow methodsFor: 'updating' stamp: 'hlsf 6/24/2022 09:34:00'!
decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	(Preferences at: #decorateBrowserButtons)
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


!BrowserWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:34:14'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code _ self buildMorphicCodePane.
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons _ LayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 18:03:12'!
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
		addMorph: mySingletonList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/30/2022 18:03:14'!
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
		addMorph: sysCatList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:34:29'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:34:31'!
initialExtent
	^`540@300` * (Preferences at: #standardCodeFont) lineSpacing // 14! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:34:36'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	(Preferences at: #optionalButtons) ifTrue: [
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
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:34:38'!
initialExtent
	^ `640 @ 320` * (Preferences at: #standardCodeFont) lineSpacing // 14! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'hlsf 6/24/2022 09:34:40'!
open: model label: aString message: messageString
	
	((Preferences at: #usePreDebugWindow) or: [messageString notNil])
		ifTrue: [self forceOpen: model label: aString message: messageString]
		ifFalse: [model openFullMorphicLabel: aString ]! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 6/30/2022 18:08:28'!
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
				target: Preferences
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:35:07'!
initialExtent

	^`600@325` * (Preferences at: #standardCodeFont) lineSpacing // 14! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'hlsf 6/24/2022 09:35:09'!
initialExtent

	^`300@500` * (Preferences at: #standardCodeFont) lineSpacing // 14! !


!MenuMorph methodsFor: 'construction' stamp: 'hlsf 6/24/2022 09:35:18'!
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
	wantsIcons _ Preferences at: #wantsMenuIcons.
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

!MenuMorph methodsFor: 'construction' stamp: 'hlsf 6/24/2022 09:35:22'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	(Preferences at: #optionalButtons) ifFalse: [ ^self ].
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

!MenuMorph methodsFor: 'construction' stamp: 'hlsf 6/24/2022 09:35:26'!
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
		font _ Preferences at: #standardMenuFont.
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

!MenuMorph methodsFor: 'control' stamp: 'hlsf 6/24/2022 09:35:32'!
popUpAt: aPoint forHand: hand in: aWorld
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self 
		popUpAt: aPoint 
		forHand: hand 
		in: aWorld 
		allowKeyboard: (Preferences at: #menuKeyboardControl)! !

!MenuMorph methodsFor: 'control' stamp: 'hlsf 6/24/2022 09:35:34'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or:
		[Preferences at: #systemWindowEmbedOK]! !

!MenuMorph methodsFor: 'modal control' stamp: 'hlsf 6/24/2022 09:35:29'!
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: (Preferences at: #menuKeyboardControl)

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


!AutoCompleterMorph class methodsFor: 'preferences' stamp: 'hlsf 6/24/2022 09:35:37'!
listFont

	^Preferences at: #standardListFont! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'hlsf 6/24/2022 09:35:41'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut
			ifTrue: [ ^ (Preferences at: #classFinder) value ].
		self isCloseWindowShortcut
			ifTrue: [ ^ self closeCurrentWindowOf: aMorph ].
		^ aMorph processKeystroke: self ].
	type == #keyDown ifTrue: [
		^ aMorph processKeyDown: self ].
	type == #keyUp ifTrue: [ 
		^ aMorph processKeyUp: self ].
	^ super sendEventTo: aMorph.! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'hlsf 6/24/2022 09:35:52'!
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
						dragSel: ((Preferences at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
						clkSel: ((Preferences at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					(Preferences at: #clickGrabsMorphs) ifFalse: [
						self shiftPressed
							ifTrue: [ aMorph is: #SystemWindow :: ifTrue: [ aMorph sendToBack ] ]
							ifFalse: [ aMorph activateWindow ] ].
					self wasHandled: true.
					^ self ]]].

	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'hlsf 6/24/2022 09:36:20'!
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
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !


!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:36:23'!
buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^(Preferences at: #standardButtonFont) lineSpacing * 14 // 8! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:31'!
fullScreenDeskMargin
	^ (Preferences at: #fullScreenLeavesDeskMargins) ifTrue: [48] ifFalse: [0]! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:45'!
layoutAdjusterThickness

	self flag: #todo.  "Revisit this; consider moving proportional stuff out of Theme entirely."
	
	^ (Preferences at: #standardListFont) pointSize // 3! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:51'!
roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^ Preferences at: #roundedButtonRadius! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:53'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^Preferences at: #roundedWindowRadius! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:55'!
scrollbarThickness
	^ Preferences at: #scrollbarThickness! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:39:57'!
titleBarButtonsExtent
	"Answer the extent to use for close & other title bar buttons.  
	The label height is used to be proportional to the fonts preferences."
	| e |
	e _ (Preferences at: #windowTitleFont) pointSize * 13 // 10.
	^e@e! !

!Theme methodsFor: 'other options' stamp: 'hlsf 6/24/2022 09:40:15'!
windowBorderWidth
	| w |
	w _ (Preferences at: #standardListFont) pointSize / 11.
	w _ w * (self roundWindowCorners ifTrue: [4] ifFalse: [2]).
	^ w rounded max: 1! !

!Theme methodsFor: 'private - shout mappings' stamp: 'hlsf 6/24/2022 09:39:36'!
italic
	^(Preferences at: #italicsInShout)
		ifTrue: [ #italic ]
		ifFalse: [ #normal ]! !


!Theme class methodsFor: 'user interface' stamp: 'hlsf 6/24/2022 09:39:13'!
changeFontSizes
	| options menu preference  |
	preference _ Preferences instanceAt: #guiElementsSize.
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

!Theme class methodsFor: 'user interface' stamp: 'hlsf 6/24/2022 09:39:59'!
useMenuIcons
	Preferences at: #wantsMenuIcons put: true! !

!Theme class methodsFor: 'user interface' stamp: 'hlsf 6/24/2022 09:40:01'!
useNoMenuIcons
	Preferences at: #wantsMenuIcons put: false! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'hlsf 6/24/2022 09:40:32'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse _ (Preferences at: #changeSetVersionNumbers)
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
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'hlsf 6/24/2022 09:40:38'!
preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: (Preferences at: #defaultAuthorName).
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"! !


!ChangeSet class methodsFor: 'services' stamp: 'hlsf 6/24/2022 09:40:34'!
install: aFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: aFileEntry name do: [ self fileIn: aFileEntry ].
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		('Installed ChangeSet: ', aFileEntry name) print]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'hlsf 6/24/2022 09:40:42'!
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
		(Preferences at: #transcriptLogVerbose) ifTrue: [
			Transcript newLine; show:'Filing in ', cls name].
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]! !


!CodePackageFile methodsFor: 'services' stamp: 'hlsf 6/24/2022 09:40:48'!
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
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage! !


!TestResult methodsFor: 'logging' stamp: 'hlsf 6/24/2022 09:40:50'!
reportAboutToRun: aTestCase
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'Will run: '; print: aTestCase; newLine]! !

!TestResult methodsFor: 'logging' stamp: 'hlsf 6/24/2022 09:40:53'!
reportFailed: aTestCase because: anException
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript print: anException; newLine].! !

!TestResult methodsFor: 'logging' stamp: 'hlsf 6/24/2022 09:40:54'!
reportPassed: aTestCase
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'finished.'; newLine]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:41:47'!
privateFormatAndConvert
	"Perform any formatting of formattedText necessary and store or a formatted copy in formattedText"
	(Preferences at: #syntaxHighlightingAsYouTypeAnsiAssignment) ifTrue: [
		self convertAssignmentsToAnsi ].
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self convertAssignmentsToLeftArrow ]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'hlsf 6/24/2022 09:41:51'!
privateStyle

	| alpha end start count startIndexes c hue |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(Preferences at: #highlightBlockNesting) ifTrue: [
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


!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'hlsf 6/24/2022 09:40:59'!
ansiAssignmentPreferenceChanged
	"the user has changed the syntaxHighlightingAsYouTypeAnsiAssignment setting.
	If they have turned it on then force syntaxHighlightingAsYouTypeLeftArrowAssignment
	to be turned off"
	(Preferences at: #syntaxHighlightingAsYouTypeAnsiAssignment) 		ifTrue: [
		Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment put: false]! !

!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'hlsf 6/24/2022 09:41:43'!
leftArrowAssignmentPreferenceChanged
	"the user has changed the syntaxHighlightingAsYouTypeLeftArrowAssignment setting.
	If they have turned it on then force syntaxHighlightingAsYouTypeAnsiAssignment
	to be turned off"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) 		ifTrue:[
		Preferences at: #syntaxHighlightingAsYouTypeAnsiAssignment put: false]! !

!SHTextStylerST80 class methodsFor: 'class initialization' stamp: 'hlsf 6/24/2022 09:41:05'!
initialize  
	"Clear styleTable and textAttributesByPixelSize cache so that they will 
	reinitialize.	 

		SHTextStylerST80 initialize
	" 
	
	styleTable := nil.
	textAttributes := nil.	
	"We want to be  informed at preference changes"
	(Preferences instanceAt: #syntaxHighlightingAsYouTypeAnsiAssignment)
		when: #preferenceChanged
		send: #ansiAssignmentPreferenceChanged
		to: self.
	(Preferences instanceAt: #syntaxHighlightingAsYouTypeLeftArrowAssignment)
		when: #preferenceChanged
		send: #leftArrowAssignmentPreferenceChanged
		to: self! !


!PopUpMenu methodsFor: 'accessing' stamp: 'hlsf 6/24/2022 09:41:53'!
frameHeight
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems _ 1 + labelString lineCount.
	^ (nItems * (Preferences at: #standardMenuFont) lineSpacing) + 4 "border width"! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 6/24/2022 09:41:54'!
startUpNonModalWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpNonModalWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (Preferences at: #menuKeyboardControl)! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 6/24/2022 09:42:07'!
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
	nLinesPer _ segmentHeight // (Preferences at: #standardMenuFont) lineSpacing - 5.
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

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 6/24/2022 09:42:09'!
startUpWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (Preferences at: #menuKeyboardControl)! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'hlsf 6/24/2022 09:42:12'!
preferredAssignmentOperator

	^ (Preferences at: #leftArrowAssignmentsInGeneratedCode)
		ifTrue: [ '_' ]
		ifFalse: [ ':=' ]! !

SHTextStylerST80 initialize!
