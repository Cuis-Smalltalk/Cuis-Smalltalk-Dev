'From Cuis 4.2 of 25 July 2013 [latest update: #1929] on 7 January 2014 at 10:07:15.066336 pm'!

!Object methodsFor: 'printing' stamp: 'jmv 1/7/2014 22:03'!
displayStringOrText
	"To be used in the UI"

	^self printString! !


!MessageSet methodsFor: 'message list' stamp: 'jmv 1/7/2014 21:51'!
addMethodReference: aMethodReference
	"Append a aMethodReference to the list.  Select the new item."

	messageList add: aMethodReference.
	self changed: #messageList.
	self messageListIndex: messageList size! !


!String methodsFor: 'converting' stamp: 'jmv 1/7/2014 22:05'!
displayStringOrText
	"To be used in the UI"
	"Answer this string."

	^ self
! !


!Text methodsFor: 'converting' stamp: 'jmv 1/7/2014 22:05'!
displayStringOrText
	"To be used in the UI"
	"Answer the receiver itself."

	^self! !


!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 21:42'!
printStringWithCommas
	"
	123456789 printStringWithCommas
	-123456789 printStringWithCommas
	"
	^String streamContents: [ :strm |
		self printOn: strm thousandSeparator: ',' includePlusSign: false ]! !


!MethodReference methodsFor: 'printing' stamp: 'jmv 1/7/2014 22:03'!
displayStringOrText
	"To be used in the UI"

	^stringVersion! !


!MessageSet methodsFor: 'message functions' stamp: 'jmv 1/7/2014 21:52'!
deleteFromMessageList: aMethodReference
	"Delete the given message from the receiver's message list"

	messageList _ messageList copyWithout: aMethodReference! !


!RecentMessageSet methodsFor: 'update' stamp: 'jmv 1/7/2014 21:54'!
updateListsAndCodeIn: aWindow

	| recentFromUtilities |
	"RAA 20 june 2000 - a recent change to how messages were displayed in the list caused them not to match what was stored in Utilities. This caused the recent submissions to be continuously updated. The hack below fixed that problem"

	self flag: #mref.	"in second pass, use simpler test"

	aWindow canDiscardEdits ifFalse: [^ self ].
	recentFromUtilities _ Utilities mostRecentlySubmittedMessage,' '.
	(messageList first stringVersion beginsWith: recentFromUtilities)
		ifFalse: [
			self reformulateList ]
		ifTrue: [
			self updatePaneIfNeeded ]! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 1/7/2014 21:44'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item item2 tokens firstToken secondToken stamp anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem feature |
	itemPosition _ file position.
	item _ file nextChunk.
	(((item includesSubString: 'commentStamp:')
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:']
	or: [item includesSubString: 'provides']
	or: [item includesSubString: 'requires'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Smalltalk actualScannerClass new scanTokens: item.
	tokens size >= 2 ifTrue: [
		stamp _ ''.
		anIndex _ tokens indexOf: #stamp: ifAbsent: nil.
		anIndex ifNotNil: [stamp _ tokens at: (anIndex + 1)].
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition: ifTrue: [
			itemPosition _ file position.
			isMeta _ secondToken includesSubString: ' class'.
			name _ isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
			def _ file nextChunk.
			record _ ChangeRecord new file: file position: itemPosition type: #classDefinition
				class: name asSymbol category: tokens last meta: isMeta stamp: nil.
			self addItem: record text: 'classDefinition: ', def.
			^ self ].

		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #classMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				self 
					addItem: (ClassDeletionChangeRecord new
						clsName: secondToken;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['clase move to some package: '] ifFalse: ['class removal: ']), secondToken ].
			^ self ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #methodMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				isMeta _ tokens third == #class.
				isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
				methodReference _ (MethodReference new
					setClassSymbol: secondToken
					classIsMeta: isMeta
					methodSymbol: tokens last
					stringVersion: secondToken, ' ', (isMeta ifTrue: ['class '] ifFalse: ['']), tokens last).
				self
					addItem: (MethodDeletionChangeRecord new
						methodReference: methodReference;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['method move to some package: '] ifFalse: ['method removal: ']), 
							methodReference stringVersion ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp].

		secondToken == #commentStamp: ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self ].

		firstToken == #provides: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].

		firstToken == #requires: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #requires
					feature: (FeatureRequirement name: secondToken minVersion: tokens third minRevision: tokens fourth maxVersion: (tokens size > 4 ifTrue: [tokens fifth])).
			self
				addItem: feature
				text: feature string ].
	]! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 1/7/2014 21:45'!
browsePackageContents: aStream
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	aStream readOnly.
	changeList _ self new scanFile: aStream from: 0 to: aStream size.
	aStream reset.
	packageFile _ CodePackageFile buildFileStream: aStream.
	aStream close.
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream localName! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 1/7/2014 21:39'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ ((Date fromSeconds: entry modificationTime) printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			(Time fromSeconds: entry modificationTime \\ Time secondsInDay)
				print24: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize printStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas + sizePad - sizeDigits.
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 1/7/2014 21:39'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	selected _ Set new.
	patternsThatSelect do: [ :pat |
		directory entries do: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList _ selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry]) ifTrue: [
				newList remove: entry ]]].
		
	newList _ newList asArray sort: self sortBlock.
	font _ Preferences standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 1/7/2014 21:39'!
report: strm cutoff: threshold
	tallyRoot tally = 0
		ifTrue: [ strm nextPutAll: ' - no tallies obtained' ]
		ifFalse: [
			strm
				nextPutAll: 'Reporting - ' , totalTally printStringWithCommas;
				nextPutAll: ' tallies, ' , totalTime printStringWithCommas , ' msec.';
				newLine;
				newLine.
			tallyRoot
				fullPrintOn: strm
				threshold: threshold
				time: totalTime
				reportOnly: observedProcess ].
	totalTime isZero ifFalse: [
		self reportGCStatsOn: strm.
		self reportProcessStatsOn: strm ].! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 1/7/2014 21:40'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := totalTime.
	oldSpaceEnd			:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (fullGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (incrGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows printStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 1/7/2014 21:51'!
askRenames: renamed addTo: msgSet using: smart
	| list |
	"Go through the renamed classes.  Ask the user if it could be in a project.  Add a method in SmartRefStream, and a conversion method in the new class."

	list _ OrderedCollection new.
	renamed do: [ :cls | | rec |
		rec _ changeRecords at: cls name.
		rec priorName ifNotNil: [
			| ans |
			ans _ PopUpMenu withCaption: 'You renamed class ', rec priorName, 
				' to be ', rec thisName,
				'.\Could an instance of ', rec priorName, 
				' be in a project on someone''s disk?'
			chooseFrom: #('Yes, write code to convert those instances'
				'No, no instances are in projects').
			ans = 1 ifTrue: [
				| oldStruct newStruct  |
				oldStruct _ structures at: rec priorName ifAbsent: nil.
				newStruct _ (Array with: cls classVersion), (cls allInstVarNames).
				oldStruct ifNotNil: [
					smart writeConversionMethodIn: cls fromInstVars: oldStruct 
							to: newStruct renamedFrom: rec priorName.
					smart writeClassRename: cls name was: rec priorName.
					list add: cls name, ' convertToCurrentVersion:refStream:']]
			ifFalse: [structures removeKey: rec priorName ifAbsent: nil]]].
	list isEmpty ifTrue: [^ msgSet].
	msgSet messageList ifNil: [msgSet initializeMessageList: list]
		ifNotNil: [list do: [:item | msgSet addMethodReference: item]].
	^ msgSet! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 1/7/2014 21:40'!
bytesLeftString
	"Return a string describing the amount of memory available"
	| availInternal availPhysical availTotal |
	self garbageCollect.
	availInternal _ self primBytesLeft.
	availPhysical _ self bytesLeft: false.
	availTotal _ self bytesLeft: true.
	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"
		ifFalse:[^availInternal printStringWithCommas, ' bytes available'].
	^String streamContents:[:s|
		s nextPutAll: availInternal printStringWithCommas, 	' bytes (internal) '; newLine.
		s nextPutAll: availPhysical printStringWithCommas,	' bytes (physical) '; newLine.
		s nextPutAll: availTotal printStringWithCommas, 	' bytes (total)     '].! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 1/7/2014 21:40'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact

	isExact
		ifTrue: [ 
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [ receivers asArray do: [ :r | myTally := myTally - r tally ] ].
			aStream
				print: myTally;
				space ]
		ifFalse: [ 
			| percentage |
			percentage := tally asFloat / total * 100.0.
			percentage printOn: aStream fractionDigits: 1.
			aStream
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
				nextPutAll: ' ms) '].
	self isPrimitives
		ifTrue: [ 
			aStream
				nextPutAll: 'primitives';
				newLine ]
		ifFalse: [ 
			| className aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector contractTo: self maxClassPlusSelectorSize - className size);
				newLine ]! !


!LookupKey methodsFor: 'bindings' stamp: 'jmv 1/7/2014 21:46'!
recompileBindingsAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-only binding"
	aBool ifTrue:[
		Utilities informUserDuring: [ :bar |
			(Smalltalk allCallsOn: self) do: [ :mref | 
				bar value: 'Recompiling ', mref stringVersion.
				mref actualClass recompile: mref methodSymbol ].
		].
	] ifFalse:[
		(Smalltalk allCallsOn: self) do: [ :mref |
			mref actualClass recompile: mref methodSymbol ]
	]! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 1/7/2014 21:40'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 

	isExact 
		ifTrue: [
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
			aStream
				print: myTally;
				space]
		ifFalse: [
			| percentage |
			percentage := tally asFloat / total * 100.0.
			percentage printOn: aStream fractionDigits: 1.
			aStream
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
				nextPutAll: 'ms) '].
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			| className aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector 
							contractTo: self maxClassPlusSelectorSize - className size);
				newLine]! !

!MessageTally methodsFor: 'reporting' stamp: 'jmv 1/7/2014 21:41'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime _ time.
	oldSpaceEnd			_ gcStats at: 1.
	youngSpaceEnd		_ gcStats at: 2.
	memoryEnd			_ gcStats at: 3.
	fullGCs				_ gcStats at: 7.
	fullGCTime			_ gcStats at: 8.
	incrGCs				_ gcStats at: 9.
	incrGCTime			_ gcStats at: 10.
	tenureCount			_ gcStats at: 11.
	rootOverflows		_ gcStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (fullGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (incrGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows printStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !


!Integer methodsFor: 'benchmarks' stamp: 'jmv 1/7/2014 21:40'!
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results"
	"0 tinyBenchmarks"
	"On a 292 MHz G3 Mac: 22,727,272 bytecodes/sec; 984,169 sends/sec"
	"On a 400 MHz PII/Win98:  18,028,169 bytecodes/sec; 1,081,272 sends/sec"
	"On a 1.6GHz Atom/Win7 with Cog:  201,099,764 bytecodes/sec; 28,197,075 sends/sec"
	| t1 t2 r n1 n2 |
	n1 _ 1.
	[
		t1 _ Time millisecondsToRun: [n1 benchmark].
		t1 < 1000] 
			whileTrue:[n1 _ n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 _ 28.
	[
		t2 _ Time millisecondsToRun: [r _ n2 benchFib].
		t2 < 1000] 
			whileTrue:[n2 _ n2 + 1]. 
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^ ((n1 * 500000 * 1000) // t1) printStringWithCommas, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printStringWithCommas, ' sends/sec'! !


!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 1/7/2014 22:03'!
getList
	"Answer the list to be displayed.  Caches the returned list in the 'list' ivar"
	getListSelector
		ifNil: [^ #()].
	list _ model perform: getListSelector.
	list
		ifNil: [^ #()].
	list _ list collect: [ :item | item displayStringOrText ].
	^ list! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 1/7/2014 21:45'!
openSingleMessageBrowser
	| msgName mr |
	"Create and schedule a message list browser populated only by the currently selected message"

	(msgName _ model selectedMessageName) ifNil: [^ self].

	mr _ MethodReference new
		setStandardClass: model selectedClassOrMetaClass
		methodSymbol: msgName.

	Smalltalk 
		browseMessageList: (Array with: mr)
		name: mr stringVersion
		autoSelect: nil! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 1/7/2014 22:03'!
drawOn: aCanvas

	listItems size = 0 ifTrue: [ ^self ].
 
	self drawHighlightOn: aCanvas.
	self drawSelectionOn: aCanvas.

	(self topVisibleRowForCanvas: aCanvas)
		to: (self bottomVisibleRowForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) displayStringOrText atRow: row on: aCanvas ]! !


!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 1/7/2014 21:54'!
mostRecentlySubmittedMessage
	"Answer a string indicating the most recently submitted method that is still extant"

	self flag: #mref.	"fix for faster references to methods"

	self assureMostRecentSubmissionExists.
	^ RecentSubmissions last stringVersion! !

!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 1/7/2014 21:55'!
noteMethodSubmission: selectorName forClass: class

	| submission |

	self flag: #mref.	"fix for faster references to methods"

	self recentMethodSubmissions.	"ensure it is valid"
	class wantsChangeSetLogging ifFalse: [^ self].
	self purgeRecentSubmissionsOfMissingMethods.
	submission _ class name asString, ' ', selectorName.
	RecentSubmissions removeAllSuchThat: [ :each |
		each stringVersion = submission
	].
	RecentSubmissions size >= self numberOfRecentSubmissionsToStore ifTrue: [
		RecentSubmissions removeFirst
	].
	RecentSubmissions addLast: (
		MethodReference new
			setClass: class 
			methodSymbol: selectorName 
			stringVersion: submission
	) 
! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 1/7/2014 21:41'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime upTime2 fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	params := Smalltalk getVMParameters.
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memoryEnd			:= params at: 3.
	fullGCs				:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs				:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.
	upTime := Time millisecondClockValue.

	str := WriteStream on: (String new: 1000).
	str	nextPutAll: 'uptime			';
		print: (upTime / 1000 / 60 // 60); nextPut: $h;
		print: (upTime / 1000 / 60 \\ 60) asInteger; nextPut: $m;
		print: (upTime / 1000 \\ 60) asInteger; nextPut: $s; newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: memoryEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd - oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	memoryEnd - youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.

	str	nextPutAll: 'GCs				';
		nextPutAll: (fullGCs + incrGCs) printStringWithCommas.
	fullGCs + incrGCs > 0 ifTrue: [
		str
			nextPutAll: ' ('; 
			print: ((upTime / (fullGCs + incrGCs)) rounded); 
			nextPutAll: 'ms between GCs)'
	].
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime / upTime * 100) rounded);
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse: [
		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime / upTime * 100) rounded);
		nextPutAll: '% uptime), avg '; print: ((incrGCTime / incrGCs) rounded); nextPutAll: 'ms'; newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	nextPutAll: self textMarkerForShortReport ;
		nextPutAll: (fullGCs2 + incrGCs2) printStringWithCommas.
	fullGCs2 + incrGCs2 > 0 ifTrue: [
		str
			nextPutAll: ' ('; 
			print: ((upTime2 / (fullGCs2 + incrGCs2)) rounded); 
			nextPutAll: 'ms between GCs)'.
	].
	str newLine.
	str	nextPutAll: '	uptime		'; print: ((upTime2 / 1000.0) rounded); nextPutAll: 's'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 printStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime2 / upTime2 * 100) rounded);
		nextPutAll: '% uptime)'.
	fullGCs2 = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 printStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime2 / upTime2 * 100) rounded);
		nextPutAll: '% uptime), avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) rounded); nextPutAll: 'ms'
	].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount2 printStringWithCommas.
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
].
	LastStats at: 1 put: upTime.
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
! !

!methodRemoval: MethodReference #asStringOrText!
MethodReference removeSelector: #asStringOrText!
!methodRemoval: MethodReference #displayString!
MethodReference removeSelector: #displayString!

!MethodReference reorganize!
('queries' actualClass category classIsMeta classSymbol compiledMethod isValid methodSymbol printOn: selector sourceCode)
('setting' setClass:methodSymbol:stringVersion: setClassAndSelectorIn: setClassSymbol:classIsMeta:methodSymbol:stringVersion: setStandardClass:methodSymbol:)
('string version' stringVersion stringVersion:)
('comparisons' <= = hash)
('services' updateReferencesTo:toBe:)
('printing' displayStringOrText)
!

!methodRemoval: Integer #asStringWithCommas!
Integer removeSelector: #asStringWithCommas!
!methodRemoval: Integer #asStringWithCommasSigned!
Integer removeSelector: #asStringWithCommasSigned!
!methodRemoval: Text #asStringOrText!
Text removeSelector: #asStringOrText!
!methodRemoval: String #asStringOrText!
String removeSelector: #asStringOrText!
!methodRemoval: MessageSet #addItem:!
MessageSet removeSelector: #addItem:!
!methodRemoval: Object #asStringOrText!
Object removeSelector: #asStringOrText!
!methodRemoval: Object #displayString!
Object removeSelector: #displayString!
