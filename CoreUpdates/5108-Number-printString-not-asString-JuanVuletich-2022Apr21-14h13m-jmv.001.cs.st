'From Cuis 6.0 [latest update: #5107] on 21 April 2022 at 2:14:21 pm'!

!Workspace methodsFor: 'variable declarations' stamp: 'jmv 4/21/2022 12:40:46'!
nameForObject: object
	"Answer a name suitable for a Workspace variable"
	^ (object class name, object identityHash printString) asIdentifier: false! !


!ReferencesExplorer methodsFor: 'references' stamp: 'jmv 4/21/2022 12:40:50'!
getList

	| wrapper |
	wrapper _ ReferencesExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash printString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper! !


!WeightExplorer methodsFor: 'references' stamp: 'jmv 4/21/2022 12:40:54'!
getList

	| wrapper |
	wrapper _ WeightExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash printString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 4/21/2022 12:40:58'!
openMorphicWindowForSuspendedProcess: aProcess
	| menu rule |
	menu _ MenuMorph new.
	"nickname  allow-stop  allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) second.
	menu add: 'Dismiss this menu' target: menu action: #delete; addLine.
	menu add: 'Open Process Browser' target: ProcessBrowserWindow action: #openProcessBrowser.
	menu add: 'Resume'
		target: self
		action: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		action: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rule ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			action: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash printString,
		' ', aProcess name,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp.
	menu popUpInWorld
! !


!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 4/21/2022 12:45:28'!
processNameList
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally _ CPUWatcher current ifNotNil: [ :pw | pw tally ].
	^ (processList asOrderedCollection
		copyWithout: nil)
		collect: [:each | | percent |
			percent _ tally
				ifNotNil: [
					(((tally occurrencesOf: each) * 100.0 / tally size) rounded
						printString padded: #left to: 2 with: $ ), '% '  ]
				ifNil: [ '' ].
			percent, (each browserPrintStringFull: false)
		]! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 4/21/2022 11:33:12'!
printHMSOn: aStream separator: aChar
	"Print just hh:mm:ss"
	aStream
		nextPutAll: (self hour printString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self minute printString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self second printString padded: #left to: 2 with: $0).
! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 4/21/2022 12:17:14'!
printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
	"Print as per ISO 8601 section 5.4.2
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c numericValue > $0 numericValue ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		aStream
			nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
			nextPutAll: (offset hours abs printString padded: #left to: 2 with: $0);
			nextPut: $:;
			nextPutAll: (offset minutes abs printString padded: #left to: 2 with: $0).
		"Note: We should check the ISO to see if the seconds part is supported by the standard
		(usually not an issue. offsets usually don't include seconds)"
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated printString) ]]! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 4/21/2022 12:17:20'!
printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo
	"Print just the year, month, and day on aStream.

	If printLeadingSpaceToo is true, then print as:
		' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative)
	otherwise print as:
		'YYYY-MM-DD' or '-YYYY-MM-DD' "

	| year month day |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ printLeadingSpaceToo ifTrue: [ aStream space ]].
	aStream
		nextPutAll: (year abs printString padded: #left to: 4 with: $0);
		nextPut: $-;
		nextPutAll: (month printString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (day printString padded: #left to: 2 with: $0)
! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 4/21/2022 12:17:00'!
filenamishPrintOn: aStream
	"
	String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]
	"
	
	| year month day monthName |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	monthName _ Month nameOfMonth: month.
	aStream
		nextPutAll: (year abs printString padded: #left to: 4 with: $0);
		nextPutAll: (monthName copyFrom: 1 to: 3);
		nextPutAll: (day printString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (self hour printString padded: #left to: 2 with: $0);
		nextPut: $h;
		nextPutAll: (self minute printString padded: #left to: 2 with: $0);
		nextPut: $m! !


!Date methodsFor: 'printing' stamp: 'jmv 4/21/2022 12:44:30'!
printOn: aStream format: formatArray 
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in printOn: and mmddyy"
	| gregorian twoDigits element monthFormat |
	gregorian _ self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
	twoDigits _ formatArray size > 6 and: [(formatArray at: 7) > 1].
	1 to: 3 do: 
		[ :i | 
			element := formatArray at: i.
			element = 1
				ifTrue: [twoDigits
						ifTrue: [aStream
								nextPutAll: (gregorian first printString
										padded: #left
										to: 2
										with: $0)]
						ifFalse: [gregorian first printOn: aStream]].
			element = 2
				ifTrue: [monthFormat := formatArray at: 5.
					monthFormat = 1
						ifTrue: [twoDigits
								ifTrue: [aStream
										nextPutAll: (gregorian middle printString
												padded: #left
												to: 2
												with: $0)]
								ifFalse: [gregorian middle printOn: aStream]].
					monthFormat = 2
						ifTrue: [aStream
								nextPutAll: ((Month nameOfMonth: gregorian middle)
										copyFrom: 1
										to: 3)].
					monthFormat = 3
						ifTrue: [aStream
								nextPutAll: (Month nameOfMonth: gregorian middle)]].
			element = 3
				ifTrue: [(formatArray at: 6)
							= 1
						ifTrue: [gregorian last printOn: aStream]
						ifFalse: [aStream
								nextPutAll: ((gregorian last \\ 100) printString
										padded: #left
										to: 2
										with: $0)]].
			i < 3
				ifTrue: [(formatArray at: 4)
							~= 0
						ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]]]
! !


!Semaphore methodsFor: 'printing' stamp: 'jmv 4/21/2022 12:23:17'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: ' with ';
		nextPutAll: excessSignals printString;
		space;
		nextPutAll: ('signal' pluralize: excessSignals ~= 1)! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 4/21/2022 12:38:28'!
displayBulletTextLeft: texLeft number: bulletNumber
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber printString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character numericValue: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character numericValue: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 4/21/2022 12:46:41'!
deletePackage

	| current cs |
	current _ model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs _ ChangeSet existingOrNewChangeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash printString, cs name.
		cs isForBaseSystem: true ].
	CodePackage deregister: current! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/21/2022 12:38:35'!
baseSystemNameFor: aChangeSetNumber

	^String streamContents: [ :strm |
	strm
		nextPutAll: (aChangeSetNumber printString padded: #left to: 4 with: $0);
		nextPutAll: '-CuisCore-';
"		nextPutAll: Utilities authorName asCamelCase;"
		nextPutAll: 'AuthorName';
"		nextPutAll: '-';
		nextPutAll: Utilities authorInitials asCamelCase;"
		nextPutAll: '-' ]! !


!SpaceTallyItem methodsFor: 'printing' stamp: 'jmv 4/21/2022 12:48:02'!
printOn: aStream

	analyzedClassName
		ifNotNil: [ aStream nextPutAll: analyzedClassName asString]. 
	aStream nextPutAll: ' ('.
	codeSize
		ifNotNil: [ aStream nextPutAll: 'code size: ' ;  nextPutAll: codeSize printString]. 
	instanceCount
		ifNotNil: [ aStream nextPutAll: ' instance count: ' ;  nextPutAll: instanceCount printString]. 
	spaceForInstances
		ifNotNil: [ aStream nextPutAll: ' space for instances: ' ;  nextPutAll: spaceForInstances printString]. 
	aStream nextPut: $).
	! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 4/21/2022 12:42:11'!
scanPast: rangeType level: level
	"first level adds no suffix to the rangeType.
	Suffix from 1 to 7 added in cycles , ((level-2) mod(7) + 1)"
	| cycle typePlusCycle |
	
	cycle := level <= 1 
		ifTrue: [0]
		ifFalse:[ ((level - 2) \\ 7) + 1].
	typePlusCycle := cycle = 0 
		ifTrue:[rangeType]
		ifFalse:[(rangeType, cycle printString) asSymbol].
	^self scanPast: typePlusCycle
! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'jmv 4/21/2022 12:42:59'!
buildInitialSelectorAnswer: parseNodesToParameterize
	"builds a selector with the shape of #m1 if unary, or #m1: something m2: else if it has args"

	^ parseNodesToParameterize
		ifEmpty: [ self formatAsKeyword: 'm1' ]
		ifNotEmpty: [ parseNodesToParameterize
			inject: ''
			into: [ :partialSelector :parseNode |
				| currentKeyword |
				currentKeyword _ 'm' , (parseNodesToParameterize indexOf: parseNode) printString , ': '.
				partialSelector
				, (self formatAsKeyword: currentKeyword)
				, (self formatAsMethodArgument: parseNode name)
				, String newLineString ] ]! !

