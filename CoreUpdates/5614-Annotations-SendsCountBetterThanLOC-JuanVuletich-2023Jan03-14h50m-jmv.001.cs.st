'From Cuis 6.0 [latest update: #5611] on 3 January 2023 at 2:59:58 pm'!

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:12:53'!
messageSendsCount
"
Object messageSendsCount
"
	"Number of message sends in code
	If asked to a class, also include its metaclass (i.e. the class side).
	If asked to a metaclass (the class side), don't include the class (the instance side)."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) messageSendsCount ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class messageSendsCount].
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) sum: [:c |
	(SystemOrganization superclassOrderIn: c) sum: [:cl | cl messageSendsCount]]
"
"
Smalltalk allClasses sum: [:cl | cl messageSendsCount]
"! !


!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:10:46'!
messageSendsCountOf: category
"
SystemOrganization messageSendsCountOf: #'System-Files'

See also #linesOfCodeOf:
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl messageSendsCount ] ifEmpty: 0.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:14:55'!
messageSendsCount
	"Number of message sends in code.
	A measure of the complexity of code. Alternative to #linesOfCode"

	| count |
	count _ 0.
	self decompile nodesDo: [ :node | node isMessageNode ifTrue: [count _ count + 1] ].
	^count! !


!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:15:17'!
messageSendsCount
	"See comment at CompiledMethod >> #messageSendsCount"

	^self methods inject: 0 into: [ :sum :each |
		sum + each compiledMethod messageSendsCount ].! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 1/2/2023 17:29:52'!
initProgramming
	#(programming) with: #(prefProgramming) do: [ :category :selector |
		(self perform: selector) do: [ :aPrefArray |
			self installDefault: aPrefArray in: category ] ].! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 1/2/2023 17:07:15'!
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
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass theNonMetaClass messageSendsCount);
						nextPutAll: ' total message sends' ].
			}]].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 1/2/2023 17:21:47'!
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
					[#messageSendsCount] -> [
						strm
							print: ((aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm | cm messageSendsCount]);
							nextPutAll: ' message sends' ].
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

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 1/2/2023 17:10:08'!
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
						print: (SystemOrganization fullListAtCategoryNamed: aCategory) size;
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
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (SystemOrganization messageSendsCountOf: aCategory);
						nextPutAll: ' total message sends' ].
			}]].! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 1/3/2023 14:55:07'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count _ 0.
		selection classesDo: [ :cls | count _ count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / (methods asFloat max: 1.0) printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method)'.
		strm nextPutAll: '. Total message sends: '.
		linesOfCode := selection messageSendsCount.
		linesOfCode printOn: strm. ]! !


!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:10:40'!
linesOfCodeOf: category
"
SystemOrganization linesOfCodeOf: #'System-Files'

See also #messageSendsCountOf:
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl linesOfCode ] ifEmpty: 0.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 1/2/2023 17:15:01'!
linesOfCode
	"An approximate measure of lines of code.
	Use decompiled source code. In this way, the measure:
	- Doesn't include comments
	- Doesn't include blank lines
	- Is not sensitive to code formatting
	The motivation is to consider LOC as an expense, not an asset. Minimizing LOC is good.
	But it is not like that for comments!!
	See #messageSendsCount"

	| lines |
	lines _ 0.
	self decompileString lineIndicesDo: [ :start :endWithoutDelimiters :end |
		endWithoutDelimiters - start > 0 ifTrue: [
			lines _ lines+1 ]].
	^lines! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 1/3/2023 14:57:37'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(allowNonBooleanReceiversOfBooleanMessages false )
		#(alternativeBrowseIt false )
		#(backgroundColorFillsAllBackground true )
		#(browseWithPrettyPrint false )
		#(debugHaloHandle true )
		#(debugLogTimestamp true )
		#(debugShowDamage false )
		#(decorateBrowserButtons true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(extraDebuggerButtons true )
		#(fullPrintItInWorkspaces false )
		#(highlightBlockNesting true )
		#(italicsInShout true)
		#(leftArrowAssignmentsInGeneratedCode false )
		#(listClassesHierarchically true )
		#(logDebuggerStackToFile false )
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(classAnnotations #(instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(methodAnnotations #(timeStamp "linesOfCode" messageSendsCount messageCategory implementorsCount sendersCount  packages changeSets) )	
		#(syntaxHighlightingAsYouType true )
		#(syntaxHighlightingAsYouTypeAnsiAssignment false )
		#(syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet initProgramming!

