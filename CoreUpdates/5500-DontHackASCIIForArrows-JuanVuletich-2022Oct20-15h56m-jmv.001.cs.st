'From Cuis 6.0 [latest update: #5496] on 20 October 2022 at 4:23:30 pm'!

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/20/2022 16:09:07'!
assignmentArrowString
	^ UnicodeContentsHandling = #useUtf8Editors
		ifTrue: [ `(UnicodeCodePoint codePoint: 16r0021D0) asString` ]
		ifFalse: [ `(Character numericValue: 28) asString` ]! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 10/20/2022 16:15:11'!
compile: aCode notifying: requestor 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| code methodAndNode |
	code _ aCode copyReplaceAll: TextModel assignmentArrowString with: '_'.
	methodAndNode _ self
		basicCompile: code
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	methodAndNode method putSource: code fromParseNode: methodAndNode node inFile: 2
			withPreamble: [ :f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: methodAndNode selector withMethod: methodAndNode method.
	^ methodAndNode selector! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 10/20/2022 16:15:24'!
compile: aCode classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	| code methodAndNode selector isExistingMethod |
	code _ aCode asPlainString copyReplaceAll: TextModel assignmentArrowString with: '_'.
	methodAndNode _ self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		self logMethodSource: code forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 10/20/2022 16:13:29'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil overridenMethodOrNil |
	
	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.
	overridenMethodOrNil := self superclass ifNotNil: [ :aSuperclass | 
		aSuperclass lookupSelector: aCompiledMethodWithNode selector ].
	
	aCompiledMethodWithNode method 
		putSource: code
		fromParseNode: aCompiledMethodWithNode node
		class: self 
		category: category 
		withStamp: changeStamp 
		inFile: 2 
		priorMethod: priorMethodOrNil 
		overridesMethod: overridenMethodOrNil.! !


!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 10/20/2022 16:10:14'!
isAssignment
	^ currentToken = ':=' or: [ currentToken = '_' or: [ currentToken = TextModel assignmentArrowString ]]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 10/20/2022 16:22:29'!
privateFormatAndConvert

	"Turn := into an arrow, only if requested"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self replaceStringForRangesWithType: #ansiAssignment with: TextModel assignmentArrowString ].

	"Turn $_ either into an arrow or into := "
	(Preferences at: #syntaxHighlightingAsYouTypeAnsiAssignment)
		ifTrue: [ self replaceStringForRangesWithType: #assignment with: ':=' ]
		ifFalse: [ self replaceStringForRangesWithType: #assignment with: TextModel assignmentArrowString ]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 10/20/2022 16:23:03'!
privateFormatAndConvertWorkspace
	"Turn := into an arrow, only if requested"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self replaceStringForRangesWithType: #ansiAssignment with: TextModel assignmentArrowString, ' ' ].

	"Turn $_ either into an arrow"
	self replaceStringForRangesWithType: #assignment with: TextModel assignmentArrowString.! !

!methodRemoval: SHTextStylerST80 #convertAssignmentsToLeftArrow stamp: 'jmv 10/20/2022 16:20:57'!
SHTextStylerST80 removeSelector: #convertAssignmentsToLeftArrow!
!methodRemoval: SHTextStylerST80 #convertAssignmentsToAnsi stamp: 'jmv 10/20/2022 16:20:40'!
SHTextStylerST80 removeSelector: #convertAssignmentsToAnsi!
!methodRemoval: SHTextStylerST80 #convertAssignmentsToLeftArrowAndSpace stamp: 'jmv 10/20/2022 16:21:21'!
SHTextStylerST80 removeSelector: #convertAssignmentsToLeftArrowAndSpace!
