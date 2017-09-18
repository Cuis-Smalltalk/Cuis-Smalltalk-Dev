'From Cuis 5.0 of 7 November 2016 [latest update: #3180] on 17 September 2017 at 9:22:40 pm'!
!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepthsStartIndexes blockDepths '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 9/13/2017 22:00:57'!
blockDepths
	^blockDepths! !

!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 9/13/2017 22:01:05'!
blockDepthsStartIndexes
	^blockDepthsStartIndexes! !

!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 9/13/2017 20:51:24'!
ranges
	^ ranges! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 9/17/2017 18:29:22'!
parseSetWorkspace: aBoolean
	"Answer a collection of SHRanges by parsing aText.
	When formatting it is not necessary to set the workspace, and this can make the parse take less time, so aBoolean specifies whether the parser should be given the workspace"

	parser ifNil: [parser _ SHParserST80 new].
	parser 
		workspace: (aBoolean ifTrue: [workspace]);
		classOrMetaClass: classOrMetaClass;
		source: formattedText asString.
	parser parse! !


!SHParserST80 methodsFor: 'parse support' stamp: 'jmv 9/17/2017 19:04:26'!
enterBlock
	blockDepth _ blockDepth + 1.
	bracketDepth _ bracketDepth + 1.
	blockDepths add: blockDepth.
	blockDepthsStartIndexes add: sourcePosition-1! !

!SHParserST80 methodsFor: 'parse support' stamp: 'jmv 9/17/2017 19:02:56'!
leaveBlock
	arguments removeKey: blockDepth ifAbsent: nil.
	temporaries removeKey: blockDepth ifAbsent: nil.
	blockDepth _ blockDepth - 1.
	bracketDepth _ bracketDepth - 1.
	blockDepths add: blockDepth.
	blockDepthsStartIndexes add: sourcePosition! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/17/2017 19:03:23'!
parse: isAMethod 
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	self initializeInstanceVariables.
	sourcePosition _ 1.
	arguments _ Dictionary new.
	temporaries _ Dictionary new.
	blockDepth _ bracketDepth := 0.
	blockDepths _ OrderedCollection with: blockDepth.
	blockDepthsStartIndexes _ OrderedCollection with: sourcePosition.
	ranges ifNil: [ ranges := OrderedCollection new: 100] ifNotNil: [ ranges reset].
	errorBlock _ [^false].
	[
		self scanNext.
		isAMethod 
			ifTrue: [
				self parseMessagePattern.
				self parsePragmaSequence].
		self parseMethodTemporaries.
		isAMethod ifTrue: [self parsePragmaSequence].
		self parseStatementList.
		currentToken ifNotNil: [self error]
	] ensure: [errorBlock _ nil].
	^true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/17/2017 19:05:25'!
parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self leaveBlock.
	self scanPast: #backtick! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/17/2017 18:40:55'!
parseBlock
	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $: ifTrue: [self parseBlockArguments].
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].
	self leaveBlock.
	self scanPast: #blockEnd level: bracketDepth.! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 9/17/2017 21:20:07'!
privateStyle

	| alpha end start count startIndexes |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	Preferences highlightBlockNesting ifTrue: [
		startIndexes _ parser blockDepthsStartIndexes.
		count _ startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start _ startIndexes at: idx.
			end _ idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha _ depth / 16.0 min: 0.5.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor 
				color: (Theme current text alpha: alpha) ) from: start to: end ]]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 9/17/2017 18:30:50'!
replaceStringForRangesWithType: aSymbol with: aString
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"
	| toReplace increaseInLength |

	"We don't handle format and conversion for debuggers"
	disableFormatAndConvert ifTrue: [ ^self ].

	self parseSetWorkspace: false.
	toReplace _ parser ranges select: [ :each |
		each rangeType = aSymbol ].
	toReplace isEmpty ifTrue: [ ^self ].
	increaseInLength := 0.

	(toReplace asArray sort: [ :a :b | a start <= b start ]) 
		do: [ :each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			formattedText replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]! !

!methodRemoval: SHTextStylerST80 #rangesSetWorkspace:!
SHTextStylerST80 removeSelector: #rangesSetWorkspace:!
!methodRemoval: SHParserST80 #rangesIn:classOrMetaClass:workspace:!
SHParserST80 removeSelector: #rangesIn:classOrMetaClass:workspace:!
!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepths blockDepthsStartIndexes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
