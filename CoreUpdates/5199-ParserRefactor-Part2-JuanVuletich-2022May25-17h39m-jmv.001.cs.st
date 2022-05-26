'From Cuis 6.0 [latest update: #5195] on 25 May 2022 at 5:40:59 pm'!

!Scanner methodsFor: 'expression types' stamp: 'jmv 5/23/2022 17:19:34'!
scanForFindSelectorUpTo: terminator
"
Scanner findSelectorTests
"
    | s |

    s := source collectionSpecies writeStream.
    [tokenType == terminator or: [tokenType == #doIt]] whileFalse: [
        tokenType caseOf: {
            [#leftParenthesis] -> [self scanToken; scanForFindSelectorUpTo: #rightParenthesis].
            [#leftBracket] -> [self scanToken; scanForFindSelectorUpTo: #rightBracket].
            [#leftBrace] -> [self scanToken; scanForFindSelectorUpTo: #rightBrace].
            [#keyword] -> [s nextPutAll: token].
        } otherwise: [].
        self scanToken
    ].
    ^s contents! !

!Scanner methodsFor: 'expression types' stamp: 'jmv 5/23/2022 17:15:22'!
scanLitWord
	"Accumulate keywords and asSymbol the result."

	token := (source collectionSpecies streamContents: [ :stream |
		stream nextPutAll: token.
		[ (self typeTableAt: hereChar) == #xLetter ] whileTrue: [
			self xLetter.
			stream nextPutAll: token ] ]) asSymbol! !

!Scanner methodsFor: 'public access' stamp: 'jmv 5/22/2022 21:18:35'!
scanFieldNames: stringOrArray
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: stringOrArray asString readStream.
	strm := (Array new: 10) writeStream.
	[tokenType == #doIt]
		whileFalse: 
			[tokenType == #word ifTrue: [strm nextPut: token].
			self scanToken].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"! !

!Scanner methodsFor: 'public access' stamp: 'jmv 5/22/2022 20:40:14'!
scanMessageParts: sourceString
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll := OrderedCollection new.
	self scan: sourceString asPlainString readStream.
	nonKeywords := 0.
	[tokenType == #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [token = #| and: [ coll size > 1 ] ])
			ifTrue: [(nonKeywords := nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords := 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll! !

!Scanner methodsFor: 'public access' stamp: 'jmv 5/22/2022 20:40:33'!
scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimitors #( and ) in a 
	Smalltalk literal expression."

	self initScannerForTokenization.
	self scan: textOrString asPlainString readStream.
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"! !

!Scanner methodsFor: 'public access' stamp: 'jmv 5/22/2022 21:17:43'!
typedScanTokens: textOrString 
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings. This methiod accepts _ (underscore) as an assignment token
	 irrespective of whether the system prefers := as the assignment token."
	| s |
	self initScannerForTokenization.
	self scan: textOrString asPlainString readStream.
	s := (Array new: 16) writeStream.
	[tokenType == #doIt] whileFalse:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
			[self scanToken.
			 token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/23/2022 17:16:04'!
xBinary

	tokenType _ #binary.
	token _ source collectionSpecies streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type _ self typeTableAt: hereChar.
		type == #xBinary and: [hereChar ~= $- or: [aheadChar isDigit not]]
		] whileTrue: [
		stream nextPut: self step]].
	token _ token asSymbol! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/23/2022 17:16:31'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
    aStream := source collectionSpecies writeStream.
    self step.
    [hereChar = $"]
        whileFalse:
            [(hereChar = stopChar and: [source atEnd])
                ifTrue: [^self offEnd: self class unmatechedCommentQuoteErrorDescription].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken! !

!Scanner methodsFor: 'initialization' stamp: 'jmv 5/23/2022 17:12:06'!
initScanner

	typeTable := Scanner typeTable.
	isForTokenization := false.
	sentInLiterals := Set new.
	! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 5/22/2022 20:39:45'!
findSelectorIn: aString

    | result |
    result _ Scanner new
        scan: aString readStream;
        scanForFindSelectorUpTo: #notATerminator.
    ^result! !


!Parser methodsFor: 'private' stamp: 'jmv 5/22/2022 20:41:00'!
initPattern: aString notifying: req return: aBlock
	| result |
	self
		init: aString asPlainString readStream
		notifying: req
		failBlock: [^nil].
	encoder _ self.
	result _ aBlock value: (self pattern: false inContext: nil).
	encoder _ failBlock _ nil.  "break cycles"
	^result! !

!Parser methodsFor: 'private' stamp: 'jmv 5/23/2022 17:23:59'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	self transformVerticalBarAndUpArrowIntoABinarySelector.

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #keyword ifTrue: [
		selector := source collectionSpecies writeStream.
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: 'Message pattern'! !

!Parser methodsFor: 'private' stamp: 'jmv 5/22/2022 20:41:16'!
privateReadSelectorFrom: aMethodSource
	"Answer the message selector for the argument, aMethodSource, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	"Note: only intended to read the selector. Parser instance might be inconsistent afterwards.
	Optimized these kind of methods (compare the following):
	[ 100000 timesRepeat: [Parser new parseSelector: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun 4824
	[ 100000 timesRepeat: [Parser new privateReadSelectorFrom: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun  342
	"

	| result |
	self initScannerForTokenization.
	self
		init: aMethodSource asString readStream
		notifying: nil
		failBlock: [ ^nil ].
	encoder _ self.
	result _ self privateReadSelector.
	encoder _ failBlock _ nil.  "break cycles"
	^result! !

!Parser methodsFor: 'expression types' stamp: 'jmv 5/23/2022 17:24:44'!
keywordPattern

	| keywordRanges selector arguments |
	
	selector := source collectionSpecies writeStream.
	arguments := OrderedCollection new.
	keywordRanges := OrderedCollection new.
	
	[hereType == #keyword] whileTrue:[ 
		self addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments ].
		
	^ {selector contents asSymbol. arguments. 3. keywordRanges}
		! !

!Parser methodsFor: 'expression types' stamp: 'jmv 5/23/2022 17:25:10'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: [
			start := self startOfNextToken.
			selector := source collectionSpecies writeStream.
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue: 
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [
			
			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue: 
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].

	parseNode := MessageNode new
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true! !

!Parser methodsFor: 'public access' stamp: 'jmv 5/22/2022 20:41:08'!
parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	
	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		startPosition _ myStream position.
		sourceCode _ myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		"Protect against possible parsing failure"
		doIt ifTrue: [
			(sourceCode beginsWith: Scanner doItInSelector)
				ifTrue: [encoder selector: Scanner doItInSelector]
				ifFalse: [
					(sourceCode beginsWith: Scanner doItSelector)
						ifTrue: [encoder selector: Scanner doItSelector]]].
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ requestor text string readStream ].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !

