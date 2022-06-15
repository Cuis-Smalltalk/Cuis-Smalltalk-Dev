'From Cuis 6.0 [latest update: #5276] on 13 June 2022 at 10:32:07 am'!

!Utf8Symbol methodsFor: 'copying' stamp: 'jmv 6/13/2022 10:25:41'!
shallowCopy
	"Answer with me, because Symbols are unique."! !

!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:30:50'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs _ self numArgs) >= n ifTrue: [ ^self ].	
	^ (Utf8String streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs _ 0 ] ifFalse: [offs _ 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:28:04'!
isInfix
	"Answer whether the receiver is an infix message selector."

	^ self precedence = 2! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:26:58'!
isUnary
	"Answer whether the receiver is an unary message selector."

	^ self precedence = 1! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:27:53'!
isValidBinarySelector

	^ self isInfix and: [ self allSatisfy: [ :character | character isValidInBinarySelectors ] ]! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:28:38'!
isValidKeywordSelector

	^ self isKeyword and: [ self keywords allSatisfy: [ :keywordString | keywordString allButLast asSymbol isValidSelector ] ]! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:27:23'!
isValidSelector

	^ self isValidUnarySelector
		or: [ self isValidBinarySelector ]
		or: [ self isValidKeywordSelector ]! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:27:32'!
isValidUnarySelector

	^ self isUnary and: [ self allSatisfy: [ :character | character isValidInIdentifiers ] ]! !


!Symbol methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:30:31'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs _ self numArgs) >= n ifTrue: [ ^self ].	
	^ (String streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs _ 0 ] ifFalse: [offs _ 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol! !


!Symbol reorganize!
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('system primitives' flushCache)
('private' errorNoModification species string:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
!


!Utf8Symbol reorganize!
('accessing' precedence)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('testing' isInfix isInitializePvtSelector isKeyword isPvtSelector isSymbol isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector)
('printing' print storeOn:)
('uCompletion' separateKeywords)
('private' species)
!

