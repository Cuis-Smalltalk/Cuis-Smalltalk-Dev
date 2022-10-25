'From Cuis 6.0 [latest update: #5508] on 25 October 2022 at 4:34:43 pm'!

!Symbol methodsFor: 'testing' stamp: 'HAW 10/25/2022 16:33:30'!
testSelectorPrefix

	^self class testSelectorPrefix ! !


!Symbol class methodsFor: 'testing' stamp: 'HAW 10/25/2022 16:33:30'!
testSelectorPrefix

	^ 'test'! !


!Symbol methodsFor: 'testing' stamp: 'HAW 10/25/2022 16:33:30'!
isTestSelector
	
	^(self beginsWith: self testSelectorPrefix) and: [ self numArgs isZero ]! !

!methodRemoval: Symbol class #testSelectorSufix stamp: 'HAW 10/25/2022 16:33:30'!
Symbol class removeSelector: #testSelectorSufix!
!methodRemoval: Symbol #testSelectorSufix stamp: 'HAW 10/25/2022 16:33:30'!
Symbol removeSelector: #testSelectorSufix!

!Symbol reorganize!
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('system primitives' flushCache)
('private' errorNoModification species string:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isTestSelector isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector testSelectorPrefix)
('uCompletion' separateKeywords)
!

