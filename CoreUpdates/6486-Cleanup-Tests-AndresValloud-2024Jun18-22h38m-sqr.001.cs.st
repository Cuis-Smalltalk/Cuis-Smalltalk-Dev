'From Cuis7.1 [latest update: #6452] on 18 June 2024 at 11:05:43 pm'!

!TestCase class methodsFor: 'accessing' stamp: 'sqr 6/18/2024 22:55:03'!
sortedTestSelectorsFrom: aCollection

	^(aCollection asArray select: [:each | self isTestSelector: each]) sort! !

!TestCase class methodsFor: 'testing' stamp: 'sqr 6/18/2024 22:50:05'!
isTestMethod: aCompiledMethod

	^(self isTestSelector: aCompiledMethod selector)
		and: [aCompiledMethod methodClass inheritsFrom: TestCase]! !

!TestCase class methodsFor: 'testing' stamp: 'sqr 6/18/2024 22:52:12'!
isTestSelector: aSelector

	^aSelector beginsWith: self testSelectorPrefix
		:: and: [aSelector numArgs isZero]! !

!TestCase class methodsFor: 'testing' stamp: 'sqr 6/18/2024 22:48:31'!
testSelectorPrefix

	^'test'! !


!CompiledMethod methodsFor: 'testing' stamp: 'sqr 6/18/2024 22:48:53'!
isTestMethod

    ^TestCase isTestMethod: self! !


!TestCase class methodsFor: 'accessing' stamp: 'sqr 6/18/2024 22:55:16'!
allTestSelectors

	^self sortedTestSelectorsFrom: self allSelectors! !

!TestCase class methodsFor: 'accessing' stamp: 'sqr 6/18/2024 22:55:25'!
testSelectors

	^self sortedTestSelectorsFrom: self selectors! !

!methodRemoval: UnicodeSymbol #testSelectorPrefix stamp: 'sqr 6/18/2024 22:52:40'!
UnicodeSymbol removeSelector: #testSelectorPrefix!
!methodRemoval: UnicodeSymbol #isTestSelector stamp: 'sqr 6/18/2024 22:52:01'!
UnicodeSymbol removeSelector: #isTestSelector!
!methodRemoval: Symbol #testSelectorPrefix stamp: 'sqr 6/18/2024 22:52:37'!
Symbol removeSelector: #testSelectorPrefix!
!methodRemoval: Symbol #isTestSelector stamp: 'sqr 6/18/2024 22:51:24'!
Symbol removeSelector: #isTestSelector!
