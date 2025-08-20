'From Cuis7.5 [latest update: #7427] on 19 August 2025 at 9:15:21 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 8/19/2025 21:13:41'!
doitPatternInContext: context

	self selector: self class doItSelector.
	^context
		ifNil: [{self class doItSelector. {}. 1. nil }]
		ifNotNil: [{self class doItInSelector. {encoder encodeVariable: Encoder doItInContextName}. 3. nil}]! !

