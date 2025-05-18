'From Cuis7.3 [latest update: #7171] on 18 May 2025 at 11:19:48 am'!

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 5/18/2025 11:19:21'!
expandToMatchExpressionOn: aSourceCode
	"takes a source code and if the source range references an expression
	that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	| firstCharAndIndex lastCharAndIndex |
	 
	firstCharAndIndex := self skipSeparatorsFrom: start by: -1 in: aSourceCode.
	lastCharAndIndex := self skipSeparatorsFrom: stop by: 1 in: aSourceCode.
	
	^ (self canBeExpandedStartingWith: firstCharAndIndex key endingWith: lastCharAndIndex key)
		ifTrue: [ (self class from: firstCharAndIndex value to: lastCharAndIndex value) expandToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]! !

