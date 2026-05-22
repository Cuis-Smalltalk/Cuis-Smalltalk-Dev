"
I represent a special case of interval, I refer to source code intervals. There are two main users of me:

* The debugger (to display which is the current piece of code being debugged)
* Refactorings (to select code for refactoring, validating against other intervals and rewriting code)
"
Class {
	#name : 'SourceCodeInterval',
	#superclass : 'Interval',
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'error messages' }
SourceCodeInterval class >> canNotExpandIncompleteStatementError [

	^ 'Can not expand an incomplete statement'.
]

{ #category : 'converting' }
SourceCodeInterval >> asSourceCodeInterval [

	^ self
]

{ #category : 'private - source code' }
SourceCodeInterval >> canBeExpandedStartingWith: firstChar endingWith: lastChar [

	^ (firstChar == $( and: [ lastChar == $) ]) or: [ firstChar == $` and: [ lastChar == $` ] ]
]

{ #category : 'private - source code' }
SourceCodeInterval >> canBeTrimmed: initialChar [

	^ initialChar notNil and: [ initialChar isSeparator or: [ initialChar == $. ] ]
]

{ #category : 'source code' }
SourceCodeInterval >> codeAfterIntervalOn: aSourceCode [

	^ aSourceCode copyFrom: (self last + 1) to: aSourceCode size
]

{ #category : 'testing' }
SourceCodeInterval >> endsOnDotOn: aSourceCode [

	^(self lastCharacterOfRangeOn: aSourceCode) == $.
]

{ #category : 'source code' }
SourceCodeInterval >> expandToMatchExpressionOn: aSourceCode [
	"takes a source code and if the source range references an expression
	that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	| firstCharAndIndex lastCharAndIndex |
	 
	firstCharAndIndex := self skipSeparatorsFrom: start by: -1 in: aSourceCode.
	lastCharAndIndex := self skipSeparatorsFrom: stop by: 1 in: aSourceCode.
	
	^ (self canBeExpandedStartingWith: firstCharAndIndex key endingWith: lastCharAndIndex key)
		ifTrue: [ (self class from: firstCharAndIndex value to: lastCharAndIndex value) expandToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]
]

{ #category : 'source code' }
SourceCodeInterval >> expandUntilNextNonBlankCharacterOn: aSourceCode [

	^SourceCodeInterval from: self first to: (self last +
		(aSourceCode copyFrom: (self last + 1) to: (aSourceCode size)) firstNonSeparator - 1)
	
]

{ #category : 'source code' }
SourceCodeInterval >> expandUntilStartOfNextStatementOn: aSourceCode [

	"Expands a source code interval containing a valid expression until the next statement. If there isn't one, the interval is not modified.
	Examples:
	- The interval *a := 1 + 2.*   b = 3 + 4.	 is expanded to *a := 1 + 2.   *b = 3 + 4.
	- The interval *a := 1 + 2.*  '
		 b = 3 + 4.	 is expanded to
		*a := 1 + 2.
		   *b = 3 + 4.
	- The interval *| a b |*
		a : =1. is expanded to
		*| a b |
		*a := 1.

	TODO: improve to take into account that there could be a comment in the middle of the statements or
	 after the last statement - Fernando"

	((self last >= aSourceCode size)
		or:[ (self isEndOfLastStatementOn: aSourceCode)
		or:[self isLastStatementOfBlockOn: aSourceCode]]) ifTrue: [^self class from: self first to: self last].

	((self isEndOfTemporariesDeclarationOn: aSourceCode) or: [self endsOnDotOn: aSourceCode ])
		ifTrue: [^self expandUntilNextNonBlankCharacterOn: aSourceCode ].

	(self nextNonBlankCharacterIsDotOn: aSourceCode)
		ifTrue: [^(self expandUntilStatementEndOn: aSourceCode) expandUntilStartOfNextStatementOn: aSourceCode ].

	self error: self class canNotExpandIncompleteStatementError.
	
]

{ #category : 'source code' }
SourceCodeInterval >> expandUntilStatementEndOn: aSourceCode [

	| nextDotIdx |
	
	nextDotIdx := aSourceCode findDelimiters: '.' startingAt: self last.
	^SourceCodeInterval
		from: self first
		to: (nextDotIdx min: aSourceCode size)
	
]

{ #category : 'testing' }
SourceCodeInterval >> intervalEndsOnEndOfStatementIn: aSourceCode [

	^(self endsOnDotOn: aSourceCode)
		or: [ (self nextNonBlankCharacterIsDotOn: aSourceCode)
	 	or: [self isEndOfTemporariesDeclarationOn: aSourceCode ]].
]

{ #category : 'testing' }
SourceCodeInterval >> isEndOfLastStatementOn: aSourceCode [

	^((self lastCharacterOfRangeOn: aSourceCode) isSeparator not)
		and: [(self codeAfterIntervalOn: aSourceCode) firstNonSeparator = 0].
	
]

{ #category : 'testing' }
SourceCodeInterval >> isEndOfTemporariesDeclarationOn: aSourceCode [

	^((self lastCharacterOfRangeOn: aSourceCode) == $|) and: [^(aSourceCode indexOf: $|) < self last]
]

{ #category : 'testing' }
SourceCodeInterval >> isLastStatementOfBlockOn: aSourceCode [

	^self nextNonBlankCharacterIs: $] on: aSourceCode.
]

{ #category : 'source code' }
SourceCodeInterval >> lastCharacterOfRangeOn: aSourceCode [

	^aSourceCode at: self last
]

{ #category : 'source code' }
SourceCodeInterval >> nextNonBlankCharacterIs: aCharacter on: aSourceCode [

	| codeAfterInterval firstNonBlankIndex |

	codeAfterInterval := self codeAfterIntervalOn: aSourceCode.

	firstNonBlankIndex := codeAfterInterval firstNonSeparator.

	^firstNonBlankIndex ~= 0 and: [(codeAfterInterval at: firstNonBlankIndex) = aCharacter]

]

{ #category : 'source code' }
SourceCodeInterval >> nextNonBlankCharacterIsDotOn: aSourceCode [

	^self nextNonBlankCharacterIs: $. on: aSourceCode.
	
]

{ #category : 'source code' }
SourceCodeInterval >> skipSeparatorsFrom: aStartIndex by: aStep in: aSourceCode [
	
	| char index |
	
	index := aStartIndex + aStep.
	[ char := aSourceCode at: index ifAbsent: [ Character null ].
	char isSeparator ] whileTrue: [ index := index + aStep ].
	
	^char -> index
]

{ #category : 'private - source code' }
SourceCodeInterval >> trimLeft: startOffset right: endOffset [

	^ SourceCodeInterval from: start + startOffset to: stop - endOffset
]

{ #category : 'source code' }
SourceCodeInterval >> trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode [

	| startOffset endOffset |

	startOffset := self trimmedOffsetFor: initialChar.
	endOffset := self trimmedOffsetFor: endingChar.
	^(startOffset = 1 or: [ endOffset = 1])
		ifTrue: [ (self trimLeft: startOffset right: endOffset) trimToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]
]

{ #category : 'source code' }
SourceCodeInterval >> trimParentesisOn: aSourceCode [
			
	| parentesis |
	
	parentesis := 1.
	start+1 to: stop-1 do: [ :index | | char |
		char := aSourceCode at: index.
		char == $( 
			ifTrue: [ parentesis := parentesis + 1]
			ifFalse: [ char == $) 
				ifTrue: [ 
					parentesis = 1 ifTrue: [ ^self ].
					parentesis := parentesis - 1]]].
		
	^(self trimLeft: 1 right: 1) trimToMatchExpressionOn: aSourceCode 
]

{ #category : 'source code' }
SourceCodeInterval >> trimToMatchExpressionOn: aSourceCode [

	| initialChar endingChar |
	
	initialChar := aSourceCode at: start ifAbsent: nil.
	endingChar := aSourceCode at: stop ifAbsent: nil.
	
	^(initialChar == $( and: [ endingChar == $) ])
		ifTrue: [ self trimParentesisOn: aSourceCode ]
		ifFalse: [ self trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode]
]

{ #category : 'source code' }
SourceCodeInterval >> trimmedOffsetFor: aChar [
			
	^(self canBeTrimmed: aChar) ifTrue: [ 1 ] ifFalse: [ 0 ]
]
