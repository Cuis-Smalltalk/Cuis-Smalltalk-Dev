'From Cuis 6.0 [latest update: #5941] on 21 July 2023 at 5:02:37 pm'!

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 7/21/2023 16:55:51'!
trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode

	| startOffset endOffset |

	startOffset := self trimmedOffsetFor: initialChar.
	endOffset := self trimmedOffsetFor: endingChar..
	^(startOffset = 1 or: [ endOffset = 1])
		ifTrue: [ (self trimLeft: startOffset right: endOffset) trimToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 7/21/2023 16:59:57'!
trimParentesisOn: aSourceCode
			
	| parentesis |
	
	parentesis := 1.
	start+1 to: stop-1 do: [ :index | | char |
		char := aSourceCode at: index.
		char = $( 
			ifTrue: [ parentesis := parentesis + 1]
			ifFalse: [ char = $) 
				ifTrue: [ 
					parentesis = 1 ifTrue: [ ^self ].
					parentesis := parentesis - 1]]].
		
	^(self trimLeft: 1 right: 1) trimToMatchExpressionOn: aSourceCode ! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 7/21/2023 16:47:45'!
trimmedOffsetFor: aChar
			
	^(self canBeTrimmed: aChar) ifTrue: [ 1 ] ifFalse: [ 0 ]! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 7/21/2023 16:53:59'!
trimToMatchExpressionOn: aSourceCode

	| initialChar endingChar |
	
	initialChar := aSourceCode at: start ifAbsent: [ nil ].
	endingChar := aSourceCode at: stop ifAbsent: [ nil ].
	
	^(initialChar = $( and: [ endingChar = $) ])
		ifTrue: [ self trimParentesisOn: aSourceCode ]
		ifFalse: [ self trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'HAW 7/21/2023 12:57:55'!
trimLeft: startOffset right: endOffset

	^ SourceCodeInterval from: start + startOffset to: stop - endOffset! !

