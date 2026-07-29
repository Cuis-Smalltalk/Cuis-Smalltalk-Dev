'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:38:30 pm'!

!ReturnNode methodsFor: 'printing' stamp: 'JEC 4/Jan/2026, 1:46:12 am (UTC)'!
printExpressionOn: aStream indent: level

	| expressionString shouldAddParenthesis |

	expressionString := String 
		streamContents: [ :expressionStream | expr printOn: expressionStream indent: level ].
		
	shouldAddParenthesis := expr isLiteralNode not 
		and: [ expr isBlockNode not
		and: [ expr isBraceNode not
		and: [ expr isVariableNode not ] ] ].

	shouldAddParenthesis
		ifTrue: [
			"This is because the MessageNode always print it with new lines."
			aStream
				nextPut: $( ;
				nextPutAll: expressionString withBlanksCondensed;
				nextPut: $) ]
		ifFalse: [
			"This is because the MessageNode always print it with new lines."
			aStream nextPutAll: expressionString withBlanksCondensed ].
! !

!methodRemoval: Transcript class #nextPut:when: stamp: 'jmv 29/Jul/2026, 5:38:23 pm (UTC)'!
Transcript class removeSelector: #nextPut:when:!
!methodRemoval: Stream #nextPut:when: stamp: 'jmv 29/Jul/2026, 5:38:23 pm (UTC)'!
Stream removeSelector: #nextPut:when:!
