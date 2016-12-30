'From Cuis 5.0 of 7 November 2016 [latest update: #3015] on 29 December 2016 at 4:06:32 pm'!

!LiteralNode methodsFor: 'printing' stamp: 'jmv 12/29/2016 16:06:13'!
printOn: aStream indent: level

	key isVariableBinding
		ifTrue: [
			key key isNil
				ifTrue: [
					aStream nextPutAll: '###'; nextPutAll: key value soleInstance name ]
				ifFalse: [
					aStream nextPutAll: '##'; nextPutAll: key key ]]
		ifFalse: [
			key isLiteral
				ifTrue: [ key storeOn: aStream ]
				ifFalse: [
					"Need to generate code for stuff that is in a CompiledMethod literal
					but is not understood as a literal by the Compiler.
					Well, then it is because it was generated using backticks!!"
					aStream nextPut: $`.
					key storeOn: aStream.
					aStream nextPut: $`.
					]
			]! !

