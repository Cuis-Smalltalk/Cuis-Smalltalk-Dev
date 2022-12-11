'From Cuis 6.0 [latest update: #5588] on 11 December 2022 at 7:00:32 pm'!

!Object methodsFor: 'error handling' stamp: 'jmv 12/11/2022 13:05:30'!
deny: aBoolean

	self assert: aBoolean not! !


!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:00:00'!
asNFC
	^self! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 18:59:58'!
asNFD
	^self! !


!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 12:24:15'!
asNFC
	"Convenience"
	^self cannonicalComposition! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 12:24:11'!
asNFD
	"Convenience"
	^self cannonicalDecomposition! !


!WordArray methodsFor: 'printing' stamp: 'jmv 12/11/2022 18:01:58'!
printElementsOn: aStream
	"Print my elements on aStream."

	aStream nextPut: $(.
	self do: [ :element | aStream print: (element printStringBase: 16 length: 8 padded: true) ] separatedBy: [ aStream space ].
	aStream nextPut: $)! !

