'From Cuis7.9 [latest update: #8094] on 28 July 2026 at 3:03:11 pm'!

!Pragma methodsFor: 'testing' stamp: 'jmv 28/Jul/2026, 6:02:28 pm (UTC)'!
isPrimitivePragma
	^Parser primitivePragmaSelectors includes: keyword
! !


!MethodNode methodsFor: 'printing' stamp: 'jmv 28/Jul/2026, 6:03:02 pm (UTC)'!
printPragmasOn: aStream
	properties ifNil: [^self].
	properties pragmas do: [ :pragma |
		"Primitives are printed in printPrimitiveOn:; skip them."
		pragma isPrimitivePragma ifFalse:
			[aStream newLineTab: 1.
			 pragma printOn: aStream]].
! !

