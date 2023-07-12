'From Cuis 6.0 [latest update: #5898] on 6 July 2023 at 11:47:39 am'!

!String methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:10:37'!
asByteStringIfAscii
	"Answer an instance of the Byte oriented String class"
	^self! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 7/6/2023 11:11:28'!
asByteStringIfAscii
	"Answer an instance of the Byte oriented String class"

	self isAscii ifTrue: [
		^self asByteString ].
	^self! !

