'From Cuis 6.0 [latest update: #5275] on 13 June 2022 at 10:04:59 am'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 10:01:22'!
asCamelCase
	"Answer a new String, without any whitespace, and with words capitalized (Except for the first one)
	' how do you do? ' asCamelCase
	' how do you do? ' asUtf8String asCamelCase
	"
	^ self species streamContents: [ :outStream | | inStream capitalize wroteSome |
		wroteSome _ false.
		capitalize _ false.
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [ | c |
			c _ inStream next.
			c isSeparator
				ifTrue: [ capitalize _ true ]
				ifFalse: [
					capitalize & wroteSome ifTrue: [ c _ c asUppercase ].
					outStream nextPut: c.
					wroteSome _ true.
					capitalize _ false ]]]! !

