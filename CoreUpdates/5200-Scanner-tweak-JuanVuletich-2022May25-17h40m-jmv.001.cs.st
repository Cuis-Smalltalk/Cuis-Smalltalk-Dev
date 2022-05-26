'From Cuis 6.0 [latest update: #5195] on 25 May 2022 at 5:47:49 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/23/2022 19:45:37'!
typeTableAt: aCharacter
	^typeTable at: aCharacter iso8859s15Code ifAbsent:[#xLetter]! !

