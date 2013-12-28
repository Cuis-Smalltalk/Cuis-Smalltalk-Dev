'From Cuis 4.2 of 25 July 2013 [latest update: #1898] on 23 December 2013 at 9:35:05.209197 am'!

!WriteStream methodsFor: 'character writing' stamp: 'jmv 12/23/2013 09:34'!
cr
	"Append a cr character to the receiver.
	Use this method when you specifically need a cr character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character cr! !

!WriteStream methodsFor: 'character writing' stamp: 'jmv 12/23/2013 09:34'!
lf
	"Append a lf character to the receiver.
	Use this method when you specifically need a lf character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character lf! !

