'From Cuis 6.0 [latest update: #5259] on 10 June 2022 at 3:17:14 pm'!

!Utf32String methodsFor: 'testing' stamp: 'jmv 6/10/2022 15:08:28'!
isAscii
	"Return true when I am pure 7-bit ASCII."

	^ codePoints max < 128! !

