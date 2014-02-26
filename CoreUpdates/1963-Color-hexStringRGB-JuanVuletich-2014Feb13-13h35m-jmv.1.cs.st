'From Cuis 4.2 of 25 July 2013 [latest update: #1962] on 13 February 2014 at 1:53:02.674899 pm'!

!Color methodsFor: 'printing' stamp: 'jmv 2/13/2014 13:41'!
hexStringRGB
	"
	Color fromUser hexStringRGB
	"
	^String streamContents: [ :strm |
		(self red * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self green * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self blue * 255) rounded printOn: strm base: 16 length: 2 padded: true ]! !

