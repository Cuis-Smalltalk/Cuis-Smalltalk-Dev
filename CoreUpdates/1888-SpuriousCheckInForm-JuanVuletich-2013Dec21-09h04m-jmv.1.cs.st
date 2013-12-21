'From Cuis 4.2 of 25 July 2013 [latest update: #1886] on 21 December 2013 at 9:05:17.428266 am'!

!Form methodsFor: 'private' stamp: 'jmv 12/20/2013 00:04'!
setExtent: extent depth: bitsPerPixel bits: bitmap
	"Create a virtual bit map with the given extent and bitsPerPixel."
	width _ extent x asInteger.
	width < 0 ifTrue: [ width _ 0 ].
	height _ extent y asInteger.
	height < 0 ifTrue: [ height _ 0 ].
	depth _ bitsPerPixel.
	bits _ bitmap! !

