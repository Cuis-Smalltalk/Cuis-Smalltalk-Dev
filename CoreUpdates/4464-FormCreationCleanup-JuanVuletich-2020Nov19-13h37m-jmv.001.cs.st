'From Cuis 5.0 [latest update: #4463] on 19 November 2020 at 1:56:46 pm'!

!Form methodsFor: 'private' stamp: 'jmv 11/19/2020 13:55:50'!
initFromArray: array
	"Fill the bitmap from array.  If the array is shorter,
	then cycle around in its contents until the bitmap is filled."
	| ax aSize |
	ax _ 0.
	aSize _ array size.
	1 to: bits size do: [ :index |
		(ax _ ax + 1) > aSize ifTrue: [ax _ 1].
		bits at: index put: (array at: ax)]! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 11/19/2020 12:57:50'!
extent: extentPoint depth: bitsPerPixel fromArray: anArray offset: offsetPoint 
	"Answer an instance of me with a pixmap of the given depth initialized from anArray."
	"See Form >> storeOn:base:"

	^ (self extent: extentPoint depth: bitsPerPixel)
		offset: offsetPoint;
		initFromArray: anArray
! !

!methodRemoval: Cursor class #extent:fromArray:offset: stamp: 'jmv 11/19/2020 13:37:12'!
Cursor class removeSelector: #extent:fromArray:offset:!
!methodRemoval: Form class #extent:fromArray:offset: stamp: 'jmv 11/19/2020 13:54:04'!
Form class removeSelector: #extent:fromArray:offset:!
