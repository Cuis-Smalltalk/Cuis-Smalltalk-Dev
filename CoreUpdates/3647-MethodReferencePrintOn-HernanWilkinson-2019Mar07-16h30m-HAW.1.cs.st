'From Cuis 5.0 of 7 November 2016 [latest update: #3645] on 7 March 2019 at 4:44:03 pm'!

!MethodReference methodsFor: 'printing' stamp: 'HAW 3/7/2019 16:41:54'!
printClassAndSelectorOn: aStream
		
	aStream nextPutAll: classSymbol.
	classIsMeta ifTrue: [ aStream nextPutAll: ' class' ].
	aStream
		nextPutAll: '>>#';
		nextPutAll: methodSymbol! !


!MethodReference methodsFor: 'printing' stamp: 'HAW 3/7/2019 16:43:30'!
printOn: aStream
	"Print the receiver on a stream"

	super printOn: aStream.
	aStream space.
	self printClassAndSelectorOn: aStream! !

