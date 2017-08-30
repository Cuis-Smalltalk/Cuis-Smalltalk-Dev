'From Cuis 5.0 of 7 November 2016 [latest update: #3147] on 25 August 2017 at 10:56:35 am'!

!Object methodsFor: 'printing' stamp: 'jmv 8/25/2017 10:55:31'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title _ self class name.
	aStream
		nextPutAll: title aOrAnPrefix;
		space;
		nextPutAll: title! !

!Object methodsFor: 'printing' stamp: 'jmv 8/25/2017 10:55:49'!
printWithClosureAnalysisOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title _ self class name.
	aStream
		nextPutAll: title aOrAnPrefix;
		space;
		nextPutAll: title! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 8/25/2017 10:55:54'!
print: anObject on: aStream
	"Safely print anObject in the face of direct ProtoObject subclasses."
	| objClass title |
	objClass _ self objectClass: anObject.
	(objClass canUnderstand: #printOn:) ifTrue: [
		^anObject printOn: aStream].
	title _ objClass name.
	aStream
		nextPutAll: title aOrAnPrefix;
		space;
		nextPutAll: title! !

