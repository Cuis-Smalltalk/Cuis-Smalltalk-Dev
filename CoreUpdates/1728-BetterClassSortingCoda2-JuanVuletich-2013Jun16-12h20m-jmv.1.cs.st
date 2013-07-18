'From Cuis 4.1 of 12 December 2012 [latest update: #1727] on 16 June 2013 at 12:21:49.619907 pm'!

!CodePackage methodsFor: 'listing' stamp: 'jmv 6/16/2013 12:21'!
classesInSafeOrderDo: aBlock

	| myClasses myPoolDicts |
	myPoolDicts _ Set new.
	myClasses _ Set new.
	self classesDo: [ :cls |
		(cls inheritsFrom: SharedPool)
			ifTrue: [ myPoolDicts add: cls ]
			ifFalse: [ myClasses add: cls ]].
	Smalltalk hierarchySorted: myPoolDicts do: aBlock.
	Smalltalk hierarchySorted: myClasses do: aBlock! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 6/16/2013 12:21'!
writeOnStream: aStream

	| sortedClasses |
	sortedClasses _ Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			strm nextPut: cls ]].
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsOn: aStream;
		write: sortedClasses initializersOn: aStream! !

!methodRemoval: CodePackage #classesInSuperclassOrderDo:!
CodePackage removeSelector: #classesInSuperclassOrderDo:!
