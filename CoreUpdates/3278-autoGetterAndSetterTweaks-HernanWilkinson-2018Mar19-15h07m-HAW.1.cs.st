'From Cuis 5.0 of 7 November 2016 [latest update: #3277] on 19 March 2018 at 3:33:10 pm'!

!Message methodsFor: 'stub creation' stamp: 'HAW 3/19/2018 15:29:03'!
addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: self arguments first argumentName ! !

!Message methodsFor: 'stub creation' stamp: 'HAW 3/19/2018 15:20:08'!
argumentNameAt: anIndex havingNamed: alreadyNamedArguments

	| argumentName |
	
	argumentName _ (self arguments at: anIndex) argumentName.
	[alreadyNamedArguments includes: argumentName] whileTrue: [argumentName _ argumentName, anIndex asString].
	alreadyNamedArguments add: argumentName.

	^argumentName! !

!Message methodsFor: 'stub creation' stamp: 'HAW 3/19/2018 15:26:18'!
writeMessageNameOn: aStream

	| alreadyNamedArguments |

	alreadyNamedArguments _ Set new.
	self selector keywords withIndexDo: [ :keyword :index |
		aStream nextPutAll: keyword.
		self hasArguments ifTrue: [ self writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments ]].
	
	! !

!Message methodsFor: 'stub creation' stamp: 'HAW 3/19/2018 15:27:23'!
writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments

	| argumentName |
	
	argumentName _ self argumentNameAt: index havingNamed: alreadyNamedArguments.
	
	aStream 
		nextPutAll: ' '; 
		nextPutAll: argumentName; 
		space
	
	! !


!Message methodsFor: 'stub creation' stamp: 'HAW 3/19/2018 15:28:33'!
createStubMethodFor: aClass

	^ String streamContents: [ :stream |
		self writeMessageNameOn: stream.
		stream newLine; tab.
		self writeShouldBeImplementedOn: stream.
		(self isGetterFor: aClass) ifTrue: [ self addGetterCodeOn: stream ].
		(self isSetterFor: aClass) ifTrue: [ self addSetterCodeOn: stream ].
	]! !

!methodRemoval: Message #addSetterCodeOn:with:!
Message removeSelector: #addSetterCodeOn:with:!
