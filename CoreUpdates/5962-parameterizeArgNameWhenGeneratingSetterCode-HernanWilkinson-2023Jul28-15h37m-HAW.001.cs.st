'From Cuis 6.0 [latest update: #5950] on 28 July 2023 at 3:51:21 pm'!

!Message methodsFor: 'stub creation' stamp: 'HAW 7/28/2023 15:46:48'!
addSetterCodeOn: stream with: anArgumentName  
		
	"This message is sent from CuisUniversity also, not only from base.
	Do not remove it - Hernan"				
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: anArgumentName ! !


!Message methodsFor: 'stub creation' stamp: 'HAW 7/28/2023 15:46:01'!
createStubMethodFor: aClass

	^ String streamContents: [ :stream |
		self writeMessageNameOn: stream.
		stream newLine; tab.
		self writeShouldBeImplementedOn: stream.
		(self isGetterFor: aClass) ifTrue: [ self addGetterCodeOn: stream ].
		(self isSetterFor: aClass) ifTrue: [ self addSetterCodeOn: stream with: self arguments first argumentName ].
	]! !

!methodRemoval: Message #addSetterCodeOn: stamp: 'HAW 7/28/2023 15:46:01'!
Message removeSelector: #addSetterCodeOn:!

!Message reorganize!
('accessing' argument arguments fullName keywords lookupClass numArgs selector sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn:with: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('testing' hasArguments hasRightNumberOfArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
!

