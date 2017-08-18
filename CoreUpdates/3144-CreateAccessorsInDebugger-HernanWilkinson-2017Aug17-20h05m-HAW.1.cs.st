'From Cuis 5.0 of 7 November 2016 [latest update: #3129] on 18 August 2017 at 9:07:32 am'!

!Message methodsFor: 'stub creation' stamp: 'HAW 8/18/2017 09:04:10'!
addGetterCodeOn: stream

	stream
		newLine; tab;
		nextPut: $^;
		nextPutAll: selector ! !

!Message methodsFor: 'stub creation' stamp: 'HAW 8/18/2017 09:04:52'!
addSetterCodeOn: stream with: argumentName
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: argumentName ! !

!Message methodsFor: 'stub creation' stamp: 'HAW 8/18/2017 07:43:09'!
writeShouldBeImplementedOn: stream.
		
	stream				
		nextPutAll: 'self ';
		nextPutAll: #shouldBeImplemented;
		nextPut: $.! !

!Message methodsFor: 'testing' stamp: 'HAW 8/18/2017 07:40:37'!
isGetter

	^selector isUnary and: [ lookupClass instVarNames includes: selector  ]! !

!Message methodsFor: 'testing' stamp: 'HAW 8/18/2017 07:44:22'!
isSetter

	^selector isKeyword and: [ self numArgs = 1 and: [ lookupClass instVarNames includes: selector allButLast ]]! !


!Message methodsFor: 'stub creation' stamp: 'HAW 8/18/2017 09:03:49'!
createStubMethod
	| argNames aOrAn argName arg argClassName |
			
	argNames _ Set new.
	^ String streamContents: [ :s |
		self selector keywords withIndexDo: [ :key :i |
			s nextPutAll: key.
			((key last = $:) or: [self selector isInfix]) ifTrue: [
				arg _ self arguments at: i.
				argClassName _ (arg class isMeta) ifTrue: ['Class'] ifFalse: [arg class name].
				aOrAn _ argClassName first isVowel ifTrue: ['an'] ifFalse: ['a'].
				argName _ aOrAn, argClassName.
				[argNames includes: argName] whileTrue: [argName _ argName, i asString].
				argNames add: argName.
				s nextPutAll: ' '; nextPutAll: argName; space
			].
		].
		s newLine; tab.
		self writeShouldBeImplementedOn: s.
		self isGetter ifTrue: [ self addGetterCodeOn: s ].
		self isSetter ifTrue: [ self addSetterCodeOn: s with: argName ].
	]! !

!methodRemoval: Message #createGetterStub!
Message removeSelector: #createGetterStub!
!methodRemoval: Message #createSetterStub!
Message removeSelector: #createSetterStub!
