'From Cuis 5.0 of 7 November 2016 [latest update: #3145] on 22 August 2017 at 11:23:45 am'!

!Object methodsFor: 'message handling' stamp: 'HAW 8/22/2017 10:17:59'!
argumentName

	^self argumentNameSufix prefixedWithAOrAn ! !

!Object methodsFor: 'message handling' stamp: 'HAW 8/22/2017 10:16:01'!
argumentNameSufix

	^self class isMeta ifTrue: ['Class'] ifFalse: [self class name]! !


!String methodsFor: 'converting' stamp: 'HAW 8/22/2017 10:14:02'!
aOrAnPrefix

	^self isEmpty 
		ifTrue: [ self ]
		ifFalse: [ self first isVowel ifTrue: ['an'] ifFalse: ['a'] ]
! !

!String methodsFor: 'converting' stamp: 'HAW 8/22/2017 10:14:46'!
prefixedWithAOrAn

	^self aOrAnPrefix, self! !


!Debugger methodsFor: 'method creation' stamp: 'HAW 8/22/2017 11:22:30'!
implement: aMessage inClass: aClass context: aContext 
	
	aClass
		compile: (aMessage createStubMethodFor: aClass)
		classified: (self askForCategoryIn: aClass default: 'as yet unclassified').
		
	aContext privRefreshWith: (aClass lookupSelector: aMessage selector).
	aMessage arguments withIndexDo: [ :arg :index | aContext tempAt: index put: arg ].
	self resetContext: aContext! !


!Message methodsFor: 'stub creation' stamp: 'HAW 8/22/2017 11:23:03'!
addGetterCodeOn: stream

	stream
		newLine; tab;
		nextPut: $^;
		nextPutAll: selector ! !

!Message methodsFor: 'stub creation' stamp: 'HAW 8/22/2017 11:23:07'!
addSetterCodeOn: stream with: argumentName
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: argumentName ! !

!Message methodsFor: 'stub creation' stamp: 'HAW 8/22/2017 11:22:53'!
createStubMethodFor: aClass

	| argNames argName arg  |

	argNames _ Set new.
	^ String streamContents: [ :s |
		self selector keywords withIndexDo: [ :key :i |
			s nextPutAll: key.
			((key last = $:) or: [self selector isInfix]) ifTrue: [
				arg _ self arguments at: i.
				argName _ arg argumentName.
				[argNames includes: argName] whileTrue: [argName _ argName, i asString].
				argNames add: argName.
				s nextPutAll: ' '; nextPutAll: argName; space
			].
		].
	
		s newLine; tab.
		self writeShouldBeImplementedOn: s.
		(self isGetterFor: aClass) ifTrue: [ self addGetterCodeOn: s ].
		(self isSetterFor: aClass) ifTrue: [ self addSetterCodeOn: s with: argName ].
	]! !

!Message methodsFor: 'stub creation' stamp: 'HAW 8/22/2017 11:23:11'!
writeShouldBeImplementedOn: stream.

	stream				
		nextPutAll: 'self ';
		nextPutAll: #shouldBeImplemented;
		nextPut: $.! !

!Message methodsFor: 'testing' stamp: 'HAW 8/22/2017 11:22:40'!
isGetterFor: aClass

	^selector isUnary and: [ aClass instVarNames includes: selector  ]! !

!Message methodsFor: 'testing' stamp: 'HAW 8/22/2017 11:22:43'!
isSetterFor: aClass

	^selector isKeyword and: [ self numArgs = 1 and: [ aClass instVarNames includes: selector allButLast ]]! !

!methodRemoval: Message #createStubMethod!
Message removeSelector: #createStubMethod!
!methodRemoval: Message #isGetter!
Message removeSelector: #isGetter!
!methodRemoval: Message #isSetter!
Message removeSelector: #isSetter!
