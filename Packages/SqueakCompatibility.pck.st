'From Cuis 4.2 of 25 July 2013 [latest update: #2093] on 29 November 2014 at 5:51:43.714597 pm'!
'Description Please enter a description for this package'!
!provides: 'SqueakCompatibility' 1 6!
!classDefinition: #Model category: #SqueakCompatibility!
ActiveModel subclass: #Model
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'Model class' category: #SqueakCompatibility!
Model class
	instanceVariableNames: ''!

!classDefinition: #TimeStamp category: #SqueakCompatibility!
DateAndTime subclass: #TimeStamp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TimeStamp class' category: #SqueakCompatibility!
TimeStamp class
	instanceVariableNames: ''!


!Model commentStamp: '<historical>' prior: 0!
Compatibility. Prefer ActiveModel.!

!ProtoObject methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:02'!
ifNotNilDo: ifNotNilBlock
	"Compatibility. Prefer #ifNotNil:"
	^self ifNotNil: ifNotNilBlock! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg
	"Activate the receiver, with one or zero arguments."
	
	numArgs >= 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg
	"Activate the receiver, with two or less arguments."
	
	numArgs >= 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg cull: thirdArg
	"Activate the receiver, with three or less arguments."
	
	numArgs >= 2 ifTrue: [ 
		numArgs >= 3 ifTrue: [ ^self value: firstArg value: secondArg value: thirdArg ].
		^self value: firstArg value: secondArg ].
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg
	"Activate the receiver, with four or less arguments."
	
	numArgs >= 3 ifTrue: [
		numArgs >= 4 ifTrue: [
			^self value: firstArg value: secondArg value: thirdArg value: fourthArg ].
		^self value: firstArg value: secondArg value: thirdArg ].
	numArgs = 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
cr
	^self crString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
crlf
	^self crlfString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
lf
	^self lfString! !

!Character methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:26'!
asUnicode
	"Answer the unicode encoding of the receiver"
	"self leadingChar = 0 ifTrue: [^ self asInteger].
	^self encodedCharSet charsetClass convertToUnicode: self charCode"
	^self asInteger! !

!UndefinedObject methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:02'!
ifNotNilDo: ifNotNilBlock
	"Compatibility. Prefer #ifNotNil:"
	^self ifNotNil: ifNotNilBlock! !
