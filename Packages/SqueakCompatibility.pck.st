'From Cuis 4.2 of 25 July 2013 [latest update: #2135] on 16 December 2014 at 9:17:11.240603 am'!
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

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
cr
	^self crString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
crlf
	^self crlfString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
lf
	^self lfString! !
