'From Cuis 4.2 of 25 July 2013 [latest update: #1885] on 18 December 2013 at 1:05:37.558007 am'!
'Description Please enter a description for this package'!
!provides: 'SqueakCompatibility' 1 3!
!classDefinition: #Model category: #SqueakCompatibility!
ActiveModel subclass: #Model
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'Model class' category: #SqueakCompatibility!
Model class
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

!StandardFileStream methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:03'!
fileID
	^fileID! !

!UndefinedObject methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:02'!
ifNotNilDo:  ifNotNilBlock
	"Compatibility. Prefer #ifNotNil:"
	^self ifNotNil: ifNotNilBlock! !
