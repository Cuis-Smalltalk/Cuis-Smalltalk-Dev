'From Cuis 4.2 of 25 July 2013 [latest update: #2350] on 9 June 2015 at 9:27:45.887503 am'!
!classDefinition: #FmSqPortableUtil category: #'FileMan-Squeak'!
FmPortableUtil subclass: #FmSqPortableUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Squeak'!

!FmSqPortableUtil class methodsFor: 'actions' stamp: 'mu 3/24/2013 00:47'!
addToStartUpList: aClass
	Smalltalk addToStartUpList: aClass! !

!FmSqPortableUtil class methodsFor: 'actions' stamp: 'mu 3/24/2013 00:47'!
removeFromStartUpList: aClass
	Smalltalk removeFromStartUpList: aClass! !

!FmSqPortableUtil class methodsFor: 'accessing' stamp: 'mu 3/24/2013 00:47'!
fileDoesNotExistException
	^FileDoesNotExistException! !

!FmSqPortableUtil class methodsFor: 'startUp' stamp: 'mu 3/26/2013 01:30'!
startUp
	FmFileIOAccessor onSystemStartUp ! !

