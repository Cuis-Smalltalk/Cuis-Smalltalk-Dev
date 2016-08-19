'From Cuis 4.2 of 25 July 2013 [latest update: #2768] on 25 May 2016 at 3:47:41.420198 pm'!
!classDefinition: #PluggableDictionary category: #'Collections-Unordered'!
Dictionary subclass: #PluggableDictionary
	instanceVariableNames: 'hashBlock equalBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!PluggableDictionary commentStamp: '<historical>' prior: 0!
Class PluggableDictionary allows the redefinition of hashing and equality by clients. This is in particular useful if the clients know about specific properties of the objects stored in the dictionary. See the class comment of PluggableSet for an example.

Instance variables:
	hashBlock	<BlockContext>	A one argument block used for hashing the elements.
	equalBlock	<BlockContext>	A two argument block used for comparing the elements.
!


!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:46'!
equalBlock
	"Return the block used for comparing the elements in the receiver."
	^equalBlock! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/27/1998 23:55'!
equalBlock: aBlock
	"Set a new equality block. The block must accept two arguments and return true if the argumets are considered to be equal, false otherwise"
	equalBlock := aBlock.! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:46'!
hashBlock
	"Return the block used for hashing the elements in the receiver."
	^hashBlock! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:46'!
hashBlock: aBlock
	"Set a new hash block. The block must accept one argument and must return the hash value of the given argument."
	hashBlock := aBlock.! !

!PluggableDictionary methodsFor: 'copying' stamp: 'ul 11/20/2009 17:36'!
copyEmpty

	^super copyEmpty
		hashBlock: hashBlock;
		equalBlock: equalBlock;
		yourself! !

!PluggableDictionary methodsFor: 'private' stamp: 'jmv 5/22/2016 23:04'!
scanFor: anObject 
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := (hashBlock
		ifNil: [ anObject hash ]
		ifNotNil: [ hashBlock value: anObject ]) \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == nil or: [
			equalBlock
				ifNil: [ element key = anObject ]
				ifNotNil: [ equalBlock value: element key value: anObject ] ])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
"	self errorNoFreeSpace"
^0! !

!PluggableDictionary methodsFor: 'private' stamp: 'jmv 5/22/2016 23:04'!
scanForEmptySlotFor: anObject
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := (hashBlock
		ifNil: [ anObject hash ]
		ifNotNil: [ hashBlock value: anObject ]) \\ array size + 1.
	[ 
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
"	self errorNoFreeSpace"
^ 0! !

!PluggableDictionary methodsFor: 'enumerating' stamp: 'ul 1/25/2010 18:40'!
collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the resulting values into a collection that is like me. Answer with the new collection."
	
	| newCollection |
	newCollection := (self species new: self size)
		hashBlock: hashBlock;
		equalBlock: equalBlock;
		yourself.
	self associationsDo: [ :each |
		newCollection at: each key put: (aBlock value: each value) ].
	^newCollection

! !


!PluggableDictionary class methodsFor: 'instance creation' stamp: 'dvf
6/10/2000 18:13'!
integerDictionary
	^ self new hashBlock: [:integer | integer hash \\ 1064164 * 1009]! !

