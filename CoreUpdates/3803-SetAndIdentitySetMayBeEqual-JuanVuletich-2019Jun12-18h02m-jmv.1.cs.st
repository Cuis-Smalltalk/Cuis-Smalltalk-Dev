'From Cuis 5.0 of 7 November 2016 [latest update: #3802] on 12 June 2019 at 6:11:48 pm'!

!Collection methodsFor: 'comparing' stamp: 'jmv 6/12/2019 18:08:54'!
emptyCollectionHash
	^self species hash! !


!Set methodsFor: 'comparing' stamp: 'jmv 6/12/2019 18:09:50'!
emptyCollectionHash
	^ Set hash! !


!Dictionary methodsFor: 'comparing' stamp: 'jmv 6/12/2019 18:10:04'!
emptyCollectionHash
	^ Dictionary hash! !


!Collection methodsFor: 'comparing' stamp: 'jmv 6/12/2019 18:09:13'!
hash
	"A default hash function for any collection.  Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash _ self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self do: [ :elem | hash _ hash bitXor: elem hash]].
	^ hash bitXor: self size hash
! !


!Set methodsFor: 'testing' stamp: 'jmv 6/12/2019 18:05:55'!
= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	(aSet is: #Dictionary) ifTrue: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !


!Dictionary methodsFor: 'comparing' stamp: 'jmv 7/5/2016 09:20'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value".

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	(aDictionary is: #Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !

!Dictionary methodsFor: 'comparing' stamp: 'jmv 6/12/2019 18:11:13'!
hash
	"Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash _ self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self associationsDo: [ :association | hash _ hash bitXor: association hash ]].
	^ hash bitXor: self size hash! !


!Set reorganize!
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('copying' postCopy)
('enumerating' collect: do: union:)
('comparing' emptyCollectionHash)
('objects from disk' comeFullyUpOnReload:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: rehash scanFor: swap:with: withArray:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = includes: is: occurrencesOf:)
('explorer' hasContentsInExplorer)
('inspecting' inspectorClass)
!

