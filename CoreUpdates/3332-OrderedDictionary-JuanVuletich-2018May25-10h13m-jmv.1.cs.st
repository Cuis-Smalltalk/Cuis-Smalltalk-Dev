'From Cuis 5.0 of 7 November 2016 [latest update: #3331] on 25 May 2018 at 10:41:35 am'!
!classDefinition: #OrderedDictionary category: #'Collections-Sequenceable'!
Dictionary subclass: #OrderedDictionary
	instanceVariableNames: 'orderedKeys'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!OrderedDictionary commentStamp: '<historical>' prior: 0!
Like Python's OrderedDict!


!OrderedDictionary methodsFor: 'removing' stamp: 'jmv 3/9/2018 14:33:08'!
removeKey: key ifAbsent: aBlock 

	super removeKey: key ifAbsent: [
		^ aBlock value ].
	orderedKeys remove: key! !

!OrderedDictionary methodsFor: 'enumerating' stamp: 'jmv 3/9/2018 14:45:35'!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	orderedKeys do: [ :key |
		aBlock value: (self associationAt: key ifAbsent: nil) ]! !

!OrderedDictionary methodsFor: 'enumerating' stamp: 'jmv 3/9/2018 14:45:31'!
do: aBlock
	"Evaluate aBlock for each of the receiver's values."

	orderedKeys do: [ :key |
		aBlock value: (self at: key ifAbsent: nil) ]! !

!OrderedDictionary methodsFor: 'enumerating' stamp: 'jmv 3/9/2018 14:45:24'!
keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	orderedKeys do: aBlock ! !

!OrderedDictionary methodsFor: 'accessing' stamp: 'jmv 5/25/2018 10:36:46'!
keysSortedSafely
	"Answer a sorted Collection containing the receiver's keys.
	Redefined from Dictionary: for us, propery sorted keys are keys in the order they were added."
	^ orderedKeys! !

!OrderedDictionary methodsFor: 'private' stamp: 'jmv 3/9/2018 14:33:17'!
atNewIndex: index put: anAssociation

	super atNewIndex: index put: anAssociation.
	orderedKeys add: anAssociation key! !

!OrderedDictionary methodsFor: 'private' stamp: 'jmv 3/9/2018 14:33:19'!
init: n

	super init: n.
	orderedKeys _ OrderedCollection new: n! !


!Dictionary methodsFor: 'accessing' stamp: 'jmv 5/25/2018 10:35:25'!
keysSortedSafely
	"Answer a sorted Collection containing the receiver's keys."
	| sortedKeys |
	sortedKeys _ OrderedCollection new: self size.
	self keysDo: [:each | sortedKeys addLast: each].
	sortedKeys sort:
		[ :x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	^ sortedKeys! !

!OrderedDictionary reorganize!
('removing' removeKey:ifAbsent:)
('enumerating' associationsDo: do: keysDo:)
('accessing' keysSortedSafely)
('private' atNewIndex:put: init:)
!

