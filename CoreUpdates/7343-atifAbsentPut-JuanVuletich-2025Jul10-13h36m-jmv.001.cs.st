'From Cuis7.5 [latest update: #7342] on 10 July 2025 at 1:38:35 pm'!

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 7/10/2025 09:39:35'!
at: key ifAbsentPut: aBlock
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	^mutex critical: [ super at: key ifAbsentPut: aBlock ]! !


!Dictionary methodsFor: 'accessing' stamp: 'jmv 7/10/2025 09:43:22'!
at: key ifAbsentPut: aBlock
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	| index assoc newObject |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc ifNotNil: [
		^assoc value ].
	newObject := aBlock value.
	self atNewIndex: index put: (self associationClass key: key value: newObject).
	^ newObject! !

