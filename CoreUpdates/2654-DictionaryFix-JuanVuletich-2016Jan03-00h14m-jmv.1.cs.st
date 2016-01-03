'From Cuis 4.2 of 25 July 2013 [latest update: #2653] on 3 January 2016 at 12:22:21.945575 am'!

!MethodDictionary methodsFor: 'accessing'!
add: anAssociation
	^ self at: anAssociation key put: anAssociation value! !


!WeakKeyDictionary methodsFor: 'adding' stamp: 'ar 3/21/98 16:00'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation! !


!WeakValueDictionary methodsFor: 'adding' stamp: 'ar 3/21/98 16:02'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation! !


!Dictionary methodsFor: 'accessing' stamp: 'jmv 1/3/2016 00:18'!
at: key put: anObject
	"Set the value at key to be anObject. 
	If key is not found, create a new entry for key and set is value to anObject.
	If key is found, update the existing association.
	Answer anObject."

	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	assoc
		ifNil: [ self atNewIndex: index put: (Association key: key value: anObject) ]
		ifNotNil: [ assoc value: anObject ].
	^ anObject! !

!Dictionary methodsFor: 'adding' stamp: 'jk 1/3/2016 00:17'!
add: anAssociation
	"Quite like doing
		self at: anAssociation key put: anAssociation value
	but making sure the argument is stored.
	This method should be used when the argument is stored elsewhere,
	and its value should be kept in sync with the value stored in me.

	If the key already exists, and it is desired to keep the existing association, then call #at:put:"

	| index |
	index _ self findElementOrNil: anAssociation key.
	(array at: index)
		ifNotNil: [ array at: index put: anAssociation ]
		ifNil: [ self atNewIndex: index put: anAssociation ].
	^ anAssociation! !

!Dictionary methodsFor: 'adding' stamp: 'jmv 1/2/2016 23:56'!
declare: key from: aDictionary
	"Add key to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver. Answer it.
	If the key wasn't in the receiver or aDictionary, use nil as the value."

	| association |
	(self includesKey: key) ifTrue: [^ nil].
	(aDictionary includesKey: key)
		ifTrue: [
			association _ aDictionary associationAt: key.
			self add: association.
			aDictionary removeKey: key.
			^ association ]
		ifFalse: [
			self at: key put: nil.
			^ nil ]! !


!SystemDictionary methodsFor: 'dictionary access' stamp: 'jmv 1/3/2016 00:22'!
at: aKey put: anObject 
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."

	| association |
	(self includesKey: aKey) ifFalse: [
		self flushClassNameCache.

		"Update existing association if there is one."
		(Undeclared includesKey: aKey) ifTrue: [
			association _ self declare: aKey from: Undeclared.
			association value: anObject.
			^ anObject ]].

	"Update existing association if there is one."
	^super at: aKey put: anObject! !

