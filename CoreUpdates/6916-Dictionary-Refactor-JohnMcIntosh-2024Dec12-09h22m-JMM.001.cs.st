'From Cuis7.3 [latest update: #6910] on 12 December 2024 at 9:23:17 am'!

!Dictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:19:05'!
associationClass

	^Association! !


!WeakKeyDictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:19:24'!
associationClass

	^WeakKeyAssociation! !


!WeakValueDictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:19:43'!
associationClass

	^WeakValueAssociation! !


!Dictionary methodsFor: 'accessing' stamp: 'JMM 12/2/2024 09:54:17'!
at: key put: anObject
	"Set the value at key to be anObject. 
	If key is not found, create a new entry for key and set is value to anObject.
	If key is found, update the existing association.
	Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [ self atNewIndex: index put: (self associationClass key: key value: anObject) ]
		ifNotNil: [ assoc value: anObject ].
	^ anObject! !


!WeakKeyDictionary methodsFor: 'accessing' stamp: 'JMM 12/12/2024 09:08:53'!
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	
	key ifNil: [ ^anObject ].
	^super at: key put: anObject! !

!methodRemoval: WeakValueDictionary #at:put: stamp: 'JMM 12/12/2024 09:22:36'!
WeakValueDictionary removeSelector: #at:put:!
