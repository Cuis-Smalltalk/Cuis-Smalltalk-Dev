'From Cuis7.9 [latest update: #8010] on 17 June 2026 at 3:37:00 pm'!

!EphemeronIdentityDictionary methodsFor: 'accessing' stamp: 'jmv 17/Jun/2026, 6:33:56 pm (UTC)'!
associationAt: key
	"Answer the association at key.  If key is not found, create an error message.
	Avoid running the error block in the #critical: section"

	| answer errored |
	errored := false.
	answer := self associationAt: key ifAbsent: [ errored := true ].
	^errored
		ifTrue: [ self errorKeyNotFound: key ]
		ifFalse: [ answer ]! !

!EphemeronIdentityDictionary methodsFor: 'accessing' stamp: 'jmv 17/Jun/2026, 6:35:13 pm (UTC)'!
at: key
	"Answer the value at key.  If key is not found, create an error message.
	Avoid running the error block in the #critical: section"

	| answer errored |
	errored := false.
	answer := self at: key ifAbsent: [ errored := true ].
	^errored
		ifTrue: [ self errorKeyNotFound: key ]
		ifFalse: [ answer ]! !

!EphemeronIdentityDictionary methodsFor: 'accessing' stamp: 'jmv 17/Jun/2026, 6:35:37 pm (UTC)'!
removeKey: key
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key.
	Avoid running the error block in the #critical: section"

	| answer errored |
	errored := false.
	answer := self removeKey: key ifAbsent: [ errored := true ].
	^errored
		ifTrue: [ self errorKeyNotFound: key ]
		ifFalse: [ answer ]! !

