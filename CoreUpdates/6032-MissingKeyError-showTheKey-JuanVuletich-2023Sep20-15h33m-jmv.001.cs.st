'From Cuis 6.0 [latest update: #6031] on 20 September 2023 at 3:35:48 pm'!

!Dictionary methodsFor: 'private' stamp: 'jmv 9/20/2023 12:11:20'!
errorKeyNotFound: key

	self error: (
		String streamContents: [ :strm |
			strm nextPutAll: 	'key: '.
			key printOn: strm.
			strm nextPutAll: ' not found' ])! !


!Trie methodsFor: 'private' stamp: 'jmv 9/20/2023 15:33:51'!
errorKeyNotFound: key

	self error: (
		String streamContents: [ :strm |
			strm nextPutAll: 	'key: '.
			key printOn: strm.
			strm nextPutAll: ' not found' ])! !


!Dictionary methodsFor: 'accessing' stamp: 'jmv 9/20/2023 12:05:58'!
associationAt: key 
	"Answer the association at key.  If key is not found, create an error message."

	^self associationAt: key ifAbsent: [ self errorKeyNotFound: key ]! !

!Dictionary methodsFor: 'accessing' stamp: 'jmv 9/20/2023 12:05:53'!
at: key 
	"Answer the value at key.  If key is not found, create an error message."

	^self at: key ifAbsent: [ self errorKeyNotFound: key ]! !

!Dictionary methodsFor: 'removing' stamp: 'jmv 9/20/2023 12:06:04'!
removeKey: key 
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key."

	^self removeKey: key ifAbsent: [ self errorKeyNotFound: key ]! !


!OrderedDictionary methodsFor: 'accessing' stamp: 'jmv 9/20/2023 12:06:18'!
at: newKey replacingKey: oldKey put: anObject
	"Mostly similar to doing
		aDict removeKey: oldKey; at: newKey put: anObject
	But newKey will be stored in the position (wrt do: and keysDo:) where oldKey was.
	"

	| keyIndex |
	newKey = oldKey ifTrue: [
		^self at: newKey put: anObject ].

	super removeKey: oldKey ifAbsent: [ self errorKeyNotFound: oldKey ].
	keyIndex := orderedKeys indexOf: oldKey.

	(self includesKey: newKey) ifTrue: [
		"Do expensive linear seach only if newKey was already there, so we can't tell where it was."
		orderedKeys remove: newKey ].
	self at: newKey put: anObject.
	orderedKeys last = newKey ifTrue: [
		orderedKeys removeLast ].
	orderedKeys at: keyIndex put: newKey.

	^anObject! !


!Trie methodsFor: 'accessing' stamp: 'jmv 9/20/2023 12:06:25'!
at: key 
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [ self errorKeyNotFound: key ]! !

!Trie methodsFor: 'removing' stamp: 'jmv 9/20/2023 12:06:36'!
removeKey: aString 
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: aString ifAbsent: [ self errorKeyNotFound: aString ]! !

!methodRemoval: Trie #errorKeyNotFound stamp: 'jmv 9/20/2023 15:34:59'!
Trie removeSelector: #errorKeyNotFound!
!methodRemoval: Dictionary class #keyNotFoundErrorDescription stamp: 'jmv 9/20/2023 15:35:02'!
Dictionary class removeSelector: #keyNotFoundErrorDescription!
!methodRemoval: Dictionary #errorKeyNotFound stamp: 'jmv 9/20/2023 15:34:59'!
Dictionary removeSelector: #errorKeyNotFound!
