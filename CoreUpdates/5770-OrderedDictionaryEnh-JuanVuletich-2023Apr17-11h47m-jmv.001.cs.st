'From Cuis 6.0 [latest update: #5768] on 17 April 2023 at 11:48:02 am'!

!OrderedDictionary methodsFor: 'accessing' stamp: 'jmv 4/17/2023 11:40:24'!
at: newKey replacingKey: oldKey put: anObject
	"Mostly similar to doing
		aDict removeKey: oldKey; at: newKey put: anObject
	But newKey will be stored in the position (wrt do: and keysDo:) where oldKey was.
	"

	| keyIndex |
	newKey = oldKey ifTrue: [
		^self at: newKey put: anObject ].

	super removeKey: oldKey ifAbsent: [self errorKeyNotFound].
	keyIndex := orderedKeys indexOf: oldKey.

	(self includesKey: newKey) ifTrue: [
		"Do expensive linear seach only if newKey was already there, so we can't tell where it was."
		orderedKeys remove: newKey ].
	self at: newKey put: anObject.
	orderedKeys last = newKey ifTrue: [
		orderedKeys removeLast ].
	orderedKeys at: keyIndex put: newKey.

	^anObject! !

!OrderedDictionary methodsFor: 'user interface' stamp: 'jmv 4/17/2023 11:08:46'!
explorerContentsWithIndexCollect: twoArgBlock

	^ orderedKeys collect: [ :k |
		twoArgBlock value: (self at: k) value: k ].! !

