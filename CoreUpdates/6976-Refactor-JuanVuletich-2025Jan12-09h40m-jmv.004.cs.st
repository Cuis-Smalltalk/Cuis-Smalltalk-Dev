'From Cuis7.3 [latest update: #6976] on 12 January 2025 at 9:41:18 am'!

!Set methodsFor: 'lookup' stamp: 'jmv 1/10/2025 10:48:11'!
lookup: anObject
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise."

	^ array at: (self findElementOrNil: anObject).! !


!Dictionary methodsFor: 'testing' stamp: 'jmv 1/10/2025 10:48:44'!
lookup: anObject
	"Look for an element equal to anObject.
	If found, answer it. Answer nil otherwise."

	self do: [:each | anObject = each ifTrue: [^each]].
	^nil! !


!Set methodsFor: 'lookup' stamp: 'jmv 1/10/2025 10:49:13'!
includes: anObject 
	^ (self lookup: anObject) ~~ nil! !


!OldWeakSet methodsFor: 'public' stamp: 'jmv 1/10/2025 10:49:49'!
includes: anObject 
	^ (self lookup: anObject) ~~ flag! !

!methodRemoval: Dictionary #includes: stamp: 'jmv 1/12/2025 09:40:38'!
Dictionary removeSelector: #includes:!

!Set reorganize!
('accessing' atRandom: size)
('adding' add: add:withOccurrences:)
('lookup' includes: lookup:)
('enumerating' collect: do: union:)
('comparing' emptyCollectionHash)
('copying' postCopy)
('objects from disk' comeFullyUpOnReload:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: privateSwapArrayElement:with: rehash scanFor:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = is: occurrencesOf:)
('inspecting' inspectorClass)
!

