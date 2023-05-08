'From Cuis 6.0 [latest update: #5788] on 5 May 2023 at 11:32:40 am'!

!Set methodsFor: 'private' stamp: 'jmv 5/5/2023 11:31:11'!
privateSwapArrayElement: oneIndex with: otherIndex
	"May be overridden by subclasses so that fixCollisionsFrom: will work"

	array swap: oneIndex with: otherIndex.! !


!MethodDictionary methodsFor: 'private' stamp: 'jmv 5/5/2023 11:28:19'!
privateSwapArrayElement: oneIndex with: otherIndex
	| element |
	element := self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	super privateSwapArrayElement: oneIndex with: otherIndex.! !


!Set methodsFor: 'private' stamp: 'jmv 5/5/2023 11:31:57'!
fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| length oldIndex newIndex element |
	oldIndex := index.
	length := array size.
	[oldIndex = length
			ifTrue: [oldIndex := 1]
			ifFalse: [oldIndex := oldIndex + 1].
	(element := self keyAt: oldIndex) == nil]
		whileFalse: [
			newIndex := self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self privateSwapArrayElement: oldIndex with: newIndex]].! !


!MethodDictionary methodsFor: 'private' stamp: 'jmv 5/5/2023 11:29:24'!
fixCollisionsFrom: start
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| key index |
	index := start.
	[ (key := self basicAt: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self scanFor: key) = index ifFalse: [
			self privateSwapArrayElement: index with: newIndex ] ]! !


!WeakSet methodsFor: 'private' stamp: 'jmv 5/5/2023 11:29:28'!
fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| length oldIndex newIndex element |

	oldIndex := index.
	length := array size.
	[oldIndex = length
			ifTrue: [oldIndex := 1]
			ifFalse: [oldIndex := oldIndex + 1].
	(element := self keyAt: oldIndex) == flag]
		whileFalse: 
			[newIndex := self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self privateSwapArrayElement: oldIndex with: newIndex]]! !

!methodRemoval: MethodDictionary #swap:with: stamp: 'jmv 5/5/2023 11:32:09'!
MethodDictionary removeSelector: #swap:with:!
!methodRemoval: Set #swap:with: stamp: 'jmv 5/5/2023 11:32:12'!
Set removeSelector: #swap:with:!
