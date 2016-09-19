'From Cuis 4.2 of 25 July 2013 [latest update: #2937] on 18 September 2016 at 10:21:45 am'!

!Object methodsFor: 'tracing' stamp: 'jmv 9/18/2016 10:21:10'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts."

	| allObjectsToExclude |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.

	allObjectsToExclude _ { thisContext }, objectsToExclude.
	^Smalltalk allObjects select: [ :object |
		object isInMemory and: [
		((object statePointsTo: self) or: [object class == self]) and: [
		(allObjectsToExclude statePointsTo: object) not ]]
		]! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 9/17/2016 19:21:21'!
allObjectsDo: aBlock 
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates
	 such as SmallInteger and (in Spur) Character."
	self allObjectsOrNil
		ifNotNil: [:allObjects| allObjects do: aBlock]
		ifNil:
			["Fall back on the old single object primitive code.  With closures, this needs
			  to use an end marker (lastObject) since activation of the block will create
			  new contexts and cause an infinite loop.  The lastObject must be created
			  before calling someObject, so that the VM can settle the enumeration (e.g.
			  by flushing new space) as a side effect of  someObject"
			| object lastObject |
			lastObject := Object new.
			object := self someObject.
			[lastObject == object or: [0 == object]] whileFalse: [
				| nextObject |
				nextObject := object nextObject.
				aBlock value: object.
				object := nextObject ]]! !

!methodRemoval: SystemDictionary #allObjectsSelect:!
SystemDictionary removeSelector: #allObjectsSelect:!
