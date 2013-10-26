'From Cuis 4.0 of 21 April 2012 [latest update: #1493] on 4 December 2012 at 10:39:44 am'!

!Object methodsFor: 'tracing' stamp: 'jmv 12/4/2012 10:39'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts. Adapted from PointerFinder class >> #pointersTo:except:"

	| object lastObject pointers objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	"Do this to get rid or just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers := OrderedCollection new: 1000.
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object := self someObject.
	[lastObject == object] whileFalse: [
		object isInMemory
			ifTrue: [((object statePointsTo: self)
				or: [object class == self])
					ifTrue: [pointers add: object]].
		object := object nextObject].

	objectsToAlwaysExclude := {
		pointers collector.
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.

	^ pointers removeAllSuchThat: [ :ea |
		(objectsToAlwaysExclude identityIncludes: ea)
			or: [ objectsToExclude identityIncludes: ea ]]! !


!ClosureTests methodsFor: 'testing' stamp: 'jmv 12/4/2012 10:33'!
testIsClean
	"
	ClosureTests new testIsClean
	"
	| tempVar |
	tempVar _ 1.
	self assert: [ 3 + 4 ] isClean.
	self assert: [ :a | a * 2 ] isClean.
	self assert: [ Smalltalk size ] isClean.
	self assert: [ :blockArg | blockArg printString ] isClean.
	self assert: [ | blockTemp | blockTemp printString ] isClean.
	self assert: [ | blockTemp | blockTemp _7 ] isClean.
	self deny: [ | outerBlockTemp | [ outerBlockTemp printString ] isClean ] value.
	self deny: [ | outerBlockTemp | [ outerBlockTemp _7 ] isClean ] value.
	self deny: [ tempVar + 1 ] isClean.
	self deny: [ tempVar _ 1 ] isClean.
	self deny: [ ivar + 1 ] isClean.
	self deny: [ ivar _ 1 ] isClean.
	self deny: [ ^ true ] isClean.
	self deny: [ self printString ] isClean.
	self deny: [ ^ self ] isClean.
	self deny: [ ClassVar + 1 ] isClean.
	self deny: [ ClassVar _ 1 ] isClean! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/4/2012 10:14'!
worldState: aWorldState
	"
	| w |
	w _ self runningWorld.
	w worldState: (w instVarNamed: 'worldState')
	"
	worldState _ aWorldState.
	worldState world: self! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 12/4/2012 10:39'!
pointersToEachIn: anArray
	"Find all occurrences in the system of pointers to elements of the argument
	anObject.
	| p1 p2 |
	p1 _ (Smalltalk pointersTo: World).
	p2 _ (Smalltalk pointersToEachIn: {World}) first.
	p1 = p2.
	
	Maybe write a few tests...
	"
	| object lastObject pointers subject |
	Smalltalk garbageCollect.
	"Do this to get rid or just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers _ anArray collect: [ :each | OrderedCollection new: 1000 ].
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object _ self someObject.
	[ lastObject == object ] whileFalse: [
		object isInMemory ifTrue: [
			1 to: anArray size do: [ :i |
				subject _ anArray at: i.
				((object statePointsTo: subject)
					or: [ object class == subject ])
						ifTrue: [ (pointers at: i) add: object ]]].
		object _ object nextObject].

	pointers do: [ :oc |
		oc
			remove: anArray;
			remove: thisContext ifAbsent: nil;
			remove: thisContext sender ifAbsent: nil;
			remove: thisContext sender sender ifAbsent: nil;
			remove: oc collector ifAbsent: nil ].
	^pointers! !


!SystemDictionaryTest methodsFor: 'testing' stamp: 'jmv 12/4/2012 10:39'!
testPointersToEachIn
	"
	SystemDictionaryTest new testPointersToEachIn
	"
	| p1 p2 |
	p1 _ (Smalltalk pointersTo: Smalltalk).
	p2 _ (Smalltalk pointersToEachIn: {Smalltalk}) first.
	self assert: p1 = p2! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	PasteUpMorph allInstancesDo: [ :w |
		(w instVarNamed: 'worldState') ifNotNil: [ :ws |
			w worldState: ws ]].
	
	SmalltalkCompleter initialize!

