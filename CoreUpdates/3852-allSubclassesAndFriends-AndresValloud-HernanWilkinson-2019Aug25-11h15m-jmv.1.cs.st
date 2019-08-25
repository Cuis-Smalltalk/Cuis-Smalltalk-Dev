'From Cuis 5.0 [latest update: #3851] on 25 August 2019 at 11:21:31 am'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'sqr 8/24/2019 18:19'!
allSuperclasses
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses. The first element is the receiver's immediate  
	superclass, followed by its superclass and subsequent superclasses,
	and proceeding as long as there is a non-nil superclass."

	| answer pivot |
	answer := OrderedCollection new.
	pivot := superclass.
	[pivot == nil] whileFalse:
		[
			answer add: pivot.
			pivot := pivot superclass
		].
	^answer! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'sqr 8/24/2019 18:25'!
subclasses
	"slow implementation since Behavior does not keep track of subclasses"
	
	^ self class allInstances select: [:each | each superclass = self ]! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 8/24/2019 15:23:44'!
withAllSubclasses
	"Answer an OrderedCollection with the receiver, the receiver's descendents, and the  
	receiver's descendents' subclasses."

	^self allSubclasses addFirst: self; yourself! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'sqr 8/24/2019 18:21'!
withAllSuperclasses
	"Answer an OrderedCollection of the receiver and the receiver's 
	superclasses. See also #allSuperclasses."

	^self allSuperclasses addFirst: self; yourself! !

!methodRemoval: Behavior #addAllSubclassesTo:!
Behavior removeSelector: #addAllSubclassesTo:!
