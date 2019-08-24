'From Cuis 5.0 [latest update: #3850] on 24 August 2019 at 10:45:54 am'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'sqr 8/23/2019 15:32'!
allSubclasses
	"A breadth-first iterative algorithm. Significantly faster than a recursive, depth-first implementation."

	| answer finger fingerLimit each |
	answer := OrderedCollection new.
	self subclassesDo: [:some | answer add: some].
	finger := 0.
	fingerLimit := answer size.
	[finger < fingerLimit] whileTrue:
		[
			finger + 1 to: fingerLimit do:
				[:index |
					each := answer at: index.
					each subclassesDo: [:some | answer add: some]
				].
			finger := fingerLimit.
			fingerLimit := answer size.
		].
	^answer! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'sqr 8/23/2019 15:41'!
allSuperclasses
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses. The first element is the receiver's immediate  
	superclass, followed by its superclass; the last element is Object."

	| answer pivot |
	answer := OrderedCollection new.
	pivot := superclass.
	[pivot == nil] whileFalse:
		[
			answer add: pivot.
			pivot := pivot superclass
		].
	^answer! !

