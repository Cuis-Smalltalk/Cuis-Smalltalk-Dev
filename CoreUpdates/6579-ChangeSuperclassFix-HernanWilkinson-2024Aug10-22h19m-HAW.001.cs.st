'From Cuis7.1 [latest update: #6578] on 10 August 2024 at 10:24:09 pm'!

!Class methodsFor: 'accessing class hierarchy' stamp: 'HAW 8/10/2024 22:21:41'!
superclass: aClassOrNil
	"Change the receiver's superclass to be aClass.
	Keep the subclass / superclass invariant.
	Not to be used lightly!!
	Usually used only to create subclasses of nil."

	| oldSuperclass |
	oldSuperclass := superclass.
	super superclass: aClassOrNil.
	(oldSuperclass notNil and: [ oldSuperclass ~~ superclass ]) ifTrue: [
		oldSuperclass removeSubclass: self ].
	aClassOrNil notNil ifTrue: [
		aClassOrNil addSubclass: self ].! !

