'From Cuis7.5 [latest update: #7683] on 3 November 2025 at 5:21:34 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 11/3/2025 16:52:06'!
hide
	self visible: false! !

!Morph methodsFor: 'drawing' stamp: 'jmv 11/4/2025 10:35:24'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean
	Store value of 'hidden', because flags default to false."

	self visible == aBoolean
		ifTrue: [ ^ self ].

	owner ifNil: [
		self privateFlagAt: 3 put: aBoolean not.
		^self ].

	aBoolean
		ifTrue: [ self redrawNeeded ]
		ifFalse: [
			self removeHalo.
			self invalidateBounds ].
	self privateFlagAt: 3 put: aBoolean not.
	owner someSubmorphPositionOrExtentChanged.! !

