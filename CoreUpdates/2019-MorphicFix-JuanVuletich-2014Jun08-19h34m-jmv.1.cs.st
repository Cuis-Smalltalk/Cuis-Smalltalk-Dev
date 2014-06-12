'From Cuis 4.2 of 25 July 2013 [latest update: #2017] on 8 June 2014 at 7:37:55.289455 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 6/7/2014 23:00'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean"

	self visible == aBoolean
		ifTrue: [ ^ self ].
	aBoolean ifFalse: [
		self redrawNeeded ].
	self setProperty: #visible toValue: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	aBoolean ifTrue: [
		self redrawNeeded ].! !

