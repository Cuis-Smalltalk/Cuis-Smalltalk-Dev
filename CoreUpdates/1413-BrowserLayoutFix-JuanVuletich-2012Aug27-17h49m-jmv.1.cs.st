'From Cuis 4.0 of 21 April 2012 [latest update: #1411] on 27 August 2012 at 6:01:48 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 2/23/2009 14:03'!
hide
	owner ifNil: [^ self].
	self visible: false! !

!Morph methodsFor: 'drawing' stamp: 'jmv 2/23/2009 14:03'!
show
	"Make sure this morph is on-stage."
	self visible: true! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/27/2012 18:01'!
visible: aBoolean 
	"set the 'visible' attribute of the receiver to aBoolean"
	(self hasExtension not and:[aBoolean])
		ifTrue: [^ self].
	self visible == aBoolean
		ifTrue: [^ self].
	self assureExtension visible: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !

