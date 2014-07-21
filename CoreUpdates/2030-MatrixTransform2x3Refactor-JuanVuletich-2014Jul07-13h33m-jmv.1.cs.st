'From Cuis 4.2 of 25 July 2013 [latest update: #2027] on 7 July 2014 at 2:43:22.328915 pm'!

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 7/7/2014 14:08'!
setRadians: radians scale: scale
	"Set the raw rotation angle in the receiver"
	| s c |
	s _ radians sin * scale.
	c _ radians cos * scale.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 7/7/2014 14:12'!
identity
	^self new setIdentiy! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 7/7/2014 14:10'!
withRadians: radians
	^self new setRadians: radians scale: 1.0! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 7/7/2014 14:09'!
withRadians: radians scale: aNumber position: aPoint
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	MatrixTransform2x3 withRadians: -3 scale: 12 position: 4.5@3
	"
	^self new
		setRadians: radians scale: aNumber;
		setPosition: aPoint! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 7/7/2014 14:11'!
withScale: aPoint
	^self new setRadians: 0.0 scale: aPoint! !

!methodRemoval: MatrixTransform2x3 #setRadians:!
MatrixTransform2x3 removeSelector: #setRadians:!
!methodRemoval: MatrixTransform2x3 #setRadians:scale:position:!
MatrixTransform2x3 removeSelector: #setRadians:scale:position:!
!methodRemoval: MatrixTransform2x3 #setScale:!
MatrixTransform2x3 removeSelector: #setScale:!
