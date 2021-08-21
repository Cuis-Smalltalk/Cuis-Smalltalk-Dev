'From Cuis 5.0 [latest update: #4779] on 20 August 2021 at 8:06:06 pm'!

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:03:28'!
italizing
	"a little shear
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 + (self scale*0.2).
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:03:33'!
italizing2
	"a little shear
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 - (self scale*0.2).
	self setTranslation: (self scale*0.2)@0 + self translation.
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:03:39'!
rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a21 a22 |
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:03:48'!
scaledBy: aPointOrNumber
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt sx sy |
	pt _ aPointOrNumber asPoint.
	sx _ pt x.
	sy _ pt y.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:04:07'!
scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a21 a22 |
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11 * aNumber.
	a12 _ self a12 * aNumber.
	a21 _ self a21 * aNumber.
	a22 _ self a22 * aNumber.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:04:12'!
translatedBy: aPoint
	"add an offset in the receiver

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: self a13 + pt x.
	self a23: self a23 + pt y.
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:05:10'!
withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self setRadians: radians scale: scale.
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:05:26'!
withScale: scale
	"Set scaling according to parameters.
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self scaledBy: scale / self scale.
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:04:16'!
withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: pt x.
	self a23: pt y.
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 8/20/2021 20:05:41'!
withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12  negated.
	self a22: self a22  negated.
	^self! !

