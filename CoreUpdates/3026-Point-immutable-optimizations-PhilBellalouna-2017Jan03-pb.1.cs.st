'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 29 October 2016 at 5:48:41 pm'!
!Point methodsFor: 'arithmetic' stamp: 'pb 10/31/2016 03:42:16'!
+ arg
	"Answer a Point that is the sum of the receiver and arg."
	arg isPoint ifTrue: [
		self == `0 @ 0` ifTrue: [^ arg].
		arg == `0 @ 0` ifTrue: [^ self].
		^ x + arg x @ (y + arg y) ].
	^ arg
		adaptToPoint: self
		andSend: #+.! !

!Point methodsFor: 'arithmetic' stamp: 'pb 10/31/2016 03:42:28'!
- arg
	"Answer a Point that is the difference of the receiver and arg."
	arg isPoint ifTrue: [
		arg == `0 @ 0` ifTrue: [^ self].
	    	"FIXME - is this check worth it?
		self == `0 @ 0` ifTrue: [^ arg negated].
		"
		^ x - arg x @ (y - arg y)].
	^ arg
		adaptToPoint: self
		andSend: #-.! !

!MorphicTranslation methodsFor: 'accessing' stamp: 'pb 10/31/2016 04:33:36'!
translation
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"
	(deltaX = 0 and: [ deltaY = 0 ]) ifTrue: [ ^ `0 @ 0` ].
	^ deltaX @ deltaY.! !
