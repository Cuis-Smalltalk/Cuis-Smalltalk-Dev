'From Cuis 5.0 of 7 November 2016 [latest update: #3743] on 13 May 2019 at 12:39:08 pm'!

!Color methodsFor: 'transformations' stamp: 'jmv 5/13/2019 12:37:50'!
*= anObject
	super *= anObject.
	self clipToValidValues! !

!Color methodsFor: 'transformations' stamp: 'jmv 5/13/2019 12:38:03'!
+= anObject
	super += anObject.
	self clipToValidValues! !

!Color methodsFor: 'transformations' stamp: 'jmv 5/13/2019 12:38:13'!
-= anObject
	super -= anObject.
	self clipToValidValues! !

!Color methodsFor: 'transformations' stamp: 'jmv 5/13/2019 12:38:18'!
/= anObject
	super /= anObject.
	self clipToValidValues! !

!Color methodsFor: 'private' stamp: 'jmv 5/13/2019 12:37:37'!
clipToValidValues
	| v |
	1 to: self size do: [ :i |
		v _ self at: i.
		v > 1 ifTrue: [self at: i put: 1.0].
		v < 0 ifTrue: [self at: i put: 0.0]]! !


!Color methodsFor: 'transformations' stamp: 'jmv 5/13/2019 12:24:11'!
* aNumberOrColor
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Color brown *2) display
	"
	(aNumberOrColor is: #Color) ifTrue: [
		^ (Color new
			setRed: (self red * aNumberOrColor red min: 1.0 max: 0.0)
			green: (self green * aNumberOrColor green min: 1.0 max: 0.0)
			blue: (self blue * aNumberOrColor blue min: 1.0 max: 0.0))
				alpha: self alpha * aNumberOrColor alpha
		].
	^ (Color new
		setRed: (self red * aNumberOrColor min: 1.0 max: 0.0)
		green: (self green * aNumberOrColor min: 1.0 max: 0.0)
		blue: (self blue * aNumberOrColor min: 1.0 max: 0.0))
			alpha: self alpha! !

!Color methodsFor: 'transformations' stamp: 'jmv 5/13/2019 12:25:02'!
/ aNumberOrColor
	"Answer this color with its RGB divided by the given number. "
	"
	(Color red / 2) display
	"
	(aNumberOrColor is: #Color) ifTrue: [
		^ Color new
			setRed: (self red / aNumberOrColor red min: 1.0 max: 0.0)
			green: (self green / aNumberOrColor green min: 1.0 max: 0.0)
			blue: (self blue / aNumberOrColor blue min: 1.0 max: 0.0)
		].
	^ Color new
		setRed: (self red / aNumberOrColor min: 1.0 max: 0.0)
		green: (self green / aNumberOrColor min: 1.0 max: 0.0)
		blue: (self blue / aNumberOrColor min: 1.0 max: 0.0)! !

