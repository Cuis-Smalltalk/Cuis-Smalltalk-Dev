'From Cuis 4.2 of 25 July 2013 [latest update: #2163] on 21 January 2015 at 11:03:23.459347 pm'!

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/21/2015 23:03'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	"
	Faster approach: Keep 2 instances, to be able to call prim.
	Cons: who cares? This is jost for M3, that inlines most calls... Besides, immediate floats makes this bogus.
t _ AffineTransformation withRadians: -3 scale: 12 position: 4.5@3.
[ 100000 timesRepeat: [ t transform: 2@3 ]] timeToRun.
[ 100000 timesRepeat: [ 2 ]] timeToRun.
z1 _ AffineTransformation withTranslation: 2@3.
z2 _ AffineTransformation new.
[ 100000 timesRepeat: [ t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
[ 100000 timesRepeat: [ z1 _ AffineTransformation withTranslation: 2@3.z2 _ AffineTransformation new.t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
"

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

