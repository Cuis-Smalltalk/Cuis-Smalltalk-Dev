'From Cuis6.3 [latest update: #6157] on 2 January 2024 at 7:01:13 am'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 1/2/2024 06:50:37'!
arcTanNonPrimitive
	"Answer the angle in radians."

	| theta delta sinTheta cosTheta minPossibleDelta |

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta := (self * Halfpi) / (self + 1.0).

	"iterate"
	[
		sinTheta := theta sin.
		cosTheta := theta cos.
		delta := (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		minPossibleDelta := theta ulp.
		delta abs >= minPossibleDelta ]
		whileTrue: [
			theta := theta - delta.
			"There is a risk of not converging if #sin and #cos are not good enough.
			Still, there's no point in trying to further reduce delta, meaning we won't find
			a better approximation of the result."
			delta abs = minPossibleDelta ifTrue: [
				^theta ]].
	^ theta! !

