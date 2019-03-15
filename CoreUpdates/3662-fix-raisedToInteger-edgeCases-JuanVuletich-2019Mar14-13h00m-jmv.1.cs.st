'From Cuis 5.0 of 7 November 2016 [latest update: #3661] on 14 March 2019 at 1:15:33 pm'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 3/14/2019 13:04:03'!
raisedToInteger: anInteger

	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	I take the first context because that's the way that was previously handled.
	Maybe further discussion is required on this topic."
	
	| bitProbe result firstTry exponent exponent1 exponent2 |

	anInteger negative ifTrue: [
		exponent := anInteger negated.
		firstTry := self raisedToInteger: exponent.
		^firstTry isInfinite
			ifFalse: [firstTry reciprocal]
			ifTrue: [
				exponent1 _ exponent // 2.
				exponent2 _ exponent - exponent1.
				(self raisedToInteger: exponent1) reciprocal * (self raisedToInteger: exponent2) reciprocal ]].
	bitProbe := 1 bitShift: anInteger highBit - 1.
 	result := self class one.
  	[
		(anInteger bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
       bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
	whileTrue: [result := result * result].
	
	^result! !

