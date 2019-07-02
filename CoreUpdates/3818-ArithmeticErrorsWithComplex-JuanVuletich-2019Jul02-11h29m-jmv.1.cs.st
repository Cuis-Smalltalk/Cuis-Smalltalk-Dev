'From Cuis 5.0 [latest update: #3817] on 2 July 2019 at 11:34:51 am'!

!Object methodsFor: 'testing' stamp: 'jmv 7/2/2019 08:22:13'!
isFloatOrFloatComplex
	"Overridden to return true in Float and Complex"
	^ false! !


!Float methodsFor: 'testing' stamp: 'jmv 7/2/2019 08:22:25'!
isFloatOrFloatComplex
	^ true! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 7/2/2019 11:30:18'!
raisedTo: exponent 
	"Answer the receiver raised to aNumber."

	exponent isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: exponent].
	exponent isFraction ifTrue: [
		"Special case for fraction power by Nicolas Cellier:
		If aNumber is a fraction, but result must be a Float, learn it as quickly as possible, and give quick Float answer
		Allows evaluating:
		(2009/2000) raisedTo: (3958333/100000)
		"
		^ self raisedToFraction: exponent].
	self < 0 ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedTo: argument: exponent].
	0 = exponent ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = exponent ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		exponent < 0
			ifTrue: [^ (self raisedTo: exponent negated) reciprocal]
			ifFalse: [^ self]].
	^ (exponent * self ln) exp		"Otherwise use logarithms"! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 7/2/2019 11:30:27'!
raisedToFraction: aFraction
	self isZero ifTrue: [
		aFraction negative ifTrue: [^ (self raisedToFraction: aFraction negated) reciprocal].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	^ (self negated ln * aFraction) exp negated! !


!ArithmeticMessageError methodsFor: 'exceptionDescription' stamp: 'jmv 7/2/2019 10:05:55'!
defaultAction
	(receiver isFloatOrFloatComplex or: [ arguments notNil and: [arguments anySatisfy: [ :a | a isFloatOrFloatComplex ]]]) ifTrue: [
		^self floatErrorValue ].
	^ super defaultAction! !


!NegativePowerError methodsFor: 'signaling' stamp: 'jmv 7/2/2019 08:25:33'!
floatErrorValue
	^ receiver class nan! !


!ZeroDivide methodsFor: 'signaling' stamp: 'jmv 7/2/2019 11:13:46'!
floatErrorValue
	| answerClass answerSign |

	receiver isZero ifTrue: [
		answerClass _ receiver isComplex ifTrue: [ receiver class ] ifFalse: [ Float ].
		^ answerClass nan ].

	receiver isComplex ifFalse: [
		answerSign _ arguments first isComplex
			ifTrue: [ receiver sign ]
			ifFalse: [ (receiver * arguments first) sign ].
		^ answerSign = -1
			ifTrue: [ Float negativeInfinity ]
			ifFalse: [ Float infinity ]].

	^ receiver class infinity! !


!ZeroDivide class reorganize!
('as yet unclassified')
!

