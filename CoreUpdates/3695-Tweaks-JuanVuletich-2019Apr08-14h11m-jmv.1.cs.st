'From Cuis 5.0 of 7 November 2016 [latest update: #3694] on 8 April 2019 at 2:43:51 pm'!

!Character methodsFor: 'accessing' stamp: 'jmv 4/8/2019 14:19:18'!
leadingChar
	"See Squeak if curious."
	^ 0! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 4/8/2019 14:39:23'!
transformFrom: originalBounds to: resultBounds
	"Answer a Transform to translate coordinates inside originalBounds into coordinates inside resultBounds.
	Parameters are instances of Rectangle. Therefore, there's no rotation involved, just scale and offset."

	^((self withTranslation: (resultBounds topLeft + resultBounds bottomRight / 2.0)) composedWith:
		(self withPointScale: (resultBounds extent / originalBounds extent) asFloatPoint)) composedWith:
			(self withTranslation: (originalBounds topLeft + originalBounds bottomRight / 2.0) negated)! !

!methodRemoval: StrikeFont #maxAscii!
StrikeFont removeSelector: #maxAscii!
!methodRemoval: StrikeFont #minAscii!
StrikeFont removeSelector: #minAscii!
