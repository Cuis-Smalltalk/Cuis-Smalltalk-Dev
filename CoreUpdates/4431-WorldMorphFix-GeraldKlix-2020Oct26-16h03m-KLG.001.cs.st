'From Cuis 5.0 [latest update: #4428] on 26 October 2020 at 4:06:09 pm'!

!WorldMorph methodsFor: 'misc' stamp: 'KLG 10/26/2020 16:05:08'!
buildMagnifiedBackgroundImage
	super buildMagnifiedBackgroundImage.
	backgroundImage ifNil: [ ^ self ].

	canvas ifNotNil: [ :c |
		(backgroundImage depth = 32 and: [ c form depth < 32 ]) ifTrue: [
			backgroundImage _ backgroundImage orderedDither32To16 ]]! !

