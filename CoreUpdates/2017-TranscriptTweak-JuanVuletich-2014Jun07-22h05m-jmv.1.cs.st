'From Cuis 4.2 of 25 July 2013 [latest update: #2016] on 7 June 2014 at 10:06:54.592067 pm'!

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 6/7/2014 13:00'!
bounds: aRectangle

	innerRectangle _ aRectangle insetBy: self borderWidth + self padding! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 6/7/2014 22:06'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: true;
		bounds: bounds;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		bounds: (0@0 extent: self morphExtentInWorld);
		displayOn: form;
		bounds: self morphBoundsInWorld.
	aCanvas image: form at: 0@0! !

!methodRemoval: Transcript class #morphBoundsInWorld:!
Transcript class removeSelector: #morphBoundsInWorld:!
