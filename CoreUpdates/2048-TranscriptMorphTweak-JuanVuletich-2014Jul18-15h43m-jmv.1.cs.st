'From Cuis 4.2 of 25 July 2013 [latest update: #2044] on 18 July 2014 at 4:05:59.108581 pm'!

!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 16:05'!
drawOn: aCanvas
	"This breaks shadow when taking the TranscriptWindow with the hand"
	"
	Transcript
		showOnDisplay: false;
		bounds: self morphBoundsInWorld;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		bounds: (0@0 extent: extent);
		displayOn: form;
		bounds: self morphBoundsInWorld.
	aCanvas image: form at: 0@0
	! !

