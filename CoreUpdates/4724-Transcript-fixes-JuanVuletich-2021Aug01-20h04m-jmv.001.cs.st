'From Cuis 5.0 [latest update: #4723] on 1 August 2021 at 8:11:49 pm'!

!Transcript class methodsFor: 'private' stamp: 'jmv 8/1/2021 20:01:46'!
canvas
	"VectorCanvas is not safe WRT changes in Display extent.
	Besides, it is expensive in memory.
	The alternative of using (UISupervisor ui canvas) is not safe. We don't know in which state it is (currentMorph, geometry, etc), or it is in midst of state change, and inconsistent. Waiting for a safe stat is not an option: we want immeiate updates.
	The only way to no longer need BitBltCanvas is to use BitBlt directly, possibly with a special StrikeFont.
	That, of course, would mean that the Morphic version has no hope of ever matching it.
	More thought is needed to find a simple and general solution.
	"
	(displayCanvas isNil or: [
		displayCanvas class ~= BitBltCanvas]) ifTrue: [
			displayCanvas _ BitBltCanvas onForm: Display ].
	^ displayCanvas! !


!TranscriptWindow class methodsFor: 'menu-world' stamp: 'jmv 8/1/2021 19:26:46'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 				-> 	10.
			#itemOrder 				-> 	50.
			#label 				->	'Transcript'.
			#object 				-> 	TranscriptWindow.
			#selector 				-> 	#openTranscript.
			#icon 				-> 	#printerIcon.
			#balloonText 				-> 	'A window showing contents of the System Transcript'.
		} asDictionary}`! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 8/1/2021 20:09:40'!
drawOn: aCanvas
	"If we don't call super, clipping fails if zoomed / rotated, and nothing is shown."
	super drawOn: aCanvas.
	aCanvas clippingByCurrentMorphDo: [
		Transcript displayOnCanvas: aCanvas in: self morphLocalBounds ].
	self displayBounds ifNotNil: [ :r |
		Transcript bounds:r ].
	self updateWorkspace! !

!methodRemoval: Transcript class #displayOnCanvas: stamp: 'jmv 8/1/2021 20:05:19'!
Transcript class removeSelector: #displayOnCanvas:!
