'From Cuis 4.2 of 25 July 2013 [latest update: #1935] on 10 January 2014 at 11:29:29.283324 pm'!

!Form methodsFor: 'transitions' stamp: 'jmv 1/10/2014 23:28'!
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectsOrNil waitTime |
	i _ 0.
	clipRect _ topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect _ clipRect intersect: clipBox ].
	[ rectsOrNil _ rectForIndexBlock value: (i _ i + 1) ]
		whileNotNil: [
			t _ Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over fillColor: nil].
			Display forceDisplayUpdate.
			waitTime _ 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]! !

!Form methodsFor: 'transitions' stamp: 'jmv 1/10/2014 23:24'!
wipeImage: otherImage at: topLeft delta: delta clippingBox: clipBox

	| wipeRect bb nSteps |
	bb _ otherImage boundingBox.
	wipeRect _ delta x = 0
		ifTrue:
		[delta y = 0 ifTrue: [nSteps _ 1. bb "allow 0@0"] ifFalse: [
		nSteps _ bb height//delta y abs + 1.  "Vertical movement"
		delta y > 0
			ifTrue: [bb topLeft extent: bb width@delta y]
			ifFalse: [bb bottomLeft+delta extent: bb width@delta y negated]]]
		ifFalse:
		[nSteps _ bb width//delta x abs + 1.  "Horizontal movement"
		delta x > 0
			ifTrue: [bb topLeft extent: delta x@bb height]
			ifFalse: [bb topRight+delta extent: delta x negated@bb height]].
	^ self wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: [ :i |
		i <= nSteps
			ifTrue: [{wipeRect translatedBy: (delta* (i-1))}]
			ifFalse: [nil]]! !

!Form methodsFor: 'transitions' stamp: 'jmv 1/10/2014 23:25'!
zoomIn: goingIn orOutTo: otherImage at: topLeft vanishingPoint: vp 
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40.
	Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40."
	| nSteps j bigR lilR minTime startTime lead |
	nSteps _ 16.
	minTime _ 500.  "milliseconds"
	startTime _ Time localMillisecondClock.
	^ self wipeImage: otherImage at: topLeft clippingBox: nil rectForIndex: [ :i | "i runs from 1 to nsteps"
		i > nSteps
			ifTrue: [nil "indicates all done"]
			ifFalse: [
				"If we are going too fast, delay for a bit"
				lead _ startTime + (i-1*minTime//nSteps) - Time localMillisecondClock.
				lead > 10 ifTrue: [(Delay forMilliseconds: lead) wait].

				"Return an array with the difference rectangles for this step."
				j _ goingIn ifTrue: [i] ifFalse: [nSteps+1-i].
				bigR _ vp - (vp*(j)//nSteps) corner:
					vp + (otherImage extent-vp*(j)//nSteps).
				lilR _ vp - (vp*(j-1)//nSteps) corner:
					vp + (otherImage extent-vp*(j-1)//nSteps).
				bigR areasOutside: lilR ]]! !

!methodRemoval: Object #asOrderedCollection!
Object removeSelector: #asOrderedCollection!
