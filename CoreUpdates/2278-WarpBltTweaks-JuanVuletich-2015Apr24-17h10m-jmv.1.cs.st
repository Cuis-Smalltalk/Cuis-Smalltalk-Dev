'From Cuis 4.2 of 25 July 2013 [latest update: #2277] on 24 April 2015 at 5:15:42.356651 pm'!

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 4/24/2015 10:16'!
test1Mirrored
	"
	Display restoreAfter: [WarpBlt test1Mirrored]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 topRight. r1 bottomRight. r1 bottomLeft}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		warp copyQuad: pts toRect: (r1 extent*5-ext//2 extent: ext)]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 4/24/2015 10:21'!
testSourceDestOverlap
	"Show that if source and destination are the same, then source quad and dest rect should not overlap,
	or data will be lost.
	WarpBlt testSourceDestOverlap
	"
	| warp | 
	warp _ (WarpBlt toForm: Display)
		sourceForm: Display;
		combinationRule: Form over.
	warp copyQuad: {0@0. 160@0. 160@160. 0@160} toRect: (0@0 extent: 160@160)! !


!Utilities class methodsFor: 'user interface' stamp: 'jmv 4/24/2015 10:15'!
informUser: aString during: aBlock
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"Utilities informUser: 'Just a sec!!' during: [(Delay forSeconds: 1) wait]"

	(MVCMenuMorph from: (SelectionMenu labels: '') title: aString) 
				displayAt: Sensor mousePoint + 60
				during: aBlock! !

