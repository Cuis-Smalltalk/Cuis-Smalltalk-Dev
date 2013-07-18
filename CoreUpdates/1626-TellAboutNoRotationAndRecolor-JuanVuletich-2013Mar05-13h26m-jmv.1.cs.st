'From Cuis 4.1 of 12 December 2012 [latest update: #1625] on 5 March 2013 at 1:30:01 pm'!

!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 3/5/2013 13:29'!
rotationDegrees: degrees
	self flag: #jmvVer2.
	self showBalloon: 'General rotation is currently disabled.'! !

!Morph methodsFor: 'menus' stamp: 'jmv 3/5/2013 13:29'!
changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #color:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"
	self flag: #jmvVer2.
	self showBalloon: 'Interactive color change is currently disabled. Please use #color:'! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 3/5/2013 13:29'!
changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu.  This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"
	self flag: #jmvVer2.
	self showBalloon: 'Interactive color change is currently disabled. Please use #color:'! !

