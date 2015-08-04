'From Cuis 4.2 of 25 July 2013 [latest update: #2439] on 1 August 2015 at 7:45:27.92703 pm'!

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'SqR 8/1/2015 19:45'!
buildMorphicWindow

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'SUnit Test Runner'.
	self refreshWindow.
	self morphExtent: 460 @ 400! !

