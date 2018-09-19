'From Cuis 5.0 of 7 November 2016 [latest update: #3445] on 17 September 2018 at 5:37:57 pm'!

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/17/2018 17:36:57'!
tileOpenWindows
	
	|windows regions i |
	
	windows _ (SystemWindow
		windowsIn: self runningWorld
		satisfying: [ :w | w visible]).
		
	windows ifEmpty: [^ self].
		
	regions _ self calculateTileRegions: windows size.
	
	i _ 1.
	windows do: [:w | |r|
		r _ regions at: i.
		w resize: r.
		i _ i + 1]! !

