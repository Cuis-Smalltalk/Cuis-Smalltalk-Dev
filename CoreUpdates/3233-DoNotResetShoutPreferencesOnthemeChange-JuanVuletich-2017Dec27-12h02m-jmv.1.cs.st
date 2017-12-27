'From Cuis 5.0 of 7 November 2016 [latest update: #3232] on 27 December 2017 at 12:03:07 pm'!

!Theme class methodsFor: 'class initialization' stamp: 'jmv 12/27/2017 11:14:22'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		CurrentTheme useTaskbar
			ifTrue: [w showTaskbar]
			ifFalse: [w hideTaskbar].
		w restoreMorphicDisplay ].
	
	^ CurrentTheme! !

