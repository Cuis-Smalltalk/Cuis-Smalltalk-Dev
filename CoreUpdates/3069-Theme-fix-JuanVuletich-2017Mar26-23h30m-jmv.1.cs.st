'From Cuis 5.0 of 7 November 2016 [latest update: #3068] on 26 March 2017 at 11:30:12 pm'!

!Theme class methodsFor: 'class initialization' stamp: 'jmv 3/26/2017 23:25:17'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
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

