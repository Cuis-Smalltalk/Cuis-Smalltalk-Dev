'From Cuis 4.2 of 25 July 2013 [latest update: #2262] on 20 April 2015 at 5:02:17.235539 pm'!

!Behavior methodsFor: 'system startup' stamp: 'jmv 4/20/2015 16:58'!
releaseClassState
	"Will be called for each class when building a new release.
	All class vars or class instVar vars that can be recreated lazily on demand, even if user options or such are lost, should be nilled.
	See implementors for examples"! !


!Taskbar class methodsFor: 'class state access' stamp: 'jmv 4/20/2015 16:47'!
releaseClassState
	"Not just cached state but actual state. User preferences will be forgotten, and defaults will be assumed.
	Taskbar releaseClassState
	"
	SoleInstance ifNotNil: [
		SoleInstance dismissMorph.
		SoleInstance _ nil ]! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 4/20/2015 16:55'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	self world ifNotNil: [ :w |
		self
			morphPosition: 0@ (w morphExtent y - (self externalizeDistance: extent ) y)
			extent: (self internalize: self world morphExtent) x @ self defaultHeight ]! !


!Taskbar class methodsFor: 'events' stamp: 'jmv 4/20/2015 16:51'!
screenSizeChanged
	"My uniqueInstance handles screen size changes"

	"Wait until Display has had a chance to complete its update"
	WorldState addDeferredUIMessage: [  SoleInstance ifNotNil: [ SoleInstance screenSizeChanged ]]! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 4/20/2015 17:02'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	Taskbar reset; initialize.
	self runningWorld ifNotNil: [ :w |
		w restoreMorphicDisplay ].
	
	^ CurrentTheme! !

