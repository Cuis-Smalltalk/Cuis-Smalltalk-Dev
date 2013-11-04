'From Cuis 4.2 of 25 July 2013 [latest update: #1857] on 3 November 2013 at 5:34:28.953622 pm'!

!Taskbar methodsFor: 'stepping' stamp: 'jmv 11/3/2013 17:33'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	self morphExtent: (self internalize: self world morphExtent) x @ (Preferences standardButtonFont height * 3//2).
	self morphPosition: 0@ (self world morphExtent y - (self externalizeDistance: extent ) y)
	
	
	! !


!Taskbar class methodsFor: 'class initialization' stamp: 'jmv 11/3/2013 17:28'!
startUp
	"Sign up to get screenSizeChanged events."

"	Transcript newLine; print: 'Taskbar startUp'. "
" @@DEBUG FIXUPS
	Taskbar removeActionsForEvent: #screenSizeChanged.
	Object removeActionsWithReceiver: Taskbar.
	Object removeActionsWithReceiver: Taskbar singleton.
	Taskbar releaseActionMap.
"
	Display
		when: #screenSizeChanged
		send: #screenSizeChanged
		to: Taskbar! !

!Taskbar class methodsFor: 'events' stamp: 'KenD 11/1/2013 20:06'!
screenSizeChanged
	"My uniqueInstance handles screen size changes"

"	Transcript newLine; print: 'Taskbar #screenSizeChanged'.
"
	"Wait until Display has had a chance to complete its update"
	WorldState addDeferredUIMessage: [  self singleton screenSizeChanged ].! !


!DisplayScreen methodsFor: 'private' stamp: 'KenD 11/1/2013 20:07'!
setExtent: aPoint depth: bitsPerPixel  "DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."
	"ar 5/1/1999: ... and that is exactly why we check for the available display depths first."

	"RAA 27 Nov 99 - if depth and extent are the same and acceptable, why go through this.
	also - record when we change so worlds can tell if it is time to repaint"

	(depth = bitsPerPixel and: [aPoint = self extent and: 
					[self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bits _ nil.  "Free up old bitmap in case space is low"
		(self supportsDisplayDepth: bitsPerPixel)
			ifTrue:[super setExtent: aPoint depth: bitsPerPixel]
			ifFalse:[(self supportsDisplayDepth: bitsPerPixel negated)
				ifTrue:[super setExtent: aPoint depth: bitsPerPixel negated]
				ifFalse:["Search for a suitable depth"
					super setExtent: aPoint depth: self findAnyDisplayDepth]].
	].
"	Transcript newLine; print: 'DisplayScreen trigger: #screenSizeChanged'. "

	"Let the world know"
	self triggerEvent: #screenSizeChanged.! !


!Taskbar class methodsFor: 'class initialization' stamp: 'KenD 11/1/2013 17:54'!
initialize
"
	Taskbar initialize.
"
	Smalltalk addToStartUpList: self.
	self singleton openInWorld.
	self startUp.! !

!Taskbar class methodsFor: 'class initialization' stamp: 'KenD 11/1/2013 19:46'!
reset
"
	Taskbar reset.
"
	self singleton delete.
	SoleInstance := nil.
	self singleton openInWorld.
	self singleton screenSizeChanged.! !

Taskbar initialize!
!methodRemoval: Taskbar #step!
Taskbar removeSelector: #step!
!methodRemoval: Taskbar #stepTime!
Taskbar removeSelector: #stepTime!
!methodRemoval: Taskbar #wantsSteps!
Taskbar removeSelector: #wantsSteps!

!Taskbar reorganize!
('change reporting' delete)
('commands' minimize: restore: restoreAll)
('initialization' defaultColor)
('testing' is: isCollapsed: isSticky)
('private' buttonFor:)
('events')
('stepping' screenSizeChanged)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Taskbar initialize!

