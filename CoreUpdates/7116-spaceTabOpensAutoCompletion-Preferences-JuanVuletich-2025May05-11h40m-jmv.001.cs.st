'From Cuis7.3 [latest update: #7115] on 5 May 2025 at 11:44:58 am'!

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 5/5/2025 11:40:23'!
prefGui
	^ #( 
		#(balloonHelpEnabled true )
		#(biggerCursors false )		
		#(cacheDisplayContentWhenMovingMorphs true )
		#(cheapWindowReframe false )		
		#(clickGrabsMorphs false )	
		#(ctrlArrowsScrollHorizontally false ) 
		#(drawKeyboardFocusIndicator true )
		#(focusFollowsMouse false )
		#(focusIndicatorWidth 1)
		#(fullScreenLeavesDeskMargins true )
		#(haloEnclosesFullBounds true )
		#(halosShowCoordinateSystem true )
		#(menuKeyboardControl true )
		#(optionalButtons true )		
		#(selectiveHalos true )
		#(tapAndHoldEmulatesButton2 true )
		#(tileResizerInWindowMenu true )
		#(wantsMenuIcons true )
		#(autoReplaceNamedCharacters true)
		#(spaceTabOpensAutoCompletion true)
	)! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 5/5/2025 11:44:07'!
openCompletionMenuFor: kbEvent

	"Ctrl-Space (not on Mac), option-Space (on Mac) for open"
	kbEvent isCtrlSpace ifTrue: [
		self openCompletionMenu.
		^ true].

	"Tab, but not after a space."
	(kbEvent isTab and: [ self opensWithTab ]) ifTrue: [
		((Preferences at: #spaceTabOpensAutoCompletion) or: [
				self lastCharacterWantsAutoComplete]) ifTrue: [
			self openCompletionMenu.
			^ true ]].

	"Auto-open - currently deactivated"
"	(ctrl not & cmd not & alphanum) ifTrue: [ self openCompletionMenu ]."
	
	^ false! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #spaceTabOpensAutoCompletion category: #gui value: false.!

