'From Cuis7.5 [latest update: #7639] on 6 October 2025 at 9:30:26 am'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 10/6/2025 09:29:37'!
clickToFocusConsumesEvent
	"Only relevant if #focusFollowsMouse == false.
	We react immediately to clicks, even if they are used to set keyboard focus on us.
	Some subclasses do not. See inheritance."
	
	^false! !


!HierarchicalListMorph methodsFor: 'event handling testing' stamp: 'jmv 10/6/2025 09:29:29'!
clickToFocusConsumesEvent
	"Only relevant if #focusFollowsMouse == false.
	According to the preference #clickToFocusClickToSelect:
	- #always: Require one click to focus, then another to select.
	- #never: A single click sets focus and the sets or clears selection.
	- #ifSelection: We don't react to a focus setting click if we have a selection,
					so we don't lose selection just for focusing."

	| prefSymbol | 
	prefSymbol := Preferences at: #clickToFocusClickToSelect.
	prefSymbol == #never ifTrue: [ ^false ].
	prefSymbol == #always ifTrue: [ ^true ].
	"Assume #ifSelection"
	^selectedMorph notNil! !


!PluggableListMorph methodsFor: 'event handling testing' stamp: 'jmv 10/6/2025 09:29:31'!
clickToFocusConsumesEvent
	"Only relevant if #focusFollowsMouse == false.
	According to the preference #clickToFocusClickToSelect:
	- #always: Require one click to focus, then another to select.
	- #never: A single click sets focus and the sets or clears selection.
	- #ifSelection: We don't react to a focus setting click if we have a selection,
					so we don't lose selection just for focusing."

	| prefSymbol | 
	prefSymbol := Preferences at: #clickToFocusClickToSelect.
	prefSymbol == #never ifTrue: [ ^false ].
	prefSymbol == #always ifTrue: [ ^true ].
	"Assume #ifSelection"
	^self getCurrentSelectionIndex > 0! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 10/6/2025 09:29:34'!
clickToFocusConsumesEvent
	"Only relevant if #focusFollowsMouse == false.
	According to the preference #clickToFocusClickToSelect:
	- #always: Require one click to focus, then another to select.
	- #never: A single click sets focus and the sets or clears selection.
	- #ifSelection: We don't react to a focus setting click if we have a selection,
					so we don't lose selection just for focusing."

	| prefSymbol | 
	prefSymbol := Preferences at: #clickToFocusClickToSelect.
	prefSymbol == #never ifTrue: [ ^false ].
	prefSymbol == #always ifTrue: [ ^true ].
	"Assume #ifSelection"
	^editor hasSelection! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 10/6/2025 09:26:41'!
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
		#(defaultLineSpacingFactor 1.15)
		#(clickToFocusClickToSelect #never) "#never, #always, #ifSelection"
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #clickToFocusClickToSelect category: #gui value: #never.
Preferences saveToDisk: #clickToFocusClickToSelect.!

