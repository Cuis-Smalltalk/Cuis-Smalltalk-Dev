'From Cuis 4.2 of 25 July 2013 [latest update: #1793] on 13 August 2013 at 9:23:09.201531 am'!

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:17'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ]) do: [ :each |
		each makeMeFullyVisible ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:18'!
closeUnchangedWindows
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	(SelectionMenu confirm: 'Do you really want to close all windows
except those with unaccepted edits?') ifFalse: [ ^ self ].
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:19'!
expandAll
	"Expand all windows"
	Taskbar singleton restoreAll! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:20'!
findDirtyBrowsers: evt
	"Present a menu of window titles for browsers with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [
				(w model isKindOf: Browser) and: [ w canDiscardEdits not ]]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:20'!
findDirtyWindows: evt
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits not ]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:23'!
findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu _ MenuMorph new.
	expanded _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs _ self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: Color red ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: Color red ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 nameForFindWindowFeature caseInsensitiveLessOrEqual: w2 nameForFindWindowFeature ]) do: [ :w |
		menu
			add: w nameForFindWindowFeature
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 8/13/2013 09:20'!
fullRepaintNeeded
	worldState doFullRepaint.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]! !


!SystemWindow class methodsFor: 'top window' stamp: 'jmv 8/13/2013 09:21'!
windowsIn: aWorld satisfying: windowBlock
	| windows |
	windows _ OrderedCollection new.
	aWorld submorphs do: [ :m |
		((m is: #SystemWindow) and: [ windowBlock value: m ]) ifTrue: [ windows addLast: m ]].
	^ windows! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/13/2013 09:21'!
cleanUpWorld
	(SelectionMenu confirm: 'This will remove all windows except those
containing unsubmitted text edits, and will
also remove all non-window morphs (other
than flaps) found on the desktop.  Are you
sure you want to do this?') ifFalse: [ ^ self ].
	myWorld allNonFlapRelatedSubmorphs do: [ :m |
		m delete ].
	(SystemWindow
		windowsIn: myWorld
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !

