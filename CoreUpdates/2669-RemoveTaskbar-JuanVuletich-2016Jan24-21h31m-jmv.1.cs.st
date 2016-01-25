'From Cuis 4.2 of 25 July 2013 [latest update: #2667] on 24 January 2016 at 10:06:22.369487 pm'!

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 1/24/2016 21:58'!
allNonWindowRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #Taskbar ] ]! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 1/24/2016 21:58'!
collapseNonWindows
	self allNonWindowRelatedSubmorphs do: [ :m |
		m collapse]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 1/24/2016 21:58'!
deleteNonWindows
	(SelectionMenu confirm:
'Do you really want to discard all objects
that are not in windows?')
		ifFalse: [^ self].

	self allNonWindowRelatedSubmorphs do: [:m |
		m delete ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 1/24/2016 21:55'!
minimize: aMorph

	self showTaskbar.
	taskbar
		ifNil: [ self inform: 'Please install Taskbar package' ]
		ifNotNil: [ taskbar minimize: aMorph ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 1/24/2016 21:57'!
showTaskbar

	taskbar ifNil: [
		(Smalltalk classNamed: #Taskbar) ifNotNil: [ :taskbarClass |
			taskbar _ taskbarClass newRow.
			taskbar openInWorld: self ]]! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 1/24/2016 21:58'!
cleanUpWorld
	(SelectionMenu confirm: 'This will remove all windows except those
containing unsubmitted text edits, and will
also remove all non-window morphs (other
than flaps) found on the desktop.  Are you
sure you want to do this?') ifFalse: [ ^ self ].
	myWorld allNonWindowRelatedSubmorphs do: [ :m |
		m delete ].
	(SystemWindow
		windowsIn: myWorld
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !

!methodRemoval: PasteUpMorph #allNonFlapRelatedSubmorphs!
PasteUpMorph removeSelector: #allNonFlapRelatedSubmorphs!
!methodRemoval: PasteUpMorph #allNonWindoeRelatedSubmorphs!
PasteUpMorph removeSelector: #allNonWindoeRelatedSubmorphs!
!classRemoval: #Taskbar!
Smalltalk removeClassNamed: #Taskbar!
!classRemoval: #TaskbarTask!
Smalltalk removeClassNamed: #TaskbarTask!
