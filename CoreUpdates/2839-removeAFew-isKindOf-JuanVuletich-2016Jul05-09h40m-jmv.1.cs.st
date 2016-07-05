'From Cuis 4.2 of 25 July 2013 [latest update: #2838] on 5 July 2016 at 9:44:20.37058 am'!

!MenuMorph methodsFor: 'menu' stamp: 'jmv 7/4/2016 22:17'!
removeStayUpBox
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LayoutMorph) ifFalse: [^self].
	box _ submorphs first submorphs first.
	(box is: #PluggableButtonMorph) 
		ifTrue: [ box hide ]! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/4/2016 22:20'!
findAFileList: evt
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileListWindow openFileList ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/4/2016 22:21'!
findAMessageNamesWindow: evt
	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == MessageNames ]
		orMakeOneUsing: [
			MessageNamesWindow open: MessageNames new label: 'Message Names' ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/5/2016 09:42'!
findDirtyBrowsers: evt
	"Present a menu of window titles for browsers with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [
				(w model is: #CodeProvider) and: [ w canDiscardEdits not ]]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 7/4/2016 22:40'!
rescaleButtons
	"boxExtent changed.  Update my buttons."
	| buttonExtent buttonPos buttonDelta |
	buttonExtent := self boxExtent.
	buttonPos := 2@2.
	buttonDelta := self boxExtent x + 2.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos := (buttonPos x  + buttonDelta) @ 2.
		].
	].
! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/4/2016 22:14'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: ((target is: #SystemWindow) ifTrue: [target windowColor] ifFalse: [Color lightBlue alpha: 0.9]).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color black.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	self addMorph: nameBackground position: namePosition - 2.
	self addMorph: nameMorph position: namePosition.
	^nameMorph! !

