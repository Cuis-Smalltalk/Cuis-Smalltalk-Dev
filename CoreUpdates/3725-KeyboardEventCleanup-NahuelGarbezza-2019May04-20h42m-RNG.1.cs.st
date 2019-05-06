'From Cuis 5.0 of 7 November 2016 [latest update: #3724] on 4 May 2019 at 9:17:44 pm'!

!KeyboardEvent methodsFor: 'actions' stamp: 'RNG 5/4/2019 21:13:06'!
closeCurrentWindowOf: aMorph

	aMorph owningWindow ifNotNil: [ :w |
		(w morphContainsPoint: (w internalizeFromWorld: self eventPosition))
			ifTrue: [ w delete. ] ]! !

!KeyboardEvent methodsFor: 'testing' stamp: 'RNG 5/4/2019 21:14:18'!
isCloseWindowShortcut

	^ (self commandAltKeyPressed or: [ self controlKeyPressed ])
		and: [self keyCharacter = $w]! !

!KeyboardEvent methodsFor: 'testing' stamp: 'RNG 5/4/2019 21:16:04'!
isFindClassShortcut

	^ self shiftPressed and: [ self isReturnKey ]! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'RNG 5/4/2019 21:14:56'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut
			ifTrue: [ ^ BrowserWindow findClass].
		self isCloseWindowShortcut
			ifTrue: [ ^ self closeCurrentWindowOf: aMorph ].
		^ aMorph
			processKeystroke: self
			localPosition: positionInAMorph ].
	type == #keyDown ifTrue: [
		^ aMorph
			processKeyDown: self
			localPosition: positionInAMorph ].
	type == #keyUp ifTrue: [ 
		^ aMorph
			processKeyUp: self
			localPosition: positionInAMorph ].
	^ super
		sentTo: aMorph
		localPosition: positionInAMorph.! !


!KeyboardEvent reorganize!
('comparing' = hash)
('actions' closeCurrentWindowOf:)
('dispatching' sentTo:localPosition: startDispatchFrom:)
('keyboard' keyCharacter keyValue)
('testing' isAlphaNumeric isArrowDown isArrowLeft isArrowRight isArrowUp isBackspace isCloseWindowShortcut isColon isCtrlSpace isEnd isEsc isFindClassShortcut isHome isKeyDown isKeyUp isKeyboard isKeystroke isMouseMove isPageDown isPageUp isQuesitonMark isReturnKey isSpace isTab)
('private' setType:buttons:position:keyValue:hand:stamp:)
('platform peculiarities' isCmdAltGreaterThan isCmdAltLessThan)
!

