'From Cuis7.5 [latest update: #7547] on 11 September 2025 at 9:17:34 am'!

!SystemWindow methodsFor: 'top window' stamp: 'jmv 9/9/2025 17:33:02'!
submorphToFocusKeyboard

	"Might answer nil"
	^self nextMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = self]]]! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 17:30:29'!
keyboardFocusNextWithinWindow
	| window |
	keyboardFocus ifNil: [^ false].
	window := keyboardFocus owningWindow.
	(keyboardFocus previousMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			aMorph keyboardFocused.
			^true ].
	^false! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 17:30:14'!
keyboardFocusPreviousWithinWindow
	| window |
	keyboardFocus ifNil: [^ false].
	window := keyboardFocus owningWindow.
	(keyboardFocus nextMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			aMorph keyboardFocused.
			^true ].
	^false! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 17:28:25'!
nextFocusMorph
	"Or nil"

	^(keyboardFocus ifNil: [ self world ])
		previousMorphThat: [ :m |
			m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]]]
	
	"revisar, probar"! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 17:28:43'!
previousFocusMorph
	"Or nil"

	^ (keyboardFocus ifNil: [ self world ])
		nextMorphThat: [ :m |
			m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]]]
	
	"revisar, probar"! !

