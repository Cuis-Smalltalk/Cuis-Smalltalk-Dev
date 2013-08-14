'From Cuis 4.2 of 25 July 2013 [latest update: #1790] on 12 August 2013 at 11:23:39.112022 am'!

!Morph methodsFor: 'iteration of all morphs' stamp: 'jmv 8/12/2013 10:43'!
nextMorphThat: aBlock
	"Keep looking for some morph that satisfies aBlock.
	Answer nil if none"

	| candidate satisfies |
	candidate _ self.
	[
		candidate _ candidate nextMorph.
		satisfies _ aBlock value: candidate.
		satisfies not
			"But stop after a whole turn."
			and: [ candidate ~~ self ] ] whileTrue.

	^satisfies ifTrue: [ candidate ]! !

!Morph methodsFor: 'iteration of all morphs' stamp: 'jmv 8/12/2013 10:43'!
previousMorphThat: aBlock
	"Keep looking for some morph that satisfies aBlock.
	Answer nil if none"

	| candidate satisfies |
	candidate _ self.
	[
		candidate _ candidate previousMorph.
		satisfies _ aBlock value: candidate.
		satisfies not
			"But stop after a whole turn."
			and: [ candidate ~~ self ] ] whileTrue.

	^satisfies ifTrue: [ candidate ]! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 8/12/2013 10:46'!
submorphToFocusKeyboard

	"Might answer nil"
	^self nextMorphThat: [ :m |  m handlesKeyboard and: [ m isReallyVisible ]]! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:49'!
activateNextWindow

	self nextFocusWindow ifNotNil: [ :w |
		w isTopWindow ifFalse: [
			w activateAndSendTopToBack: true ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:49'!
activatePreviousWindow

	self previousFocusWindow ifNotNil: [ :w |
		w isTopWindow ifFalse: [
			w activateAndSendTopToBack: false ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:47'!
keyboardFocusNext

	| m |
	m _ self nextFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: false.
		self newKeyboardFocus: m ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:45'!
keyboardFocusPrevious

	| m |
	m _ self previousFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: true.
		self newKeyboardFocus: m ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:47'!
nextFocusMorph
	"Or nil"

	^(keyboardFocus ifNil: [ self world ])
		previousMorphThat: [ :m |
			m handlesKeyboard and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:45'!
nextFocusWindow
	"Or nil"

	^(SystemWindow topWindow ifNil: [ self world ])
		nextMorphThat: [ :m | 
			(m is: #SystemWindow) and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:45'!
previousFocusMorph
	"Or nil"

	^ (keyboardFocus ifNil: [ self world ])
		nextMorphThat: [ :m | 
			m handlesKeyboard and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/12/2013 10:48'!
previousFocusWindow
	"Or nil"

	^ (SystemWindow topWindow ifNil: [ self world ])
		previousMorphThat: [ :m |
			(m is: #SystemWindow) and: [ m isReallyVisible ]]! !

