Class {
	#name : 'KeyboardEvent',
	#superclass : 'UserInputEvent',
	#instVars : [
		'keyEventType',
		'keyValue'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'comparing' }
KeyboardEvent >> = aMorphicEvent [

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^false ].

	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	keyValue = aMorphicEvent keyValue ifFalse: [ ^ false ].
	^ true
]

{ #category : 'dispatching' }
KeyboardEvent >> dispatchIn: focusedMorph [
	"Dispatch me.
	The event will be passed to focusedMorph.
	If #rejectsEvent:, or #wasHandled not by any reason, try with the owner.
	Note: event position is usually irrelevant for KeyboardEvents."

	| handlerMorph |
	handlerMorph := focusedMorph.
	[ handlerMorph notNil and: [self wasHandled not]] whileTrue: [
		(handlerMorph rejectsEvent: self) not ifTrue: [
			
			self sendEventTo: handlerMorph.
			
		].
		handlerMorph := handlerMorph owner.
	].
]

{ #category : 'comparing' }
KeyboardEvent >> hash [
	^buttons hash bitXor: keyValue hash

]

{ #category : 'testing' }
KeyboardEvent >> isAlphaNumeric [

	^ self keyCharacter isAlphaNumeric
]

{ #category : 'testing' }
KeyboardEvent >> isArrowDown [

	^ keyValue = 31
]

{ #category : 'testing' }
KeyboardEvent >> isArrowLeft [
	
	^keyValue = 28
]

{ #category : 'testing' }
KeyboardEvent >> isArrowRight [
	
	^keyValue = 29 
]

{ #category : 'testing' }
KeyboardEvent >> isArrowUp [

	^ keyValue = 30
]

{ #category : 'testing' }
KeyboardEvent >> isBackspace [
	"Returns true if the pressed key is a backspace key. In Text Editors, pressing backspace usually means to delete the character before the cursor position"
	^ keyValue = 8
]

{ #category : 'platform peculiarities' }
KeyboardEvent >> isCmdAltGreaterThan [
	"Answer true if the user pressed cmd/Alt + $>
	We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!)
	Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed...
	Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !"
	| char shifted |

	self commandAltKeyPressed ifFalse: [ ^false ].

	char := self keyCharacter.
	"Do the easy test for non macOS"
	Smalltalk platformName = 'Mac OS' ifFalse: [
		^char == $> ].

	shifted := self shiftPressed.
	
	"For Mac keyboard layouts where $> is unshifted (Is there such a layout?)"
	(shifted not and: [ char == $> ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $. (for example US).
	As cmd is pressed, the Mac VM gives us the unshifted character, i.e. $."
	(shifted and: [ char == $. ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $<.
	In this case, in the German layout, the Mac VM gives us $> (surprising!)"
	(shifted and: [ char == $> ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $<
	In the Spanish layout, as cmd is pressed, the Mac VM gives us the unshifted character, i.e. $<"
	(shifted and: [ char == $< ])
		ifTrue: [ ^true ].

	"We guess it is not cmd + $> then..."
	^false
]

{ #category : 'platform peculiarities' }
KeyboardEvent >> isCmdAltLessThan [
	"Answer true if the user pressed cmd/Alt + $<
	We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!)
	Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed...
	Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !"
	| char shifted |

	self commandAltKeyPressed ifFalse: [ ^false ].

	char := self keyCharacter.
	"Do the easy test for non macOS"
	Smalltalk platformName = 'Mac OS' ifFalse: [
		^char == $< ].

	shifted := self shiftPressed.
	
	"For Mac keyboard layouts where $< is unshifted (for example, German and Spanish)"
	(shifted not and: [ char == $< ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $< is shift + $, (for example US).
	As cmd is pressed, the Mac VM gives us the unshifted character, i.e. $,"
	(shifted and: [ char == $, ])
		ifTrue: [ ^true ].

	"We guess it is not cmd + $< then..."
	^false
]

{ #category : 'testing' }
KeyboardEvent >> isColon [

	^ self keyCharacter == $:.
]

{ #category : 'testing' }
KeyboardEvent >> isCtrlSpace [
	
	^ (self controlKeyPressed or: [ self rawMacOptionKeyPressed ]) and: [ self isSpace ]
]

{ #category : 'testing' }
KeyboardEvent >> isDelete [
	"Returns true on the delete key, which is not the same as the backspace key. In Text Editors, it usually means to delete the character after the cursor"
	^keyValue = 127	
]

{ #category : 'testing' }
KeyboardEvent >> isEnd [
	
	^ keyValue = 4
]

{ #category : 'testing' }
KeyboardEvent >> isEsc [

	^ keyValue = 27
]

{ #category : 'testing' }
KeyboardEvent >> isHome [
	
	^ keyValue = 1
]

{ #category : 'testing' }
KeyboardEvent >> isKeyDown [
	^ keyEventType == #keyDown
]

{ #category : 'testing' }
KeyboardEvent >> isKeyUp [
	^ keyEventType == #keyUp
]

{ #category : 'testing' }
KeyboardEvent >> isKeyboard [
	^true
]

{ #category : 'testing' }
KeyboardEvent >> isKeystroke [
	^ keyEventType == #keystroke
]

{ #category : 'testing' }
KeyboardEvent >> isPageDown [

	^ keyValue = 12
]

{ #category : 'testing' }
KeyboardEvent >> isPageUp [

	^ keyValue = 11
]

{ #category : 'testing' }
KeyboardEvent >> isQuestionMark [
	
	^ self keyCharacter == $? 
]

{ #category : 'testing' }
KeyboardEvent >> isReturnKey [
	"Answer true if the return key (called Enter in many PC keyboards) was pressed"

	^keyValue = InputSensor returnKey
]

{ #category : 'testing' }
KeyboardEvent >> isSpace [

	^ #(0 32 160) includes: keyValue.
]

{ #category : 'testing' }
KeyboardEvent >> isTab [
	"On the Mac, ctrl-shift-tab has keyValue = 25"
	^keyValue = 9 or: [ keyValue = 25 ]
]

{ #category : 'keyboard' }
KeyboardEvent >> keyCharacter [
	"Answer the character corresponding this keystroke. This is defined only for keystroke events."
	| codePoint |
	codePoint := keyValue.
	self isArrowLeft ifTrue: [ codePoint := 16r2190 ].
	self isArrowUp ifTrue: [ codePoint := 16r2191 ].
	self isArrowRight ifTrue: [ codePoint := 16r2192 ].
	self isArrowDown ifTrue: [ codePoint := 16r2193 ].
	^ Character codePoint: codePoint.
]

{ #category : 'keyboard' }
KeyboardEvent >> keyValue [
	"Answer the ascii value for this keystroke. This is defined only for keystroke events."

	^ keyValue
]

{ #category : 'dispatching' }
KeyboardEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	keyEventType == #keystroke ifTrue: [
		(self shiftPressed and: [ self isReturnKey ]) ifTrue: [ 
			(Preferences at: #classFinder) value.
			self wasHandled: true.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].
]

{ #category : 'private' }
KeyboardEvent >> setType: aSymbol buttons: anInteger position: pos keyValue: aValue hand: aHand stamp: stamp [
	
	keyEventType := aSymbol.
	buttons := anInteger.
	position := pos.
	keyValue := aValue.
	source := aHand.
	wasHandled := false.
	timeStamp := stamp.
]

{ #category : 'dispatching' }
KeyboardEvent >> startDispatchFromHand: aHand [
	"double dispatch the event dispatch"

	aHand startKeyboardDispatch: self
]
