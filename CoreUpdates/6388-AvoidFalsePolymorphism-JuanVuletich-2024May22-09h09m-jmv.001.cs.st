'From Cuis7.1 [latest update: #6387] on 22 May 2024 at 9:11:18 am'!

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 5/22/2024 09:10:26'!
doKeystrokeAction: aChar
	| args |
	keystrokeActionSelector ifNil: [^false].
	args := keystrokeActionSelector numArgs.
	args = 1 ifTrue: [
		mainView perform: keystrokeActionSelector with: aChar.
		^true ].
	args = 2
		ifTrue: [
			mainView
				perform: keystrokeActionSelector
				with: aChar
				with: self.
			^true ].
	^self error: 'keystrokeActionSelector must be a 1- or 2-keyword symbol'.
! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 5/21/2024 16:20:01'!
keyStroke: aKeyboardEvent 
	"Process keys"
	| aCharacter |
	(self focusKeyboardFor: aKeyboardEvent) ifTrue: [ ^ self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	
	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	aKeyboardEvent isDelete ifTrue: [ "delete key" ^ self deleteAction ].
	aKeyboardEvent isBackspace ifTrue: [ "backspace key" ^ self deleteAction ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self doKeystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !

!methodRemoval: PluggableListMorph #keystrokeAction: stamp: 'jmv 5/22/2024 09:10:54'!
PluggableListMorph removeSelector: #keystrokeAction:!
