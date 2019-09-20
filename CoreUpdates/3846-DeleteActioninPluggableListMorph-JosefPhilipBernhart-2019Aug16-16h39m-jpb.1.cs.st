'From Cuis 5.0 [latest update: #3826] on 16 August 2019 at 5:44:37 pm'!

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jpb 8/16/2019 17:34:29'!
deleteAction
	"Handles deleting action, which happens when the user presses backspace or delete key within me"
	| deleteActionBlock |
	deleteActionBlock _ self valueOfProperty: #deleteAction  ifAbsent: [ nil ].
	deleteActionBlock isNil
		ifTrue: [ self flash ]
		ifFalse: [ deleteActionBlock value ].
	^self! !


!PluggableListMorph methodsFor: 'events' stamp: 'jpb 8/16/2019 17:27:02'!
keyStroke: aKeyboardEvent 
	"Process keys"
	| aCharacter |
	(self focusKeyboardFor: aKeyboardEvent) ifTrue: [ ^ self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	
	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	aKeyboardEvent isDelete ifTrue: [ "delete key" ^ self deleteAction ].
	aKeyboardEvent isBackspace ifTrue: [ "backspace key" ^ self deleteAction ].
	
	aCharacter _ aKeyboardEvent keyCharacter.
	
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self keystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !


!KeyboardEvent methodsFor: 'testing' stamp: 'jpb 8/16/2019 16:46:40'!
isBackspace
	"Returns true if the pressed key is a backspace key. In Text Editors, pressing backspace usually means to delete the character before the cursor position"
	^ keyValue = 8! !

!KeyboardEvent methodsFor: 'testing' stamp: 'jpb 8/16/2019 16:47:15'!
isDelete
	"Returns true on the delete key, which is not the same as the backspace key. In Text Editors, it usually means to delete the character after the cursor"
	^keyValue = 127	! !

