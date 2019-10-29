'From Cuis 5.0 [latest update: #3942] on 28 October 2019 at 6:15:49 pm'!

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 10/28/2019 18:14:40'!
findInSourceCode
	| textToSearch |

	textToSearch _ FillInTheBlankMorph request: 'Text to search Source Code for?'.
	Smalltalk browseMethodsWithSourceString: textToSearch! !


!PasteUpMorph methodsFor: 'events' stamp: 'jmv 10/28/2019 18:13:11'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ BrowserWindow openBrowser ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MessageSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent! !

!methodRemoval: MessageSetWindow class #findSourceCode stamp: 'jmv 10/28/2019 18:12:59'!
MessageSetWindow class removeSelector: #findSourceCode!
