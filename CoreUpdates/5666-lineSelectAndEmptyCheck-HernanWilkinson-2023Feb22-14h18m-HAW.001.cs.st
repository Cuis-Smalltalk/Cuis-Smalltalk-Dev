'From Cuis 6.0 [latest update: #5662] on 22 February 2023 at 2:20:34 pm'!

!Editor methodsFor: 'accessing-selection' stamp: 'HAW 2/22/2023 14:19:01'!
hasSelection

	self subclassResponsibility ! !

!Editor methodsFor: 'new selection' stamp: 'HAW 2/22/2023 14:18:50'!
selectLine

	self subclassResponsibility ! !


!Editor methodsFor: 'menu messages' stamp: 'HAW 2/22/2023 14:19:58'!
lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.
	
	Do not push down this method. It is used in SimpleEditor of package Widgets-Extras
	"

	self selectLine.  "if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]! !

!methodRemoval: TextEditor #lineSelectAndEmptyCheck: stamp: 'HAW 2/22/2023 14:19:08'!
TextEditor removeSelector: #lineSelectAndEmptyCheck:!
