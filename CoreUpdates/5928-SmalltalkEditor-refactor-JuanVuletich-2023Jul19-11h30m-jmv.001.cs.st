'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 11:32:30 am'!

!Editor methodsFor: 'private' stamp: 'jmv 7/19/2023 11:31:53'!
lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.
	
	Do not push down this method. It is used in SimpleEditor of package Widgets-Extras
	"

	"if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [
		self selectLine ].
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]! !


!TextEditor methodsFor: 'new selection' stamp: 'jmv 7/19/2023 11:32:16'!
selectLine
	"Make the receiver's selection, encompass the current line."

	self selectInterval: (model actualContents encompassLine: self selectionInterval)! !

