'From Cuis 4.1 of 12 December 2012 [latest update: #1637] on 22 March 2013 at 8:43:58 am'!

!FileList methodsFor: 'file list' stamp: 'jmv 3/22/2013 08:34'!
fileListIndex: anInteger
	"Select the file name having the given index, and display its contents."

	listIndex _ anInteger.
	fileName _ listIndex = 0
		"open the file selected"
		ifFalse: [
			self fileNameFromFormattedItem: (list at: anInteger)].

	brevityState _ #needToGetBrief.
	self changed: #fileListIndex.
	self acceptedContentsChanged.
	self changed: #updateButtonRow! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 3/22/2013 08:42'!
addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| response newName index |
	(response _ FillInTheBlankMorph
						request: ('New {1} Name?' format: {aString})
						initialAnswer: ('{1}Name' format: {aString}))
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	Cursor wait showWhile: [
		aBlock value: newName].
	self updateFileList.
	index _(1 to:  list size) detect: [ :i |
		(list at: i) includesSubString: newName ] ifNone: [ 0 ].
	self fileListIndex: index.
! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 3/22/2013 08:24'!
renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	(response _ FillInTheBlankMorph request: 'NewFileName?'
 					initialAnswer: fileName)
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	newName = fileName ifTrue: [^ self].
	directory rename: fileName toBe: newName.
	self updateFileList.
	listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
	listIndex > 0 ifTrue: [fileName _ newName].
	self changed: #fileListIndex.
	self changed: #updateButtonRow
! !

