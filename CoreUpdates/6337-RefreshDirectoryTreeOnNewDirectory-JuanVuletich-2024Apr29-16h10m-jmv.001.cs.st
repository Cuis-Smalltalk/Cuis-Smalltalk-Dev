'From Cuis6.3 [latest update: #6336] on 29 April 2024 at 4:28:39 pm'!

!FileList methodsFor: 'file menu action' stamp: 'jmv 4/29/2024 16:16:14'!
addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	self request: ('New {1} name?' format: {aString})
		initialAnswer: ('{1}Name' format: {aString})
		verifying: [ :response | response notEmpty ]
		do: [ :response | aBlock value: response asFileName ].! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 4/29/2024 16:26:28'!
addNewDirectory

	self 
		addNew: 'directory'
		byEvaluating: [ :newName | | newDirectory |
			newDirectory := (directory / newName) assureExistence.
			self changed: #topDirectoryWrappers.
			self directory: newDirectory.
			self updateDirectory ].! !

!FileList methodsFor: 'file menu action' stamp: 'jmv 4/29/2024 16:23:53'!
addNewFile

	self 
		addNew: 'file'
		byEvaluating: [ :newName | | index |
			(directory // newName) assureExistence.
			self updateFileList.
			index := (1 to: list size) detect: [ :i |
				(list at: i) includesSubString: newName ] ifNone: [ 0 ].
			self fileListIndex: index ].! !

