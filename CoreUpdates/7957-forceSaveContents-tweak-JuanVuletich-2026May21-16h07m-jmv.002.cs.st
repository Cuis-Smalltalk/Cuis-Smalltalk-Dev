'From Cuis7.7 [latest update: #7955] on 21 May 2026 at 4:08:36 pm'!

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 21/May/2026 16:08:10'!
forceSaveContents
	"Saves the contents to the file name in the #lastFilenameUsedToSave property.
	
	If none exists ask the user for a filename."
	
	self hasSaveAs ifTrue: [
		self valueOfProperty: #lastFilenameUsedToSave
			ifPresentDo: [ :pathName | self forceSaveContentsTo: pathName asFullFileEntry ]
			ifAbsent: [
				self
					request: 'Enter file name'
					initialAnswer: ''
					verifying: [ :aString | aString notEmpty ]
					do: [ :fileName | | file |
						file := fileName asFileEntry.
						self saveContentsTo: file.
						self setLabel: file pathName ]]].! !

