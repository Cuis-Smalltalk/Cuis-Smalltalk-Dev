'From Cuis7.7 [latest update: #7957] on 21 May 2026 at 4:46:00 pm'!

!TextModel methodsFor: 'commands' stamp: 'jmv 21/May/2026 16:18:21'!
acceptContentsFrom: aTextModelMorph
	"Tell the owning window to save the contents if it supports that action.
	
	Also clear the user edits."
	
	aTextModelMorph owningWindow ifNotNil: [ :ow |
		ow hasSaveAs ifTrue: [ 
			ow forceSaveContents ] ].! !


!SystemWindow methodsFor: 'user interface' stamp: 'jmv 21/May/2026 16:24:37'!
forceSaveContents
	"Saves the contents to the file name in the #lastFilenameUsedToSave property.
	If none exists ask the user for a filename."

	self hasSaveAs ifTrue: [
		self valueOfProperty: #lastFilenameUsedToSave
			ifPresentDo: [ :pathName |
				self forceSaveContentsTo: pathName asFullFileEntry ]
			ifAbsent: [
				self
					request: 'Enter file name'
					initialAnswer: ''
					verifying: [ :aString | aString notEmpty ]
					do: [ :fileName | | file |
						file := fileName asFileEntry.
						(self saveContentsTo: file) ifNotNil: [ :pathName |
							self setLabel: pathName ]]]].! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 21/May/2026 16:19:10'!
forceSaveContentsTo: aFileEntry
	"Saves the contents to the given filename.
	Force overwrting the file if it exists."

	self hasSaveAs ifFalse: [^self ].
	aFileEntry forceWriteStreamDo: [ :stream |
		stream nextPutAll: model contentsForFileSaving ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName.
	model changed: #clearUserEdits.! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 21/May/2026 16:24:59'!
saveContents
	"Prompts the user for a file name and saves the contents to the file.
	Suggest the #lastFilenameUsedToSave property."

	self hasSaveAs ifTrue: [
		self
			request: 'Enter file name'
			initialAnswer: (self valueOfProperty: #lastFilenameUsedToSave ifAbsent: [ '' ])
			verifying: [ :aString | aString notEmpty ]
			do: [ :fileName | | file |
				file := fileName asFileEntry.
				(self saveContentsTo: file) ifNotNil: [ :pathName |
					self setLabel: pathName ]]].! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 21/May/2026 16:32:32'!
saveContentsTo: aFileEntry
	"Saves the contents to the given filename.
	Answer the pathName if the file is actually written, or nil otherwise."

	self hasSaveAs ifFalse: [^nil].
	aFileEntry writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [ | pathName |
			fileStream nextPutAll: model contentsForFileSaving.
			pathName := fileStream fullName.
			self 
				setProperty: #lastFilenameUsedToSave toValue: pathName;
				notifyUserWith: 'Contents saved to', String newLineString, pathName.
			model changed: #clearUserEdits.
			^pathName ]].
	^nil! !

