'From Cuis6.3 [latest update: #6242] on 23 February 2024 at 9:45:00 am'!

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/23/2024 06:10:06'!
forceSaveContents
	"Saves the contents to the file name in the #lastFilenameUsedToSave property.
	
	If none exists ask the user for a filename."
	
	self hasSaveAs ifFalse: [ ^ self ].
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
					self setLabel: file pathName ]].! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/23/2024 06:10:46'!
saveContents
	"Prompts the user for a file name and saves the contents to the file"

	self hasSaveAs ifFalse: [ ^ self ].
	self
		request: 'Enter file name'
		initialAnswer: (self valueOfProperty: #lastFilenameUsedToSave ifAbsent: [ '' ])
		verifying: [ :aString |
			aString notEmpty ]
		do: [ :fileName | | file |
			file := fileName asFileEntry.
			self saveContentsTo: file.
			self setLabel: file pathName ].! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/23/2024 06:05:42'!
saveContentsTo: aFileEntry
	"Saves the contents to the given filename"

	self hasSaveAs ifFalse: [^self].
	aFileEntry writeStreamDo: [ :stream |
		model saveOn: stream ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName;
		notifyUserWith: 'Contents saved to', String newLineString, aFileEntry pathName.! !

