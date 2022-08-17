'From Haver 6.0 [latest update: #5448] on 16 August 2022 at 9:03:11 pm'!

!FileList methodsFor: 'own services' stamp: 'KLG 8/16/2022 21:00:29'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"

	| aString aName |
	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream upToEnd.
		aName _ stream localName ].

	(Workspace new contents: aString) 
		openLabel: 'Workspace from ', aName ::
			setProperty: #lastFilenameUsedToSave
			toValue: aName
		! !


!SystemWindow methodsFor: 'user interface' stamp: 'KLG 8/16/2022 20:56:47'!
saveContents
	"Prompts the user for a file name and saves the contents to the file"

	self hasSaveAs ifFalse: [ ^ self ].
	self
		request: 'Enter file name'
		initialAnswer: (self valueOfProperty: #lastFilenameUsedToSave ifAbsent: [ '' ])
		verifying: [ :aString |
			aString notEmpty ]
		do: [ :fileName |
			self saveContentsTo: fileName ].! !

!SystemWindow methodsFor: 'user interface' stamp: 'KLG 8/16/2022 20:51:21'!
saveContentsTo: fileName
	"Saves the contents to the given filename"

	self hasSaveAs ifFalse: [^self].
	fileName asFileEntry writeStreamDo: [ :stream |
		model saveOn: stream ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: fileName;
		notifyUserWith: 'Contents saved'.! !

