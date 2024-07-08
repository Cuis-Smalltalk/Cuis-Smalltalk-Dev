'From Cuis7.1 [latest update: #6507] on 8 July 2024 at 4:22:40 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 7/8/2024 16:19:56'!
saveAs: newName
	"Save the image under newName. Don't quit.
	See senders."

	(self fullNameForImageNamed: newName) = self imageName
		ifTrue: [
			"If saving over the our own image file, call #saveImage instead."
			self saveImage ]
		ifFalse: [
			"This method requires saving on a different file."
			self saveAs: newName clearAllClassState: false ].! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 7/8/2024 16:16:02'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self request: 'New file name?' initialAnswer: self imageName asFullFileEntry name do: [ :newName |
		(
			"Either both files are new,"
			((self fullNameForImageNamed: newName) asFullFileEntry exists not
				and: [(self fullNameForChangesNamed: newName) asFullFileEntry exists not])
			"Or user confirms overwriting them."
			or: [self confirm: ('{1} already exists. Overwrite?' format: {newName})]
		)
			ifTrue: [
				self saveAs: newName ]].
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 7/8/2024 16:10:17'!
saveAs: newName clearAllClassState: clearAllStateFlag
	"Save the image under a new name. Don't quit.
	Note: newName must be different from current name.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.! !

