'From Cuis7.1 [latest update: #6589] on 13 August 2024 at 6:31:47 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/13/2024 18:31:06'!
saveAndQuit
	"Save image and quit. No questions asked.
	Smalltalk saveAndQuit.
	"
	ChangeSet zapAllChangeSets.
	self garbageCollect.
	^ self
		snapshot: true
		andQuit: true
		embedded: false
		clearAllClassState: false.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/13/2024 18:31:11'!
saveAndQuitAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: true embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/13/2024 18:31:15'!
saveAndStayAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/13/2024 18:31:18'!
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
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/13/2024 18:31:22'!
saveImage
	"Save image. Don't quit. No questions asked.
	Smalltalk saveImage.
	"
	ChangeSet zapAllChangeSets.
	self garbageCollect.
	^ self
		snapshot: true
		andQuit: false
		embedded: false
		clearAllClassState: false.! !

