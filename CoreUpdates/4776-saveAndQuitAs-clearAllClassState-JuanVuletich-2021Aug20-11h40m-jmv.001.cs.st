'From Cuis 5.0 [latest update: #4775] on 20 August 2021 at 11:41:07 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/20/2021 11:40:27'!
saveAndQuitAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFileEntry to: newChangesName asFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: true embedded: false clearAllClassState: clearAllStateFlag.! !

