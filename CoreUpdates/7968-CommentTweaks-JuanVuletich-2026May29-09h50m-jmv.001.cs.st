'From Cuis7.7 [latest update: #7967] on 29 May 2026 at 9:51:33 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 28/May/2026 14:40:43'!
saveAndQuitAs: newName clearAllClassState: clearAllStateFlag
	"Save the image under a new name. Then quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName := self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFullFileEntry to: newChangesName asFullFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFullFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	FontFamily releaseClassCachedState.
	self garbageCollect.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: true embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 28/May/2026 14:43:05'!
saveAndStayAs: newName clearAllClassState: clearAllStateFlag
	"Save the image under a new name. Don't quit.
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

