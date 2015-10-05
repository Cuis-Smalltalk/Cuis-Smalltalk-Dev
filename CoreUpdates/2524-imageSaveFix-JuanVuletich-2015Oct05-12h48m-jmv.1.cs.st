'From Cuis 4.2 of 25 July 2013 [latest update: #2523] on 5 October 2015 at 12:48:22.600242 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/5/2015 12:48'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FmFileIOAccessor default copy: oldChangesName to: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !

