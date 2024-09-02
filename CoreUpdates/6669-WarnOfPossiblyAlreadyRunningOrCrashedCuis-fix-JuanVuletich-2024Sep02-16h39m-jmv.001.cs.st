'From Cuis7.1 [latest update: #6668] on 2 September 2024 at 4:39:39 pm'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/2/2024 16:39:15'!
checkIfAlreadyRunningOrStoppedNoExit
	"
	If the Changes file is not clean during startup, it may mean either:
	- last run crashed or was killed (code recovery may be in order)
	- Cuis is already running. Running it again may mean inconsistent updates to .changes file.
	Smalltalk checkIfAlreadyRunningOrStoppedNoExit
	"
	| imageSaveChunk postImageSaveChunk |
	imageSaveChunk := nil.
	postImageSaveChunk := nil.
	self withChangesFileDo: [ :changesFile |
		changesFile position: self lastQuitLogPosition.
		changesFile atEnd ifFalse: [ imageSaveChunk := changesFile nextChunk ].
		changesFile atEnd ifFalse: [ postImageSaveChunk := changesFile nextChunk ]].

	"If we can't find the image save chunk at the image save position, something is wrong."
	(imageSaveChunk isNil or: [ ((Smalltalk isQuitRecord: imageSaveChunk		) or: [
			Smalltalk isSnapshotRecord: imageSaveChunk ]) not])
		ifTrue: [
			UISupervisor whenUIinSafeState: [
				self notify:
					self currentChangesName asFullFileEntry pathName, String newLineString,
					'Image save chunk not found. Possible image / changes file corruption. Proceed with caution.' ].
				^self ].

	"If there is already a post-imageSave chunk, it may mean Cuis is running in another OS process,
	or in a previous run, it was killed or crashed."
	postImageSaveChunk notNil ifTrue: [
		(Preferences at: #checkLostChangesOnStartUp)
			ifTrue: [
				"Let the user chose automatic or manual recovery from .changes file.
				Used mainly by CuisUniversity."
				UISupervisor whenUIinSafeState: [self restoreLostChanges ]]
			ifFalse: [
				"Notify user about the situation, open changes recovery tool on last User Changes file."
				(Preferences at: #warnAndOfferLastUserChanges) ifTrue: [
					UISupervisor whenUIinSafeState: [self warnAndOfferLastUserChanges]]]].! !

