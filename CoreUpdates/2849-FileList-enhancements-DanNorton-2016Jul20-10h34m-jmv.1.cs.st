'From Cuis 4.2 of 25 July 2013 [latest update: #2848] on 20 July 2016 at 10:41:17 am'!

!FileList methodsFor: 'file list' stamp: 'dhn 6/20/2016 17:57'!
fileListIndex: anInteger
	"Select the file name having the given index, and display its contents."

	listIndex _ anInteger.
	fileName _ listIndex = 0
		"open the file selected"
		ifFalse: [
			self fileNameFromFormattedItem: (list at: anInteger)].

	brevityState _ #needToGetBrief.
	self changed: #fileListIndex.
	self acceptedContentsChanged.
	self triggerEvent: #updateButtonRow! !

!FileList methodsFor: 'file menu action' stamp: 'dhn 6/20/2016 17:57'!
renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	(response _ FillInTheBlankMorph request: 'NewFileName?'
 					initialAnswer: fileName)
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	newName = fileName ifTrue: [^ self].
	directory // fileName rename: newName.
	self updateFileList.
	listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
	listIndex > 0 ifTrue: [fileName _ newName].
	self changed: #fileListIndex.
	self triggerEvent: #updateButtonRow
! !

!FileList methodsFor: 'private' stamp: 'dhn 6/20/2016 17:57'!
resort: newMode
	"Re-sort the list of files."

	| name |
	listIndex > 0
		ifTrue: [name _ self fileNameFromFormattedItem: (list at: listIndex)].
	sortAscending _ sortMode = newMode
		ifTrue: [ sortAscending not ]
		ifFalse: [ newMode = #name].
	sortMode _ newMode.
	self pattern: pattern.
	name ifNotNil: [
		fileName _ name.
		listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = name. ].
		self changed: #fileListIndex].
	listIndex = 0 ifTrue: [
		self acceptedContentsChanged ].
	self triggerEvent: #updateButtonRow
! !

!FileList methodsFor: 'private' stamp: 'jmv 7/20/2016 10:34:35'!
setSelectedDirectoryTo: aFileDirectoryWrapper
	currentDirectorySelected _ aFileDirectoryWrapper.
	self directory: aFileDirectoryWrapper withoutListWrapper.
	brevityState := #FileList.
	self changed: #fileList.
	self acceptedContentsChanged.
	self changed: #currentDirectorySelected! !

!FileList methodsFor: 'private' stamp: 'dhn 6/20/2016 17:57'!
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns patternsThatReject patternsThatSelect |
	patterns _ OrderedCollection new.
	(pattern findTokens: (String with: Character cr with: Character lf with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $?)
				ifTrue: [ patterns add: (each copyReplaceAll: '?' with: '#')]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].
	"A pattern that starts with $/ is used to reject entries
	Rejecting patterns are applied after selecting patterns."
	patternsThatSelect _ patterns reject: [ :any | any first = $/ ].
	patternsThatSelect isEmpty ifTrue: [ patternsThatSelect add: '*' ]. 
	patternsThatReject _ patterns select: [ :any | any first = $/ ] thenCollect: [ :each | each copyFrom: 2 to: each size ].
	list _ self listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject.
	listIndex _ 0.
	fileName _ nil.
	acceptedContentsCache _ ''.
	self changed: #fileList.
	self triggerEvent: #updateButtonRow! !


!SimpleServiceEntry methodsFor: 'performing service' stamp: 'jmv 7/20/2016 10:39:22'!
performService
	"carry out the service I provide"

	selector numArgs = 0
		ifTrue: [ provider perform: selector ]
		ifFalse: [ provider perform: selector with: (self getArgumentsFrom: argumentProvider) ].
	triggerFileListChanged ifTrue: [
		self triggerEvent: #fileListChanged ]! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 7/20/2016 10:39:37'!
updateButtonRow: row
	"Dynamically update the contents of the button row, if any."

	| buttons |
	row removeAllMorphs.
	buttons _ OrderedCollection new.
	model universalButtonServices do: [ :service |
		buttons add: (self buttonToTriggerIn: model for: service) ].
	model dynamicButtonServices do: [ :service | 
		buttons add: (self buttonToTriggerIn: model for: service).
		service when: #fileListChanged send: #updateFileList to: model ].
	row addMorphs: buttons! !

!FileListWindow methodsFor: 'menu building' stamp: 'jmv 7/20/2016 10:40:02'!
fileSelectedMenu

	| itemsPart1 itemsPart2 itemsPart3 itemsPart4 n1 n2 n3 services aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	itemsPart1 _ model itemsForAnyFile1.
	itemsPart2 _ model itemsForFile: model fileName.
	itemsPart3 _ model itemsForAnyFile2.
	itemsPart4 _ model itemsForNoFile.
	n1 _ itemsPart1 size.
	n2 _ n1 + itemsPart2 size.
	n3 _ n2 + itemsPart3 size.
	services _ itemsPart1, itemsPart2, itemsPart3, itemsPart4.
	services do: [ :svc | svc when: #fileListChanged send: #updateFileList to: model ].
	^ aMenu 
		addServices: services 
		for: model
		extraLines:{ n1 . n2 . n3 }
! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'dhn 6/20/2016 17:56'!
openFileList
	"
	FileListWindow openFileList
	"
	| win |
	
	win _ FileListWindow open: (FileList new directory: DirectoryEntry currentDirectory) label: nil.
	win model when: #updateButtonRow send: #updateButtonRow to: win.
	^ win! !

!methodRemoval: FileListWindow #update:!
FileListWindow removeSelector: #update:!
!methodRemoval: FileList #update:!
FileList removeSelector: #update:!
