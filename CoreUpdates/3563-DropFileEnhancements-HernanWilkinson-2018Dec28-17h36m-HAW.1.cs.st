'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 29 December 2018 at 11:17:58 am'!
!classDefinition: #SimpleServiceEntry category: #'Tools-FileList'!
ActiveModel subclass: #SimpleServiceEntry
	instanceVariableNames: 'provider label selector useLineAfter stateSelector description argumentGetter buttonLabel argumentProvider triggerFileListChanged sortOrder icon '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!
!classDefinition: #DropFilesAction category: #'Morphic-Worlds'!
Object subclass: #DropFilesAction
	instanceVariableNames: 'shouldAskForCancel dropFilesEvent cancelBlock stopHereBlock selectedFileEntry shouldAskToStop '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!SimpleServiceEntry methodsFor: 'accessing' stamp: 'HAW 12/29/2018 09:55:38'!
icon

	^icon ! !

!SimpleServiceEntry methodsFor: 'accessing' stamp: 'HAW 12/29/2018 09:55:29'!
icon: anIcon

	icon := anIcon ! !


!SimpleServiceEntry class methodsFor: 'instance creation' stamp: 'HAW 12/29/2018 10:21:57'!
provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel icon: anIcon
	
	^ (self provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel)
		icon: anIcon;
		yourself! !

!SimpleServiceEntry class methodsFor: 'instance creation' stamp: 'HAW 12/29/2018 10:21:33'!
provider: anObject label: aString selector: aSymbol description: anotherString icon: anIcon

	^(self provider: anObject label: aString selector: aSymbol description: anotherString)
		icon: anIcon;
		yourself! !


!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/29/2018 11:14:32'!
createMenuFor: options

	| icons lines labels |

	"options is a small collection, no problem to collect twice - Hernan"	
	labels := options collect: [ :option | option label ].
	icons := options collect: [ :option | option icon ].

	shouldAskToStop 
		ifTrue: [
			lines := Array with: labels size.
			labels add: 'stop here'.
			icons add: #cancelIcon ]
		ifFalse: [ lines := #() ].
		
	^PopUpMenu labelArray: labels lines: lines icons: icons! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/29/2018 11:06:41'!
performService: aService

	aService 
		argumentProvider: self;
		performService 
! !

!DropFilesAction methodsFor: 'FileList protocol' stamp: 'HAW 12/28/2018 20:31:41'!
fullName
	
	^selectedFileEntry name ! !

!DropFilesAction methodsFor: 'FileList protocol' stamp: 'HAW 12/28/2018 20:26:43'!
selectedFileEntry
	
	^selectedFileEntry! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'HAW 12/29/2018 10:06:58'!
serviceBrowseCode
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse code'
		selector: #browseCode:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code'
		icon: #editFindReplaceIcon)
			sortOrder: 10;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'HAW 12/29/2018 10:07:24'!
serviceBrowsePackage
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'browse package'
		selector: #browsePackage:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser'
		icon: #editFindReplaceIcon)
			sortOrder: 10;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'HAW 12/29/2018 10:01:13'!
serviceContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'see code file contents'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents'
		icon: #changesIcon)
			sortOrder: 20;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'HAW 12/29/2018 09:59:06'!
servicePackageContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'see package file contents'
		selector: #browsePackageContents:
		description: 'open a code file contents tool on this package file'
		buttonLabel: 'pck contents'
		icon: #changesIcon)
			sortOrder: 20;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'HAW 12/29/2018 10:01:37'!
serviceRecentChanges
	"Answer a service for opening a changelist browser on the tail end of a .changes file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'see recent changes in file'
		selector: #browseRecentLogOn:
		description: 'open a changelist tool on recent changes in file'
		buttonLabel: 'recent changes'
		icon: #changesIcon! !


!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:23:22'!
serviceAddNewDirectory
	"Answer a service entry characterizing the 'add new directory' command"

	^ SimpleServiceEntry 
		provider: self 
		label: 'add new directory' 
		selector: #addNewDirectory
		description: 'adds a new, empty directory (folder)' 
		icon: #listAddIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:42:05'!
serviceAddNewFile
	"Answer a service entry characterizing the 'add new file' command"

	^ SimpleServiceEntry 
		provider: self 
		label: 'add new file' 
		selector: #addNewFile 
		description: 'create a new,. empty file, and add it to the current directory.'
		icon: #newIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:24:39'!
serviceCopyName

	^ SimpleServiceEntry 
		provider: self 
		label: 'copy name to clipboard' 
		selector: #copyName 
		description:'copy name to clipboard' 
		icon: #copyIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:26:03'!
serviceDeleteFile

	^ SimpleServiceEntry 
		provider: self 
		label: 'delete' 
		selector: #deleteFile
		description: 'delete the seleted item'
		icon: #deleteIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:41:24'!
serviceGet
	"Answer a service for getting the entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'get entire file' 
		selector: #get
		description: 'if the file has only been partially read in, because it is very large, read the entire file in at this time.'
		icon: #textEditorIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:40:08'!
serviceGetHex

	^ SimpleServiceEntry 
		provider: self 
		label: 'view as hex' 
		selector: #getHex
		description: 'view as hex'
		icon: #fontXGenericIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:27:40'!
serviceRenameFile

	^ SimpleServiceEntry 
		provider: self 
		label: 'rename' 
		selector: #renameFile 
		description: 'rename file'
		icon: #saveAsIcon! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:31:06'!
serviceSortByDate
	"Answer a service for sorting by date"
	| buttonLabel |
	buttonLabel _ sortMode = #date
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - date' ]
				ifFalse: [ '[v] - date' ]]
		ifFalse: [ 'date' ].
	^  (SimpleServiceEntry 
			provider: self 
			label: 'by date' 
			selector: #sortByDate 
			description: 'sort entries by date'
			icon: #sendReceiveIcon)
		extraSelector: #sortingByDate;
		buttonLabel: buttonLabel! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:31:00'!
serviceSortByName
	"Answer a service for soring by name"
	| buttonLabel |
	buttonLabel _ sortMode = #name
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - name' ]
				ifFalse: [ '[v] - name' ]]
		ifFalse: [ 'name' ].
	^ (SimpleServiceEntry 
		provider: self 
		label: 'by name' 
		selector: #sortByName 
		description: 'sort entries by name'
		icon: #sendReceiveIcon)
			extraSelector: #sortingByName;
			buttonLabel: buttonLabel! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:29:42'!
serviceSortBySize
	"Answer a service for sorting by size"
	| buttonLabel |
	buttonLabel _ sortMode = #size
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - size' ]
				ifFalse: [ '[v] - size' ]]
		ifFalse: [ 'size' ].
	^ (SimpleServiceEntry
		provider: self
		label: 'by size'
		selector: #sortBySize
		description: 'sort entries by size'
		icon: #sendReceiveIcon)
			extraSelector: #sortingBySize;
			buttonLabel: buttonLabel.! !

!FileList methodsFor: 'own services' stamp: 'HAW 12/29/2018 10:33:04'!
serviceViewContentsInWorkspace
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ SimpleServiceEntry 
		provider: self 
		label: 'workspace with contents' 
		selector: #viewContentsInWorkspace
		description: 'open a new Workspace whose contents are set to the contents of this file'
		icon: #terminalIcon! !


!SimpleServiceEntry methodsFor: 'services menu' stamp: 'HAW 12/29/2018 10:20:15'!
addServiceFor: served toMenu: aMenu
	argumentProvider _ served.
	aMenu 
		add: self label 
		target: self 
		action: #performService
		icon: icon.
	self useLineAfter ifTrue: [ aMenu addLine ].! !

!SimpleServiceEntry methodsFor: 'initialization' stamp: 'HAW 12/29/2018 10:20:39'!
initialize

	triggerFileListChanged _ false.
	sortOrder _ 1.
	icon _ nil.! !


!DropFilesAction methodsFor: 'initialization' stamp: 'HAW 12/29/2018 11:14:32'!
initializeFor: aDropFilesEvent 

	dropFilesEvent := aDropFilesEvent.
	shouldAskToStop := aDropFilesEvent numberOfFiles > 1! !

!DropFilesAction methodsFor: 'evaluating' stamp: 'HAW 12/29/2018 11:14:49'!
value

	stopHereBlock := [ ^self ].
	dropFilesEvent fileNamesDo: [ :fileName | self fileNamedDropped: fileName ]
		
! !

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/29/2018 11:15:10'!
fileNamedDropped: aFileName 

	| options selectionIndex menu |
	
	selectedFileEntry := aFileName asFileEntry.
	options := FileList itemsForFile: aFileName.
	options isEmpty ifTrue: [ ^self inform: 'No action found for ', selectedFileEntry name ].
	menu := self createMenuFor: options.
		
	selectionIndex := menu startUpWithCaption: 'Select action for ', selectedFileEntry name.
	
	selectionIndex = 0 ifTrue: [ ^self ].
	(options isInBounds: selectionIndex) ifTrue: [ ^self performService: (options at: selectionIndex) ].
	"The only available option is 'stop here'. This could change if #createMenuFor: changes - Hernan"
	stopHereBlock value
	
! !


!ChangeSet class methodsFor: 'file list services' stamp: 'HAW 12/29/2018 10:04:54'!
serviceFileIn
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'file in'
		selector: #fileIn:
		description: 'file in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'filein'
		icon: #saveIcon)
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ChangeSet class methodsFor: 'file list services' stamp: 'HAW 12/29/2018 10:05:49'!
serviceInstall
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install change set'
		buttonLabel: 'install'
		icon: #saveIcon)
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'HAW 12/29/2018 10:16:53'!
serviceInstallPackage
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackage:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package'
		icon: #saveIcon)
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!methodRemoval: DropFilesAction #ask:onYes:!
DropFilesAction removeSelector: #ask:onYes:!
!methodRemoval: DropFilesAction #canBeFiledIn:!
DropFilesAction removeSelector: #canBeFiledIn:!
!methodRemoval: DropFilesAction #canBeInstalled:!
DropFilesAction removeSelector: #canBeInstalled:!
!methodRemoval: DropFilesAction #ifUserWantsFileIn:!
DropFilesAction removeSelector: #ifUserWantsFileIn:!
!methodRemoval: DropFilesAction #ifUserWantsInstall:!
DropFilesAction removeSelector: #ifUserWantsInstall:!
!methodRemoval: DropFilesAction #isAChangeSet:!
DropFilesAction removeSelector: #isAChangeSet:!
!methodRemoval: DropFilesAction #isChangeSet:!
DropFilesAction removeSelector: #isChangeSet:!
!classDefinition: #SimpleServiceEntry category: #'Tools-FileList'!
ActiveModel subclass: #SimpleServiceEntry
	instanceVariableNames: 'provider label selector useLineAfter stateSelector description argumentGetter buttonLabel argumentProvider triggerFileListChanged sortOrder icon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!
!classDefinition: #DropFilesAction category: #'Morphic-Worlds'!
Object subclass: #DropFilesAction
	instanceVariableNames: 'dropFilesEvent selectedFileEntry shouldAskToStop stopHereBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!DropFilesAction reorganize!
('initialization' initializeFor:)
('evaluating' value)
('evaluating - private' createMenuFor: fileNamedDropped: performService:)
('FileList protocol' fullName selectedFileEntry)
!

