'From Cuis7.1 [latest update: #6584] on 13 August 2024 at 3:58:11 pm'!

!ChangeList class methodsFor: 'public access' stamp: 'jmv 8/13/2024 15:26:31'!
browseCondensedChanges: aFileEntry
	"Opens a changeList on a fileStream
	Condense older versions. Remove doits."
	| changeList |
	changeList := self new.
	aFileEntry readStreamDo: [ :stream |
		changeList scanFile: stream from: 0 to: stream size ].
	changeList
		removeOlderMethodVersions;
		removeDoIts.
	ChangeListWindow open: changeList label: 'Condensed changes from: ', aFileEntry name.! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 8/13/2024 15:22:12'!
serviceCondensedChanges
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'open condensed changes'
		selector: #browseCondensedChanges:
		description: 'open a changes tool on this file, including only the last version of methods and avoiding doits'
		buttonLabel: 'real changes'
		icon: #changesIcon)
			sortOrder: 20! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 8/13/2024 15:18:54'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	| coda sampleCoda |
	(#('st' 'cs' 'cs.st') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].


	suffix = 'changes'
		ifTrue: [
			sampleCoda := '.user.159.changes'.
			filename name size > sampleCoda size ifTrue: [
				coda := filename name last: sampleCoda size.
				((coda beginsWith: '.user.') and: [ coda endsWith: '.changes' ]) ifTrue: [
					^ { self serviceContents. self serviceCondensedChanges} ]].
			^ {self serviceRecentChanges} ].

	^#()! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 8/13/2024 15:21:32'!
serviceContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'open code changes'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'all changes'
		icon: #changesIcon)
			sortOrder: 20! !

