'From Cuis7.9 [latest update: #8112] on 30 July 2026 at 10:44:39 am'!

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 30/Jul/2026, 1:30:18 pm (UTC)'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowsePackage } ].
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ]) ifTrue: [
		^ { self serviceBrowsePackage } ].

	(#('st' 'cs' 'cs.st' 'mth' 'cls') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	^#()! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 30/Jul/2026, 1:30:46 pm (UTC)'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	| coda sampleCoda |

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ])
		ifTrue: [ ^ {self servicePackageContents} ].

	(#('st' 'cs' 'cs.st' 'mth' 'cls') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	suffix = 'changes'
		ifTrue: [
			sampleCoda := '.user.159.changes'.
			aFileEntry name size > sampleCoda size ifTrue: [
				coda := aFileEntry name last: sampleCoda size.
				((coda beginsWith: '.user.') and: [ coda endsWith: '.changes' ]) ifTrue: [
					^ { self serviceContents. self serviceCondensedChanges} ]].
			^ {self serviceRecentChanges} ].

	^#()! !


!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 30/Jul/2026, 1:31:11 pm (UTC)'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"These would better be done by ChangeSorter!!"

	"See implementor at CodePackageFile class"
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ])
		ifTrue: [ ^ #() ].

	(#('cs' 'cs.st') includes: suffix) ifTrue: [
		^{self serviceInstall} ].

	(#('st' 'mth' 'cls') includes: suffix) ifTrue: [
		^{self serviceFileIn} ].

	^#()! !

!methodRemoval: FileList #isFileSmalltalkCode stamp: 'jmv 30/Jul/2026, 1:42:28 pm (UTC)'!
FileList removeSelector: #isFileSmalltalkCode!
