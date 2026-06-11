'From Cuis7.9 [latest update: #7997] on 11 June 2026 at 2:55:53 pm'!

!ImageReadWriter class methodsFor: 'instance creation' stamp: 'jmv 11/Jun/2026 14:49:42'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that read a TrueType font on contents."

	( self allTypicalFileExtensions includes: suffix) ifTrue: [
		^ { self serviceReadImage } ].

	^#()! !


!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 11/Jun/2026 14:49:45'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that read a TrueType font on contents."

	(#( 'ttf' 'ttc') includes: suffix) ifTrue: [
		^ { self serviceReadTTFont } ].

	^#()! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 11/Jun/2026 14:53:26'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowsePackage } ].
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ]) ifTrue: [
		^ { self serviceBrowsePackage } ].

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	^#()! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 11/Jun/2026 14:52:39'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	| coda sampleCoda |

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ])
		ifTrue: [ ^ {self servicePackageContents} ].

	(#('st' 'cs' 'cs.st') includes: suffix)
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


!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 11/Jun/2026 14:54:52'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"These would better be done by ChangeSorter!!"

	"See implementor at CodePackageFile class"
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ])
		ifTrue: [ ^ #() ].

	(#('cs' 'cs.st') includes: suffix) ifTrue: [
		^{self serviceInstall} ].

	(#('st') includes: suffix) ifTrue: [
		^{self serviceFileIn} ].

	^#()! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 11/Jun/2026 14:48:45'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that are serviced by us."

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceInstallPackage } ].
	(suffix = 'st' and: [
			'*pck.*.st' match: aFileEntry name ])
		ifTrue: [ ^ { self serviceInstallPackage } ].
	^#()! !

