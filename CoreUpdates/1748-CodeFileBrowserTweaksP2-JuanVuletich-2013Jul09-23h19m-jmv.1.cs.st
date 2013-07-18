'From Cuis 4.1 of 12 December 2012 [latest update: #1747] on 9 July 2013 at 11:36:03.07291 pm'!

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 22:46'!
browsePackage: aStream

	CodeFileBrowserWindow browsePackageStream: aStream named: aStream name! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 22:46'!
serviceBrowsePackage
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'package-file browser'
		selector: #browsePackage:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser')
			argumentGetter: [ :fileList | fileList readOnlyStream ]! !


!CodeFile methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:48'!
summary
	| nClasses newClasses oldClasses |
	nClasses := newClasses := oldClasses := 0.
	classes do:[:cls|
		nClasses := nClasses + 1.
		(Smalltalk includesKey: (cls name asSymbol))
			ifTrue: [ oldClasses := oldClasses + 1]
			ifFalse: [ newClasses := newClasses + 1]].
	^nClasses printString,' classes (', newClasses printString, ' new / ', oldClasses printString, ' modified)'! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 7/7/2013 22:47'!
browsePackageStream: aStream named: aString

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	codeFile _ (CodePackageFile new fullName: aString; buildFrom: aStream).
	aStream close.
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	CodeFileBrowserWindow open: browser label: 'Code Package File Browser'! !


!CodeFileBrowser methodsFor: 'infoView' stamp: 'jmv 7/9/2013 18:16'!
infoViewContents
	| theClass |
	editSelection == #newClass ifTrue: [
		^codeFile
			ifNil: [ 'No file selected' ]
			ifNotNil: [ codeFile summary ]].
	self selectedClass ifNil: [^ ''].
	theClass _ Smalltalk at: self selectedClass name asSymbol ifAbsent: nil.
	editSelection == #editClass ifTrue: [
		^ theClass
			ifNotNil: ['Class exists already in the system']
			ifNil: ['Class not in the system']].
	editSelection == #editMessage ifFalse: [^ ''].
	(theClass notNil and: [self metaClassIndicated])
		ifTrue: [theClass _ theClass class].
	^ (theClass notNil and: [theClass includesSelector: self selectedMessageName])
		ifTrue: ['Method already exists' , self extraInfo]
		ifFalse: ['Method not in the system']! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 22:42'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowsePackage } ].

	^#()! !

!methodRemoval: CodeFileBrowser #infoFor:!
CodeFileBrowser removeSelector: #infoFor:!
