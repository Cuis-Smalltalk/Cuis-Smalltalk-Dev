'From Cuis 4.2 of 25 July 2013 [latest update: #2111] on 30 November 2014 at 4:36:46.95323 pm'!

!VersionsBrowser methodsFor: 'menu' stamp: 'jmv 11/30/2014 16:35'!
compareToOtherVersion
	"Prompt the user for a reference version, then spawn a window 
	showing the diffs between the older and the newer of the current 
	version and the reference version as text."

	| change1 change2 s1 s2 differDesc diffWords targetChanges labels label1 |
	change1 _ changeList at: listIndex ifAbsent: [ ^self ].
	label1 _ list at: listIndex.

	labels _ list reject: [ :any | any = label1 ].
	targetChanges _ changeList reject: [ :any | any = change1 ].
	change2 _ ((SelectionMenu
				labels: labels
				selections: targetChanges) startUp) ifNil: [ ^self ].
	
	"compare earlier -> later"
	"change1 timeStamp < change2 timeStamp
		ifFalse: [ | temp | temp _ change1. change1 _ change2. change2 _ temp ]."

	s1 _ change1 string.
	s2 _ change2 string.
	s1 = s2
		ifTrue: [^ self inform: 'Exact Match'].

	diffWords _ self shouldDiffWords.
	differDesc _ diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].

	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: s1 to: s2
			tryWords: diffWords
			prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [classOfMethod])))
		openLabel: 
			(('Comparison from {1} to {2}: ', differDesc, 
				(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])) 
					format: { change1 stamp. change2 stamp })! !


!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 11/30/2014 16:23'!
makeRelative: path
	"Ensure that path looks like an relative path"
	^path first = self pathNameDelimiter
		ifTrue: [ path allButFirst ]
		ifFalse: [ path ]! !


!MacFileDirectory class methodsFor: 'platform specific' stamp: 'jmv 11/30/2014 16:23'!
makeAbsolute: path
	"Ensure that path looks like an absolute path"
	| absolutePath |
	(self isAbsolute: path)
		ifTrue: [ ^path ].
	"If a path begins with a colon, it is relative."
	absolutePath _ (path first = $:)
		ifTrue: [ path allButFirst ]
		ifFalse: [ path ].
	(self isAbsolute: absolutePath)
		ifTrue: [ ^absolutePath ].
	"Otherwise, if it contains a colon anywhere, it is absolute and the first component is the volume name."
	^absolutePath, ':'! !


!Theme methodsFor: 'icon lookup' stamp: 'jmv 11/30/2014 16:28'!
prepend: aTheme toContentSpec: anArray

	^ { aTheme printString}, anArray! !

!methodRemoval: OrderedCollection class #ofSize:!
OrderedCollection class removeSelector: #ofSize:!
!methodRemoval: SequenceableCollection #copyLast:!
SequenceableCollection removeSelector: #copyLast:!
!methodRemoval: SequenceableCollection #copyWithFirst:!
SequenceableCollection removeSelector: #copyWithFirst:!
!methodRemoval: SequenceableCollection #copyWithoutFirst!
SequenceableCollection removeSelector: #copyWithoutFirst!
!methodRemoval: SequenceableCollection #copyWithoutIndex:!
SequenceableCollection removeSelector: #copyWithoutIndex:!
!methodRemoval: Collection class #ofSize:!
Collection class removeSelector: #ofSize:!
