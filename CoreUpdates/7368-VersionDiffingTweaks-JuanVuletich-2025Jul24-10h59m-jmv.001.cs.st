'From Cuis7.5 [latest update: #7367] on 24 July 2025 at 11:00:14 am'!

!CodeFileBrowser methodsFor: 'diffs' stamp: 'jmv 7/24/2025 10:59:57'!
methodDiffFor: aString selector: selector
	"Answer the diff between the current copy of the given class/selector/meta for the string provided"
	| theClass base |
	base := ''.
	theClass := self pvtBaseClassOrMetaclass.
	theClass ifNotNil: [
		(theClass includesSelector: selector) ifTrue: [
			base := theClass sourceCodeAt: selector ]].
	^ DifferenceFinder
		displayPatchFrom: base
		to: aString
		tryWords: self shouldDiffWords
		prettyPrintedIn:
			(self showingAnyKindOfPrettyDiffs ifTrue: [ theClass ]).! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 7/24/2025 11:00:00'!
compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| base class baseString caseString differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	base := changeList at: listIndex.
	((class := base changeClass) notNil
			and: [class includesSelector: base methodSelector])
		ifTrue: [
			baseString := base string.
			caseString := (class sourceCodeAt: base methodSelector) asPlainString.
			baseString = caseString
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords := self shouldDiffWords.
			differDesc := diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: baseString to: caseString
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: (Preferences at: #standardCodeFont)))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !


!VersionsBrowser methodsFor: 'menu' stamp: 'jmv 7/24/2025 11:00:03'!
compareToOtherVersion
	"Prompt the user for a reference version, then spawn a window 
	showing the diffs between the older and the newer of the current 
	version and the reference version as text."

	| base case baseString caseString differDesc diffWords targetChanges labels label1 |
	base := changeList at: listIndex ifAbsent: [ ^self ].
	label1 := list at: listIndex.

	labels := list reject: [ :any | any = label1 ].
	targetChanges := changeList reject: [ :any | any = base ].
	case := ((SelectionMenu
				labels: labels
				selections: targetChanges) startUpMenu) ifNil: [ ^self ].
	
	"compare earlier -> later"
	"change1 timeStamp < change2 timeStamp
		ifFalse: [ | temp | temp := change1. change1 := change2. change2 := temp ]."

	baseString := base string.
	caseString := case string.
	baseString = caseString
		ifTrue: [^ self inform: 'Exact Match'].

	diffWords := self shouldDiffWords.
	differDesc := diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].

	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: baseString to: caseString
			tryWords: diffWords
			prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [classOfMethod])))
		openLabel: 
			(('Comparison from {1} to {2}: ', differDesc, 
				(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])) 
					format: { base stamp. case stamp })! !

