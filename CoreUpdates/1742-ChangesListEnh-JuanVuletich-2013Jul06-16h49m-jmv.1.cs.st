'From Cuis 4.1 of 12 December 2012 [latest update: #1741] on 6 July 2013 at 5:02:57.6451 pm'!

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 7/6/2013 16:53'!
contentsDiffedFromCurrent
	"Answer the contents diffed forward from current (in-memory) method version"

	|  aChange aClass  name aSelector |
	listIndex = 0
		ifTrue: [^ ''].
	aChange _ changeList at: listIndex.
	((aChange changeType == #method 
			and: [(aClass _ aChange changeClass) notNil]) 
			and: [aClass includesSelector: aChange methodSelector]) ifTrue: [
		aSelector _ aChange methodSelector.
		(aClass notNil and: [aClass includesSelector: aSelector])
			ifFalse: [ ^aChange text copy ].
		^DifferenceFinder
				displayPatchFrom: (aClass sourceCodeAt: aSelector)
				to: aChange text
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [aClass]) ].
	(aChange changeType == #classDefinition and: [
			name _ aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass _ Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass _ aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange text tryWords: true].
	(aChange changeType == #classComment
			and: [(aClass _ aChange changeClass) notNil]) ifTrue: [
		^DifferenceFinder
				displayPatchFrom: aClass comment to: aChange text tryWords: self shouldDiffWords].
	^(changeList at: listIndex) text! !

!ChangeList methodsFor: 'shout styling' stamp: 'jmv 7/6/2013 17:01'!
shoutAboutToStyle: aSHTextStyler
	"This is a notification that aSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in aSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) changeType = #method ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				aSHTextStyler classOrMetaClass: cl.
				^true ]]].
	^false! !

