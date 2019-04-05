'From Cuis 5.0 of 7 November 2016 [latest update: #3692] on 5 April 2019 at 5:52:57 pm'!

!CodeFileBrowser methodsFor: 'infoView' stamp: 'KenD 3/30/2019 02:10:13'!
extraInfo
	^ (self
		methodDiffFor: (self selectedClassOrMetaClass sourceCodeAt: self selectedMessageName)
		class: self selectedClass
		selector: self selectedMessageName
		meta: self metaClassIndicated) hasAnyAttribute
			ifTrue: [' - **MODIFIED**']
			ifFalse: [' - identical']! !

!CodeFileBrowser methodsFor: 'infoView' stamp: 'KenD 3/30/2019 02:11:17'!
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
		ifFalse: ['**NEW** Method not in the system']! !

