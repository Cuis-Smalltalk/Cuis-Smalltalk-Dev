'From Cuis 4.1 of 12 December 2012 [latest update: #1747] on 9 July 2013 at 6:16:55.606113 pm'!

!CodeFileBrowser methodsFor: 'accessing' stamp: 'jmv 7/9/2013 18:15'!
acceptedStringOrText
	self updateInfoView.
	(editSelection == #newClass and: [ codeFile notNil ])
		ifTrue: [ ^codeFile description ].
	editSelection == #editClass
		ifTrue:[ ^self modifiedClassDefinition ].
	^super acceptedStringOrText! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'jmv 7/9/2013 18:16'!
removeClass
	| class |
	selectedClassName ifNil: [ ^self ].
	class _ self selectedClass.
	(self confirm:'Are you certain that you
want to delete the class ', class name, '?') ifFalse:[^self].
	codeFile removeClass: class.
	self classListIndex: 0.
	self changed: #classList.! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'jmv 7/9/2013 18:15'!
classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^(selectedSystemCategory isNil or: [ codeFile isNil ])
		ifTrue: [ #() ]
		ifFalse: [ codeFile classes keys sort ]! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'jmv 7/9/2013 18:16'!
renameClass
	| oldName newName |
	selectedClassName ifNil: [ ^self ].
	oldName _ self selectedClass name.
	newName _ (self request: 'Please type new class name'
						initialAnswer: oldName) asSymbol.
	(newName isEmpty or:[newName = oldName]) ifTrue: [^ self].
	(codeFile classes includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists in the CodeFile'].
	systemOrganizer classify: newName under: selectedSystemCategory.
	systemOrganizer removeElement: oldName.
	codeFile renameClass: self selectedClass to: newName.
	self changed: #classList.
	self classListIndex: ((systemOrganizer listAtCategoryNamed: selectedSystemCategory) indexOf: newName).
! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'jmv 7/9/2013 18:16'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^self selectedClassName ifNotNil: [ :scn |
		codeFile classAt: scn ]! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 7/9/2013 18:15'!
fileIn
	codeFile fileIn! !

!CodeFileBrowser methodsFor: 'fileIn/fileOut' stamp: 'jmv 7/9/2013 18:15'!
fileOut

	codeFile fileOut! !

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

