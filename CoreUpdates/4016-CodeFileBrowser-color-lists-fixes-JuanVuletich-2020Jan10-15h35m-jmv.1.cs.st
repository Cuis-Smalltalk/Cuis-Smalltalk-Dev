'From Cuis 5.0 [latest update: #4014] on 10 January 2020 at 3:47:01 pm'!

!CodeFileBrowser methodsFor: 'diffs' stamp: 'jmv 1/10/2020 15:39:10'!
methodDiffFor: aString selector: selector
	"Answer the diff between the current copy of the given class/selector/meta for the string provided"
	| theClass source |
	source _ ''.
	theClass _ self pvtBaseClassOrMetaclass.
	theClass ifNotNil: [
		(theClass includesSelector: selector) ifTrue: [
			source _ theClass sourceCodeAt: selector ]].
	^ DifferenceFinder
		displayPatchFrom: source
		to: aString
		tryWords: self shouldDiffWords
		prettyPrintedIn:
			(self showingAnyKindOfPrettyDiffs ifTrue: [ theClass ]).! !


!PseudoClassOrganizer methodsFor: 'testing' stamp: 'jmv 1/10/2020 15:29:44'!
isRemoved: aSelector
	^(self categoryOfElement: aSelector) = PseudoClass removedCategoryName! !


!CodeFileBrowser methodsFor: 'edit pane' stamp: 'jmv 1/10/2020 15:41:21'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	answer _ class sourceCodeAt: selector.
	(self classOrMetaClassOrganizer isRemoved: selector) ifTrue: [
		^ Text
			string: answer
			attribute: TextColor red ].
	Preferences browseWithPrettyPrint ifTrue: [
		answer _ class compilerClass new
						format: answer in: class notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		answer _ self
			methodDiffFor: answer
			selector: self selectedMessageName ].
	^ answer! !

!CodeFileBrowser methodsFor: 'infoView' stamp: 'jmv 1/10/2020 15:38:07'!
extraInfo
	^ (self
		methodDiffFor: (self selectedClassOrMetaClass sourceCodeAt: self selectedMessageName)
		selector: self selectedMessageName)
			hasAnyAttribute
				ifTrue: [' - **MODIFIED**']
				ifFalse: [' - identical']! !

!CodeFileBrowser methodsFor: 'infoView' stamp: 'jmv 1/10/2020 15:33:10'!
infoViewContents
	| theClass selector useLabel |
	useLabel _ self baseCodeSource baseLabel.
	editSelection == #newClass ifTrue: [
		^ caseCodeSource
			ifNil: [ 'No file selected' ]
			ifNotNil: [ caseCodeSource summary ]].
	self selectedClass ifNil: [ ^ '' ].
	theClass _ self pvtBaseClassOrMetaclass.
	editSelection == #editClass ifTrue: [
		^ theClass
			ifNil: [ 'Class not in the ' , useLabel ]
			ifNotNil: [ 'Class exists already in the ' , useLabel ]].
	editSelection == #editMessage ifFalse: [ ^ '' ].
	selector _ self selectedMessageName.
	^ (theClass notNil and: [ theClass includesSelector: selector ])
		ifTrue: [ 'Method already exists' , self extraInfo ]
		ifFalse: [			
			(self classOrMetaClassOrganizer isRemoved: selector)
				ifTrue: [ 'Method not in the ' , useLabel ]
				ifFalse: [ '**NEW** Method not in the ' , useLabel ]]! !

!CodeFileBrowser methodsFor: 'message list' stamp: 'jmv 1/10/2020 15:30:55'!
messageList
	"Colorize messages as needed"
	^ super messageList collect: [ :eaListItem | | useAttr |
		useAttr _ (self classOrMetaClassOrganizer isRemoved: eaListItem)
			ifTrue: [ TextColor red ]
			ifFalse: [ | baseSrc |
				baseSrc _ self pvtBaseSelectedMessageSourceCodeFor: eaListItem.
				baseSrc
					ifNil: [ TextColor green ]
					ifNotNil: [ | caseSrc |
						caseSrc _ self pvtCaseSelectedMessageSourceCodeFor: eaListItem.
						baseSrc = caseSrc ifFalse: [ TextColor blue ]]].
		useAttr
			ifNil: [ eaListItem ]
			ifNotNil: [ :attr |
				Text
					string: eaListItem
					attribute: attr ]].! !

!CodeFileBrowser methodsFor: 'private' stamp: 'jmv 1/10/2020 15:14:57'!
pvtBaseSelectedMessageSourceCodeFor: selector
	^ self pvtBaseClassOrMetaclass ifNotNil: [ :theClass |
		(theClass includesSelector: selector) ifTrue: [ theClass sourceCodeAt: selector ]].! !

!methodRemoval: CodeFileBrowser #methodDiffFor:class:selector:meta: stamp: 'jmv 1/10/2020 15:38:22'!
CodeFileBrowser removeSelector: #methodDiffFor:class:selector:meta:!
