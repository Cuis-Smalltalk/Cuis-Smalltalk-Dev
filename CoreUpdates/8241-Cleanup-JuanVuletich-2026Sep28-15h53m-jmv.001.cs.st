'From Cuis7.9 [latest update: #8240] on 28 September 2026 at 3:54:38 pm'!

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 6:44:03 pm (UTC)'!
acceptedStringOrText
	"Answer the contents string, obeying diffing directives if needed"

	^ self showingAnyKindOfDiffs
		ifFalse: [
			self undiffedContents]
		ifTrue: [
			self contentsDiffedFromCurrent]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 28/Sep/2026, 6:42:20 pm (UTC)'!
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| method class |
	model selectedMessageName isNil ifTrue: [
		^self.
		"^ VersionsBrowserWindow
			browseCommentOf: model selectedClass" ].
	method := model currentCompiledMethod.
	"because maybe the method has been removed --- unhelpful"
	method isNil ifTrue: [^self].
	class := method methodClass.
	^ MethodVersionsBrowserWindow 
		browseVersionsOf: method
		class: class theNonMetaClass 
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 28/Sep/2026, 6:47:25 pm (UTC)'!
optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph 
					model: model
					action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	buttons add:  self prettyLineDiffButton.
	widths add: 21.
	buttons add:  self prettyWordDiffButton.
	widths add: 23.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !

!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 28/Sep/2026, 6:50:14 pm (UTC)'!
optionalModelButtonTuples

	^#(
		(11		'select all' 			selectAll			'select all entries')
		(14		'deselect all'			deselectAll			'deselect all entries')
		(19		'file in selections' 	fileInSelections		'file in all selected entries')
	)! !

!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 28/Sep/2026, 6:46:46 pm (UTC)'!
browseVersions

	| method class browser |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		model selectedClass ifNotNil: [ :selectedClass |
			^self.
			"^ VersionsBrowserWindow
				browseCommentOf: selectedClass" ]].
	class := method methodClass.
	browser := MethodVersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector.
	^browser! !

!methodRemoval: ChangeList #diffedVersionContents stamp: 'jmv 28/Sep/2026, 6:53:25 pm (UTC)'!
ChangeList removeSelector: #diffedVersionContents!
!methodRemoval: ChangeList #wantsPrettyDiffOption stamp: 'jmv 28/Sep/2026, 6:53:25 pm (UTC)'!
ChangeList removeSelector: #wantsPrettyDiffOption!
!methodRemoval: ChangeList #xtraVersion stamp: 'jmv 28/Sep/2026, 6:53:25 pm (UTC)'!
ChangeList removeSelector: #xtraVersion!
!methodRemoval: ChangeList #showsVersions stamp: 'jmv 28/Sep/2026, 6:53:25 pm (UTC)'!
ChangeList removeSelector: #showsVersions!

!ChangeList reorganize!
('initialization-release' addItem:text: initialize)
('scanning' classDefinitionRecordFrom: scanAndIgnore:in: scanCategory:class:meta:stamp:prior: scanClassComment: scanClassDefinition: scanClassRemoval: scanClassRenamed: scanDoIt scanFeatureProvision: scanFeatureRequirement: scanFile:from:to: scanMethodDefinition: scanMethodDefinitionNoCats: scanMethodRemoval: scanSpecificChangeType scanUpTo:informing:)
('menu actions' compareToCurrentVersion currentVersionsOfSelections deselectAll destroyCurrentCodeOfSelections fileInSelections fileOutCurrentVersionsOfSelections fileOutSelections invertSelections removeDoIts removeEmptyClassComments removeNonSelections removeOlderMethodVersions removeSelections removeUpToDate selectAll selectAllForAbsentClasses selectAllForThisClass selectContentsMatching selectEquivalentMethods selectMethodsOlderThanCurrent selectNewMethods selectRemovalsOfSent selectSuchThat: selectUnchangedMethods)
('viewing access' acceptedStringOrText annotation contentsDiffedFromCurrent list listIndex listSelectionAt: listSelectionAt:put: selectedClass selectedClassOrMetaClass selectedMessageName toggleListIndex: undiffedContents)
('accessing' changeList contentsSymbol currentChange currentCompiledMethod file)
('shout styling' allowsStylingFor:isTextDirty:)
('private' clearSelections)
('testing' isNewMethod:)
('message list' selector:refactoredTo:)
('controls' whatToShowSubmenuSpec)
!

!classRemoval: #ClassCommentVersionsBrowser stamp: 'jmv 28/Sep/2026, 6:54:19 pm (UTC)'!
Smalltalk removeClassNamed: #ClassCommentVersionsBrowser!

!classRemoval: #VersionsBrowser stamp: 'jmv 28/Sep/2026, 6:54:24 pm (UTC)'!
Smalltalk removeClassNamed: #VersionsBrowser!

!classRemoval: #VersionsBrowserWindow stamp: 'jmv 28/Sep/2026, 6:53:59 pm (UTC)'!
Smalltalk removeClassNamed: #VersionsBrowserWindow!

