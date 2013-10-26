'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 16 March 2012 at 6:00:34 pm'!
!classDefinition: #CodePackageList category: #'Package Support'!
ActiveModel subclass: #CodePackageList
	instanceVariableNames: 'selection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!
!classDefinition: #CodePackageListWindow category: #'Package Support'!
SystemWindow subclass: #CodePackageListWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!Object methodsFor: 'user interface' stamp: 'jmv 3/16/2012 17:01'!
             autoCompleterClass
	^nil! !

!Object methodsFor: 'user interface' stamp: 'jmv 3/16/2012 16:59'!
editorClass
	^TextEditor! !


!CodePackage methodsFor: 'printing' stamp: 'jmv 3/16/2012 17:12'!
asStringOrText
	^self packageName! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 3/16/2012 16:51'!
          installedPackages

	^InstalledPackages! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:40'!
             description

	selection ifNil: [ ^'' ].
	^selection printString! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:30'!
                     packageDirtyFlags

	^self packages collect: [ :each | "each packageName" true ]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:29'!
     packageNames

	^self packages collect: [ :each | each packageName ]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:30'!
                 packageRepositories

	^self packages collect: [ :each | "each repository" 'some' ]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:15'!
  packages
	^CodePackage installedPackages asArray sort: [ :a :b |
		 a packageName < b packageName ]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:22'!
                 selectionIndex

	^self packages indexOf: selection! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:39'!
  selectionIndex: anInteger

	selection _ anInteger = 0 ifFalse: [ self packages at: anInteger ].
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageRepositories;
		changed: #description;
		changed: #summary! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/16/2012 17:40'!
        summary

	selection ifNil: [ ^'' ].
	^'summary for: ', selection printString! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/16/2012 18:00'!
                              buildMorphicWindow
	" Inspector openAsMorphOn: SystemOrganization "
	| dirtyFlags names repositories  upperRow description summary buttonRow diffsButton saveButton |
	dirtyFlags _ PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names _ PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	repositories _ PluggableListMorph
		model: model 
		listGetter: #packageRepositories
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addMorph: names proportionalWidth: 0.3;
		addMorph: repositories proportionalWidth: 0.6.
	description _ TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.
	summary _ TextModelMorph
		textProvider: model
		textGetter: #summary.
	saveButton _ PluggableButtonMorph model: self action: #save label: 'Save'.
	diffsButton _ PluggableButtonMorph model: self action: #diffs label: 'Diffs with saved'.
	buttonRow _ LayoutMorph newRow.
"	buttonRow
		addMorph: saveButton layoutSpec: (LayoutSpec
			proportionalWidth: 0.3
			proportionalHeight: 0.6);
		addMorph: diffsButton layoutSpec: (LayoutSpec
			proportionalWidth: 0.3
			proportionalHeight: 0.6)."
	buttonRow
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent)proportionalWidth: 0.1;
		addMorph: diffsButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: summary proportionalHeight: 0.1;
		addAdjusterAndMorph: description proportionalHeight: 0.3;
		addAdjusterAndMorph: buttonRow proportionalHeight: 0.1.
	self setLabel: 'Installed Packages'! !


!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 3/16/2012 17:48'!
          model: anObject action: actionSel label: aString

	^ self new
		model: anObject
		stateGetter: nil
		action: actionSel
		label: aString! !


!SystemWindow class methodsFor: 'instance creation' stamp: 'jmv 3/16/2012 16:49'!
 open: model

	^self open: model label: nil! !

!methodRemoval: TextProvider #autoCompleterClass!
TextProvider removeSelector: #autoCompleterClass!
!methodRemoval: TextProvider #editorClass!
TextProvider removeSelector: #editorClass!
!methodRemoval: TextModel #editorClass!
TextModel removeSelector: #editorClass!
!methodRemoval: TestRunner #autoCompleterClass!
TestRunner removeSelector: #autoCompleterClass!
!methodRemoval: TestRunner #editorClass!
TestRunner removeSelector: #editorClass!
!methodRemoval: ProcessBrowser #editorClass!
ProcessBrowser removeSelector: #editorClass!
!methodRemoval: FillInTheBlankMorph #editorClass!
FillInTheBlankMorph removeSelector: #editorClass!

!CodePackageListWindow reorganize!
('GUI building' buildMorphicWindow)
!

!methodRemoval: CodePackageList #toggleIndex:!
CodePackageList removeSelector: #toggleIndex:!

!CodePackageList reorganize!
('accessing' description packageDirtyFlags packageNames packageRepositories packages selectionIndex selectionIndex: summary)
!

