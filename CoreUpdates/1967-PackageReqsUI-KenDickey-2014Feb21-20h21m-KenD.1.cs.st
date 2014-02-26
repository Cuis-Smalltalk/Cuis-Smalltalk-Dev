'From Cuis 4.2 of 25 July 2013 [latest update: #1966] on 21 February 2014 at 8:21:34.030103 pm'!
!classDefinition: #PackageRequirementsList category: #'Package Support'!
ActiveModel subclass: #PackageRequirementsList
	instanceVariableNames: 'codePackageList requirements selection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!PackageRequirementsList commentStamp: '<historical>' prior: 0!
I model the requirements of a CodePackageList!


!PackageRequirementsList methodsFor: 'initialization' stamp: 'KenD 12/31/2013 19:10'!
codePackageList: cpList

	codePackageList := cpList.
	selection := 0.
	self updateRequirementsFromPackageList.
	codePackageList 
		when: #changed 
		send:  #updateRequirementsFromPackageList 
		to: self! !

!PackageRequirementsList methodsFor: 'events' stamp: 'KenD 1/1/2014 18:27'!
updateRequirementsFromPackageList
	
	| req |
	req := requirements.
	((codePackageList selectionIndex isNil) 
			or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ requirements := #() ]
		ifFalse: [ requirements := codePackageList selection requires asArray ].
		
	(req = requirements) ifFalse: [
		self changed: #requirements
	]! !

!PackageRequirementsList methodsFor: 'accessing' stamp: 'KenD 1/1/2014 18:28'!
deleteSelectedRequirement

	| selectedPackage featureSpec |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	featureSpec 
		provides: featureSpec provides 
		requires: (featureSpec requires copyWithout: self selection).
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	! !

!PackageRequirementsList methodsFor: 'accessing' stamp: 'KenD 12/31/2013 20:52'!
requirementsStrings

	^ (requirements collect: [ :req | req printString ]) asArray! !

!PackageRequirementsList methodsFor: 'accessing' stamp: 'KenD 12/31/2013 18:27'!
selection

	^selection! !

!PackageRequirementsList methodsFor: 'accessing' stamp: 'KenD 12/31/2013 18:23'!
selectionIndex

	^ requirements indexOf: selection! !

!PackageRequirementsList methodsFor: 'accessing' stamp: 'KenD 12/31/2013 18:26'!
selectionIndex: anInteger

	selection := anInteger = 0 ifFalse: [ requirements at: anInteger ].! !


!PackageRequirementsList class methodsFor: 'instance creation' stamp: 'KenD 12/31/2013 18:48'!
fromCodePackageList: aCodePackageList

	^ self new codePackageList: aCodePackageList ! !


!CodePackage methodsFor: 'accessing' stamp: 'KenD 12/31/2013 16:11'!
featureSpec

	^ featureSpec! !

!CodePackage methodsFor: 'accessing' stamp: 'KenD 12/31/2013 15:25'!
requirementOfMe

	^ self provides requirementOfMe! !


!Feature methodsFor: 'accessing' stamp: 'KenD 12/31/2013 15:22'!
requirementOfMe

	^ FeatureRequirement 
		name: self name 
		minVersion: self version 
		minRevision: self revision 
		maxVersion: nil! !


!FeatureSpec methodsFor: 'accessing' stamp: 'KenD 12/31/2013 15:21'!
requirementOfMe
	"Answer a FeatureRequirement with suitable defaults"
	
	^ self provides requirementOfMe! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2014 16:17'!
buildButtonPane

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'Save (overwrite)'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'Create Package'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'Delete (merge in Cuis)'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'Browse unsaved changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'Browse package code'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'Add requirement'.
							
	buttonRow := LayoutMorph newRow.
	^ buttonRow
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6;
		yourself! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2014 18:31'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'Delete'.
							
	buttonLayout := LayoutMorph newColumn.
	buttonLayout addMorph: deleteReqButton 
					layoutSpec: (LayoutSpec 
										proportionalWidth: 1.0 
										fixedHeight: 30 
										minorDirectionPadding: #top).
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.9 
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.1 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right)
		yourself
		! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'KenD 1/1/2014 16:09'!
addRequirement
	"Ask user for a FeatureRequirement based on loaded packages"
	
	| current currentName packageNames reqiredNames selectionNames |
	current _ model selection.
	current ifNil: [ ^self ].

	packageNames := model packages collect: [ :pak | pak packageName ].
	currentName := current packageName.
	reqiredNames := current requires collect: [ :req | req name ].
	selectionNames := packageNames select:
		[ :name | ((name = currentName) or: [reqiredNames includes: name]) not ].
		
	(selectionNames size isZero)
		ifTrue: [ PopUpMenu inform: 'Required package must already be loaded' ]
		ifFalse: [ | choices selection cuisBaseName |
			cuisBaseName := Feature baseSystemFeature name.
			(reqiredNames includes: cuisBaseName)
			ifTrue: [ choices := OrderedCollection with: #CANCEL ]
			ifFalse: [ choices := OrderedCollection with: #CANCEL with: cuisBaseName ].
			choices addAll: selectionNames.
			selection := PopUpMenu withCaption: 'Choose package to require' 
											chooseFrom: choices.
			(selection = 1)
				ifTrue: [ ^ self ]  "1 -> Cancel"
				ifFalse: [ | req selectedName |
					selectedName := choices at: selection.
					(selectedName = cuisBaseName)
					ifTrue: [ req := Feature baseSystemFeature requirementOfMe ]
					ifFalse: [ req := (CodePackage installedPackages 
											at: selectedName) requirementOfMe].
					current featureSpec requires: req.
					current hasUnsavedChanges: true.
					self changed: #requirement
				]
		].
				

	! !


!CodePackageList methodsFor: 'accessing' stamp: 'KenD 12/31/2013 18:22'!
selectionIndex: anInteger

	selection _ anInteger = 0 ifFalse: [ packages at: anInteger ].
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements! !

!CodePackageList methodsFor: 'events' stamp: 'KenD 12/31/2013 18:32'!
updatePackageList
	
	| newPackages oldPackages |
	oldPackages _ (packages ifNil: [#()]) asIdentitySet.
	newPackages _ CodePackage installedPackages asIdentitySet.
	oldPackages do: [ :old |
		(newPackages includes: old) ifFalse: [
			old removeActionsWithReceiver: self ]].
	newPackages do: [ :new |
		(oldPackages includes: new) ifFalse: [
			new when: #dirtyFlagChanged send:#updateDirtyFlags to: self ]].
	newPackages _ newPackages asArray sort: [ :a :b |
		 a packageName < b packageName ].
	packages _ newPackages.
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2014 16:21'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: 16;
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: 16;
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.
		
	description := TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary := TextModelMorph
		textProvider: model
		textGetter: #summary.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: self buildButtonPane proportionalHeight: 0.1;
		addAdjusterAndMorph: summary proportionalHeight: 0.18;
		addAdjusterAndMorph: description proportionalHeight: 0.22;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.2.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2014 16:21'!
initialExtent

	^540@400! !

