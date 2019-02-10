'From Cuis 5.0 of 7 November 2016 [latest update: #3597] on 4 February 2019 at 12:02:07 am'!

!PackageRequirementsList methodsFor: 'accessing' stamp: 'KLG 2/3/2019 23:50:55'!
updateSelectedRequirement

	| selectedPackage featureSpec newRequires selectedName |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	newRequires := (featureSpec requires copyWithout: self selection),
			 {((selectedName _ self selection name) = Feature baseSystemFeature name)
				ifTrue: [ Feature baseSystemFeature requirementOfMe ]
				ifFalse: [ (CodePackage installedPackages at: selectedName) requirementOfMe]}.
	featureSpec 
		provides: featureSpec provides 
		requires: newRequires.
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KLG 2/3/2019 23:30:07'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete'.
	deleteReqButton color: self widgetsColor.
	updateReqButton _ PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						minorDirectionPadding: #top);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						minorDirectionPadding: #top);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right);
		color: `Color transparent`;
		yourself
		! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'KLG 2/3/2019 23:35:44'!
addRequirement
	"Ask user for a FeatureRequirement based on loaded packages"
	
	| current currentName packageNames reqiredNames selectionNames choices selection cuisBaseName req selectedName |
	current _ model selection.
	current ifNil: [ ^self ].

	packageNames := model packages collect: [ :pak | pak packageName ].
	currentName := current packageName.
	reqiredNames := current requires collect: [ :r | r name ].
	selectionNames := packageNames select: [ :name |
		((name = currentName) or: [reqiredNames includes: name]) not ].
	cuisBaseName := Feature baseSystemFeature name.
	choices := OrderedCollection with: #CANCEL.
	(reqiredNames includes: cuisBaseName)
		ifFalse: [ choices add: cuisBaseName ].		
	choices addAll: selectionNames.
	choices size = 1 ifTrue: [
		^ PopUpMenu inform: 'All loaded packages are already required, as is Cuis base system' ].
	selection := PopUpMenu
		withCaption: 'Choose package to require' 
		chooseFrom: choices.
	selection <= 1
		ifTrue: [ ^ self ]  "1 -> Cance, 0 -> Clicked outside the menu"
		ifFalse: [
			selectedName := choices at: selection.
			req := (selectedName = cuisBaseName)
				ifTrue: [ Feature baseSystemFeature requirementOfMe ]
				ifFalse: [ (CodePackage installedPackages at: selectedName) requirementOfMe].
			current featureSpec requires: req.
			current hasUnsavedChanges: true.
			self changed: #requirement ]! !


!PackageRequirementsList reorganize!
('initialization' codePackageList:)
('events' updateRequirementsFromPackageList)
('accessing' deleteSelectedRequirement requirementsStrings selection selectionIndex selectionIndex: updateSelectedRequirement)
!

