"
Morphic view for CodePackageList models. See category 'GUI building'.
"
Class {
	#name : 'CodePackageListWindow',
	#superclass : 'SystemWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu-world' }
CodePackageListWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		10.
			#label 			-> 		'Installed Packages'.
			#object 		-> 		CodePackageList.
			#selector 		-> 		#open.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for managing Packages (optional units of code) installed in the system'.
		} asDictionary}`
]

{ #category : 'commands' }
CodePackageListWindow >> addRequirement [
	"Ask user for a FeatureRequirement based on loaded packages"
	
	| current currentName packageNames reqiredNames selectionNames choices selection cuisBaseName req selectedName |
	current := model selection.
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
			self changed: #requirement ]
]

{ #category : 'commands' }
CodePackageListWindow >> browse [

	| current browser |
	current := model selection.
	current ifNil: [ ^self ].

	browser := SinglePackageBrowser new.
	browser package: current.
	BrowserWindow open: browser label: browser labelString
]

{ #category : 'commands' }
CodePackageListWindow >> browseChanges [
	| current |
	current := model selection.
	current ifNil: [ ^self ].

	ChangeSorterWindow
		open: (SingleSetChangeSorter new 
			myChangeSet: (ChangeSet existingOrNewChangeSetForPackage: current))
		label: nil
]

{ #category : 'GUI building' }
CodePackageListWindow >> buildButtonPane [

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'save'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'new'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'delete/merge'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'browse'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'add requirement'.
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		doAdoptWidgetsColor;
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow 
]

{ #category : 'GUI building' }
CodePackageListWindow >> buildMorphicWindow [
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| names fileNames namesPane fileNamesPane 
	upperRow  description summary labelBackground |
	labelBackground := Theme current background.
	
	names := PluggableListMorph
		withModel: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	namesPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		withModel: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames alwaysHideVerticalScrollbar.
	fileNamesPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: namesPane proportionalWidth: 0.4;
		addAdjusterAndMorph: fileNamesPane proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.
	
	names scrollSiblings: {fileNames}.
	fileNames scrollSiblings: {names}.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'
]

{ #category : 'GUI building' }
CodePackageListWindow >> buildRequirementsPane [

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		withModel: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
		
	deleteReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete')
							setBalloonText: 'Remove selected Feature requirement'.
	deleteReqButton color: self widgetsColor.
	updateReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update')
							setBalloonText: 'Update requirement to current Feature revision'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LinearLayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSizeSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSizeSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LinearLayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSizeSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #leftOrTop);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSizeSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #rightOrBottom);
		color: `Color transparent`;
		yourself
		
]

{ #category : 'commands' }
CodePackageListWindow >> createPackage [
	self request: 'Name for new package?' do: [:aString|
		aString ifNotEmpty:
			[CodePackage
				named: aString
				createIfAbsent: true
				registerIfNew: true]]
]

{ #category : 'commands' }
CodePackageListWindow >> deletePackage [

	| current cs |
	current := model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs := ChangeSet existingOrNewChangeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash printString, cs name.
		cs isForBaseSystem: true ].
	CodePackage deregister: current
]

{ #category : 'geometry' }
CodePackageListWindow >> fontPreferenceChanged [
	"System fonts have changed; rebuild myself"
	self layoutMorph removeAllMorphs.
	super fontPreferenceChanged.
	self buildMorphicWindow.

]

{ #category : 'GUI building' }
CodePackageListWindow >> initialExtent [

	^`540@400` * (Preferences at: #standardCodeFont) lineSpacing // 14
]

{ #category : 'GUI building' }
CodePackageListWindow >> windowColor [
	^ Theme current packageList
]
