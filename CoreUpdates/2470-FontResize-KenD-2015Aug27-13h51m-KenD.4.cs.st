'From Cuis 4.2 of 25 July 2013 [latest update: #2464] on 27 August 2015 at 4:15:48.437612 pm'!

!PluggableListMorph methodsFor: 'geometry' stamp: 'KenD 8/27/2015 15:45'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: Preferences standardListFont.! !


!CodePackageListWindow methodsFor: 'geometry' stamp: 'KenD 8/28/2015 16:11'!
fontPreferenceChanged
	"System fonts have changed; rebuild myself"
	self layoutMorph removeAllMorphs.
	super fontPreferenceChanged.
	self buildMorphicWindow.
! !


!InnerListMorph methodsFor: 'geometry' stamp: 'KenD 8/27/2015 15:40'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self font: Preferences standardListFont.! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'KenD 8/27/2015 15:58'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self updateFromTextComposition.! !


!SystemWindow methodsFor: 'geometry' stamp: 'KenD 8/27/2015 15:26'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self rescaleButtons.
	self model ifNotNil: [ :m | m changed: #actualContents ].
	self updatablePanes ifNotNil: [ :panes | panes do: [:aPane | aPane verifyContents] ].
	self redrawNeeded.
! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 8/27/2015 15:16'!
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
	buttonRow
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow ! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 8/27/2015 14:39'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground textHeigth |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	textHeigth := AbstractFont default height.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: textHeigth;
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
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: textHeigth;
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
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: textHeigth;
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

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 8/27/2015 15:19'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'Delete'.
	deleteReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newColumn.
	buttonLayout addMorph: deleteReqButton 
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
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.9 
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.1 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right);
		color: Color transparent;
		yourself
		! !


!Theme class methodsFor: 'user interface' stamp: 'KenD 8/27/2015 13:51'!
changeFontSizes

	| menu result |
	menu _ SelectionMenu fromArray: {
		{ 'Huge Fonts'. #hugeFonts}.
		{ 'Very big Fonts'. #veryBigFonts}.
		{ 'Big Fonts'. #bigFonts}.
		{ 'Standard Fonts'. #standardFonts}.
		{ 'Small Fonts'. #smallFonts}.
		{ 'Very small Fonts'. #verySmallFonts}.
		{ 'Tiny Fonts'. #tinyFonts}}.
	result _ menu startUpWithCaption: 'Choose a size'.

	result ifNotNil: [
		Preferences perform: result.
		DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].
		"self inform: 'Please close and reopen all windows' "
	]! !


!CodePackageListWindow reorganize!
('GUI building' buildButtonPane buildMorphicWindow buildRequirementsPane initialExtent windowColor)
('commands' addRequirement browse browseChanges createPackage deletePackage)
('geometry' fontPreferenceChanged)
!

