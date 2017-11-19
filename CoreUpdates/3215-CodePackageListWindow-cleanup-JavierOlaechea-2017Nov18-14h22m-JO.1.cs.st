'From Cuis 5.0 of 7 November 2016 [latest update: #3207] on 18 November 2017 at 2:30:36 pm'!

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'JO 11/18/2017 14:28:55'!
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
		addMorphKeepMorphHeight: (StringMorph new contents: ' Unsaved?');
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
		addMorphKeepMorphHeight: (StringMorph new contents: ' Package Name');
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
		addMorphKeepMorphHeight: (StringMorph new contents: ' File Name');
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
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !

