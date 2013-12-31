'From Cuis 4.0 of 21 April 2012 [latest update: #1386] on 21 August 2012 at 5:04:21 pm'!

!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 8/21/2012 17:01'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Change Set name') fixedHeight: 16;
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Classes') fixedHeight: 16;
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.47;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Methods') fixedHeight: 16;
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 8/21/2012 16:58'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow description summary buttonRow browseChangesButton saveButton browseButton createButton deleteButton backColor labelBackground |
	backColor _ self textBackgroundColor.	
	labelBackground _ Theme current background.
	dirtyFlags _ PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	names _ PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: 16;
		addMorphUseAll: names.

	fileNames _ PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: 16;
		addMorphUseAll: fileNames.

	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.

	description _ TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary _ TextModelMorph
		textProvider: model
		textGetter: #summary.

	saveButton _ PluggableButtonMorph model: model action: #save label: 'Save (overwrite)'.
	createButton _ PluggableButtonMorph model: self action: #createPackage label: 'Create Package'.
	deleteButton _ PluggableButtonMorph model: self action: #deletePackage label: 'Delete (merge in Cuis)'.
	browseChangesButton _ PluggableButtonMorph model: self action: #browseChanges label: 'Browse unsaved changes'.
	browseButton _ PluggableButtonMorph model: self action: #browse label: 'Browse package code'.
	buttonRow _ LayoutMorph newRow.
	buttonRow
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: summary proportionalHeight: 0.18;
		addAdjusterAndMorph: description proportionalHeight: 0.22;
		addAdjusterAndMorph: buttonRow proportionalHeight: 0.1.
	self setLabel: 'Installed Packages'! !


!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 8/21/2012 17:02'!
testWeakDumps
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	ReferenceStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ ReferenceStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	window delete! !


!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 8/21/2012 17:00'!
testWeakDumps
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	SmartRefStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ SmartRefStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	window delete! !


!TaskbarTaskTest methodsFor: 'as yet unclassified' stamp: 'cbr 4/19/2011 18:18'!
setUp

	morph _ Morph new.
	task _ TaskbarTask new minimize: morph! !

