'From Cuis 4.1 of 12 December 2012 [latest update: #1563] on 19 January 2013 at 3:27:17 pm'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/19/2013 15:23'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ false! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/19/2013 15:23'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false."

	^ false! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:24'!
morphicDirectoryTreePane

	^(HierarchicalListMorph
		model: model
		listGetter: #initialDirectoryList
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:
		mainView: self
		menuGetter: #volumeMenu
		keystrokeAction: nil)
			autoDeselect: false;
			yourself! !

!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 1/19/2013 15:24'!
morphicFileListPane

	^PluggableListMorph
		model: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: 
		mainView: self
		menuGetter: #fileListMenu
		keystrokeAction: nil! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 15:24'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 15:25'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^ true! !

!methodRemoval: PasteUpMorph #dropEnabled!
PasteUpMorph removeSelector: #dropEnabled!
!methodRemoval: Morph #dragEnabled!
Morph removeSelector: #dragEnabled!
!methodRemoval: Morph #dropEnabled!
Morph removeSelector: #dropEnabled!
!methodRemoval: Morph #enableDrag:!
Morph removeSelector: #enableDrag:!
!methodRemoval: Morph #enableDrop:!
Morph removeSelector: #enableDrop:!
