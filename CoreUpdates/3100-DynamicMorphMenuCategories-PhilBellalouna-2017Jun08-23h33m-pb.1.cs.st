'From Cuis 5.0 of 7 November 2016 [latest update: #3099] on 9 June 2017 at 12:14:01 am'!

!RectangleLikeMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:47:05'!
categoryInNewMorphMenu
	^ 'Kernel'! !


!PasteUpMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:51:39'!
categoryInNewMorphMenu
	^ 'Worlds'! !


!EllipseMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:50:14'!
categoryInNewMorphMenu
	^ 'Basic'! !


!ProgressBarMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/9/2017 00:05:22'!
categoryInNewMorphMenu
	^ 'Widgets'! !


!ImageMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:54:09'!
categoryInNewMorphMenu
	^ 'Basic'! !


!StringMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:55:41'!
categoryInNewMorphMenu
	^ 'Basic'! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/9/2017 00:04:50'!
categoryInNewMorphMenu
	^ 'Widgets'! !


!LayoutMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:48:35'!
categoryInNewMorphMenu
	^ 'Layouts'! !


!ProgressMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/9/2017 00:10:17'!
categoryInNewMorphMenu
	^ 'Widgets'! !


!HaloHandleMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:53:34'!
categoryInNewMorphMenu
	^ 'Halos'! !


!HaloMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:53:26'!
categoryInNewMorphMenu
	^ 'Halos'! !


!ResizeMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:56:28'!
categoryInNewMorphMenu
	^ 'Views'! !


!FillInTheBlankMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:52:51'!
categoryInNewMorphMenu
	^ 'Widgets'! !


!HoverHelpMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:54:39'!
categoryInNewMorphMenu
	^ 'Widgets'! !


!TheWorldMenu methodsFor: 'construction' stamp: 'pb 6/9/2017 00:11:33'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu subMenu catDict |
	menu _ self menu: 'Add a new morph'.
	menu
		
			add: 'From Clipboard'
			target: myHand
			action: #pasteMorph;
		
			add: 'From Alphabetical List'
			subMenu: self alphabeticalMorphMenu.
	menu addLine.
	"Add 'Classic' New Morph menu"
	catDict _ Dictionary new.

	Morph allSubclassesDo: [ :eaSubclass |
		eaSubclass includeInNewMorphMenu ifTrue: [ | category |
			(eaSubclass respondsTo: #categoryInNewMorphMenu)
				ifTrue: [ category _ eaSubclass categoryInNewMorphMenu ]
				ifFalse: [ category _ 'Unknown' ].
			(catDict includesKey: category)
				ifTrue: [ (catDict at: category) add: eaSubclass ]
				ifFalse: [
					catDict
						at: category
						put: (OrderedCollection with: eaSubclass) ]]].
	catDict keys sort do: [ :categ |
		subMenu _ MenuMorph new.
		((catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ]) do: [ :cl |
			subMenu
				add: cl name
				target: self
				selector: #newMorphOfClass:event:
				argument: cl ].
		menu
			add: categ
			subMenu: subMenu ].
	self doPopUp: menu.! !

!methodRemoval: TheWorldMenu #newMorphOld!
TheWorldMenu removeSelector: #newMorphOld!

!RectangleLikeMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu)
!


!PasteUpMorph class reorganize!
('system startup' initClassCachedState releaseClassCachedState)
('instance creation' newWorld)
('new-morph participation' categoryInNewMorphMenu)
!


!EllipseMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu)
!


!ProgressBarMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu)
!


!ImageMorph class reorganize!
('cached state access' defaultForm releaseClassCachedState)
('new-morph participation' categoryInNewMorphMenu)
!


!StringMorph class reorganize!
('instance creation' contents: contents:font: contents:font:emphasis:)
('new-morph participation' categoryInNewMorphMenu)
!


!UpdatingStringMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu initializedInstance)
!


!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newRow)
('examples' example1 example10 example11 example13 example1b example2 example20 example3 example6 example7 launcherExample)
('new-morph participation' categoryInNewMorphMenu)
!


!ProgressMorph class reorganize!
('example' example)
('instance creation' label: label:subLabel:)
('new-morph participation' categoryInNewMorphMenu)
!


!HaloHandleMorph class reorganize!
('accessing' circleForm:)
('cached state access' releaseClassCachedState)
('new-morph participation' categoryInNewMorphMenu)
!


!HaloMorph class reorganize!
('class initialization' initialize)
('accessing - icons' haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon)
('cached state access' handleSize icons releaseClassCachedState)
('new-morph participation' categoryInNewMorphMenu)
!


!ResizeMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu)
!


!FillInTheBlankMorph class reorganize!
('instance creation' initializedInstance request: request:initialAnswer: request:initialAnswer:centerAt:onCancelReturn:acceptOnCR: request:initialAnswer:onCancel: request:onCancel:)
('new-morph participation' categoryInNewMorphMenu)
!


!HoverHelpMorph class reorganize!
('instance creation' contents: initializedInstance)
('new-morph participation' categoryInNewMorphMenu)
!

