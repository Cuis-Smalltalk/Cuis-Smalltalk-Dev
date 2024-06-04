'From Cuis7.1 [latest update: #6436] on 4 June 2024 at 12:14:23 pm'!

!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 12:11:33'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10		'browse full (b)' 						$b 	window 			browseMethodFull 						editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 						$h 	window 			browseHierarchy 						goTopIcon)
	)! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 12:12:47'!
buildSelectiveClassList
	"Define the class hierarchy list pane"

	^(PluggableListMorph
		withModel: model
		listGetter: #selectiveClassList
		indexGetter: #selectiveClassListIndex
		indexSetter: #selectiveClassListIndex:)
			menuNshortcutsKey: #classListMenuSpec.
! !

!methodRemoval: ProtocolBrowserWindow #protocolClassListKey:from: stamp: 'jmv 6/4/2024 12:13:12'!
ProtocolBrowserWindow removeSelector: #protocolClassListKey:from:!
!methodRemoval: ProtocolBrowserWindow #protocolClassListMenu stamp: 'jmv 6/4/2024 12:13:10'!
ProtocolBrowserWindow removeSelector: #protocolClassListMenu!

!ProtocolBrowserWindow reorganize!
('GUI building' buildCategoriesList buildMorphicWindow buildSelectiveClassList)
('menu & kbd shortcuts' classListMenuSpec messageListMenuSpec)
!

