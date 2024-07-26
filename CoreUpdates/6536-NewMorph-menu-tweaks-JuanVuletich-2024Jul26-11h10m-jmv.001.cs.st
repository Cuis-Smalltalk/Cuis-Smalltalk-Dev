'From Cuis7.1 [latest update: #6535] on 26 July 2024 at 11:19:51 am'!

!WorldMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/26/2024 11:10:39'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!ClassNameRequestMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/26/2024 11:13:48'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!DraggableButtonMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/26/2024 11:13:31'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!HoverableButtonMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/26/2024 11:13:18'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!TileResizeMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/26/2024 11:14:51'!
categoryInNewMorphMenu

	^ 'User Interface'! !


!SmalltalkCompleterMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/26/2024 11:11:00'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!AutoCompleterMorph class methodsFor: 'instance creation' stamp: 'jmv 7/26/2024 11:18:23'!
initializedInstance
	| completer m |
	completer := SmalltalkCompleter withModel: (TextModel withText: 'Small').
	completer
		instVarNamed: 'position'
		put: 5.
	completer computeEntries.
	m := completer morphClass
		completer: completer
		position: `10@10`.
	completer instVarNamed: 'menuMorph' put: m.
	^m! !


!WorldMorph class reorganize!
('instance creation' newWorld)
('class initialization' initClassCachedState)
('new-morph participation' includeInNewMorphMenu)
!


!ClassNameRequestMorph class reorganize!
('new-morph participation' includeInNewMorphMenu)
!


!DraggableButtonMorph class reorganize!
('new-morph participation' includeInNewMorphMenu)
!


!HoverableButtonMorph class reorganize!
('instance creation' model:stateGetter:action:onMouseEnterSend:onMouseLeaveSend:)
('new-morph participation' includeInNewMorphMenu)
!


!TileResizeMorph class reorganize!
('as yet unclassified' allowedArea calculateTileRegions: calculateTileRegionsHorizontallyIn:by: calculateTileRegionsIn:by: calculateTileRegionsVerticallyIn:by: divideRectHorizontally:by: divideRectVertically:by: tileOpenWindows)
('new-morph participation' categoryInNewMorphMenu)
!


!SmalltalkCompleterMorph class reorganize!
('new-morph participation' includeInNewMorphMenu)
!

