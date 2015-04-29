'From Cuis 4.2 of 25 July 2013 [latest update: #2274] on 22 April 2015 at 9:13:42.914195 am'!

!MenuItemMorph class methodsFor: 'new-morph participation' stamp: 'jmv 4/22/2015 09:11'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!AutoCompleter methodsFor: 'entries' stamp: 'jmv 4/22/2015 09:09'!
insertSelected
	| entry editor selEnd str |
	textMorph ifNil: [ ^self ].
	entry _ self selectedEntry.
	editor _ textMorph editor.
	str _ model actualContents string.
	selEnd _ position.
	[selEnd < str size and: [ (str at: selEnd+1) tokenish ]] whileTrue: [ selEnd _ selEnd + 1 ].
	(selEnd < str size and: [ (str at: selEnd+1) = $ ]) ifTrue: [ selEnd _ selEnd + 1].
	editor selectFrom: position-prefix size+1 to: selEnd.
	editor
		replaceSelectionWith: entry;
		deselectAndPlaceCursorAt: position - prefix size + 1 + (self newCursorPosition: entry).
	textMorph redrawNeeded.
	menuMorph delete.
	menuMorph _ nil.
	^ true! !


!AutoCompleterMorph class methodsFor: 'instance creation' stamp: 'jmv 4/22/2015 09:08'!
initializedInstance
	| completer m |
	completer _ SmalltalkCompleter withModel: (TextModel withText: 'Small').
	completer
		instVarNamed: 'position'
		put: 5.
	completer computeEntries.
	m _ AutoCompleterMorph
		completer: completer
		position: 200 @ 200.
	completer instVarNamed: 'menuMorph' put: m.
	^m! !


!MenuItemMorph class reorganize!
('cached state access' releaseClassCachedState subMenuMarker)
('new-morph participation' includeInNewMorphMenu)
!

