"
Highlights the target when hovered.
"
Class {
	#name : 'HighlightingMenuItemMorph',
	#superclass : 'MenuItemMorph',
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'selecting' }
HighlightingMenuItemMorph >> isSelected: aBoolean [

	super isSelected: aBoolean.
	target highlighted: aBoolean.
]
