'From Cuis 4.1 of 12 December 2012 [latest update: #1538] on 4 January 2013 at 8:37:24 pm'!

!LayoutSpec commentStamp: '<historical>' prior: 0!
LayoutSpecs are the basis for the layout mechanism. Any Morph can be given a LayoutSpec, but in order to honor it, its owner must be a LayoutMorph.

A LayoutSpec specifies how a morph wants to be layed out. It can specify either a fixed width or a fraction of some available owner width. Same goes for height. If a fraction is specified, a minimum extent is also possible.


Alternatives:
	- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
	- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
	- proportionalWidth isNil, fixedWidth notNil		->		Use fixedWidth
	- proportionalWidth notNil, fixedWidth isNil		->		NOT VALID

Same goes for proportionalHeight and fixedHeight
!


!Morph commentStamp: '<historical>' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://minnow.cc.gatech.edu/squeak/30. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

Structure:
instance var 	Type 					Description 
owner 			Morph		 			My parent Morph, or nil for the top-level Morph, which is a
 				or nil					world, typically a PasteUpMorph.
submorphs 	Array 					My child Morphs. 
location 		MatrixTransform2x3 	Specifies position (and in the future, angle of rotation and scale change) inside owner
extension 		MorphExtension 		Allows extra properties to be stored without adding a
				or nil  					storage burden to all morphs. 
!


!InspectorWindow commentStamp: 'jmv 1/4/2013 13:00' prior: 0!
A Window built specifically to be the view on an Inspector model object. See category 'GUI building'.!


!LayoutMorph commentStamp: '<historical>' prior: 0!
A row or column of widgets, does layout by placing them either horizontally or vertically.

Submorphs might specify a LayoutSpec. If some don't, then, for a column, the column width is taken as the width, and any morph height is kept. Same for rows: submorph width would be maintained, and submorph height would be made equal to row height.!


!StrikeFont commentStamp: 'jmv 1/4/2013 13:26' prior: 0!
I represent a compact encoding of a set of Forms corresponding to characters in the ISO-8859-15 character set. All the forms are placed side by side in a large form whose height is the font height, and whose width is the sum of all the character widths. The xTable variable gives the left-x coordinates of the subforms corresponding to the glyphs. Characters are mapped to glyphs by using the characterToGlyphMap.

Subclasses can have non-trivial mapping rules as well as different representations for glyphs sizes (e.g., not using an xTable). If so, these classes should return nil when queried for xTable and/or the characterToGlyphMap. This will cause the CharacterScanner primitive to fail and query the font for the width of a character (so that a more programatical approach can be implemented).

For display, fonts need to implement two messages:
	#installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor
This method installs the receiver (a font) on the given DisplayContext (which may be an instance of BitBlt or Canvas (or any of it's subclasses). The font should take the appropriate action to initialize the display context so that further display operations can be optimized.
	#displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta
This method is called for each subsequent run of characters in aString which is to be displayed with the (previously installed) settings.!


!TheWorldMenu commentStamp: 'jmv 1/4/2013 13:27' prior: 0!
Instances of TheWorldMenu serve to present the primary Squeak menu obtained by clicking on open desktop.!


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 1/4/2013 13:05'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |
	"Build widgets. We'll assemble them below."
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contentsText _ TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection
			allowStyler: true.
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	self setLabel: ([model object printStringLimitedTo: 64] on: Error do: [ model object class name, ' (printing failed)']).
	"This is needed due to the way we update some windows when stuff changes. A cleaner approach is needed here!!"
	self setUpdatablePanesFrom: #(fieldList)! !

