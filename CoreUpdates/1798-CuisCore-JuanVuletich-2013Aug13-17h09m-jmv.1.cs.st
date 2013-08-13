'From Cuis 4.2 of 25 July 2013 [latest update: #1797] on 13 August 2013 at 5:10:13.636 pm'!

!HierarchicalListMorph methodsFor: 'commands' stamp: 'jmv 8/13/2013 17:09'!
expandAll
	(selectedMorph isNil
		or: [selectedMorph isExpanded])
		ifTrue: [^self].
	self expandAll: selectedMorph.
	scroller adjustExtent.
	self setScrollDeltas! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

PluggableButtonMorph allInstancesDo: [ :b |
	b actionSelector = #collapseOrExpand 
		ifTrue: [ b action: #collapse]]!

