'From Cuis7.1 [latest update: #6729] on 17 September 2024 at 5:12:13 pm'!

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 9/17/2024 17:08:46'!
defaultPadding
	"Answer the default inner padding for the receiver.
	Redefine this method for a different initial padding in your morphs."

	^ `3@3`! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:10:07'!
minimumExtent
	| minW minH h w |
	w := theScrollerExtent ifNotNil: [ theScrollerExtent x ] ifNil: [ 0 ].
	h := theScrollerExtent ifNotNil: [ theScrollerExtent y ] ifNil: [ 0 ].
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW := padding x * 2 + w min: ScrollBar scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW := minW + ScrollBar scrollbarThickness].
	minH := padding y * 2 + h.
	self hIsScrollbarShowing
		ifTrue: [
			minH := minH + ScrollBar scrollbarThickness].
	minH := minH min: ScrollBar scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:10:13'!
viewableAreaBottom
	^ self focusIndicatorBottom - padding y! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:10:19'!
viewableAreaLeft
	^ self focusIndicatorLeft + padding x! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:10:24'!
viewableAreaRight
	^ self focusIndicatorRight - padding x! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:10:28'!
viewableAreaTop
	^ self focusIndicatorTop + padding y! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:11:09'!
desiredExtent
	^ scroller desiredExtent + (padding + borderWidth + 1 * 2)! !

!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 9/17/2024 17:11:17'!
desiredHeight
	^ scroller desiredHeight + (padding y + borderWidth + 1 * 2)! !

!methodRemoval: PluggableScrollPane #xtraBorder stamp: 'jmv 9/17/2024 17:11:28'!
PluggableScrollPane removeSelector: #xtraBorder!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PluggableScrollPane allSubInstancesDo: [ :m | m padding: 3@3 ].!

