'From Cuis 4.2 of 25 July 2013 [latest update: #2048] on 20 July 2014 at 10:15:17.875305 am'!

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:56'!
minimumExtent
	| minH minW |
	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	minH _ self vIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	minW _ self hIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	^minW@minH! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:57'!
minimumExtent

	^14@14! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:03'!
minimumExtent
	| ext |
	"This returns the minimum extent that the morph may be shrunk to.  Not honored in too many places yet, but respected by the resizeToFit feature, at least.  copied up from SystemWindow 6/00"
	self flag: #jmvVer2.	"in owner's coordinates?"
	(ext _ self valueOfProperty: #minimumExtent)
		ifNotNil:
			[^ ext].
	^ 4@4! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:44'!
morphHeight

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ self morphExtent y! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:44'!
morphWidth

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ self morphExtent x! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:59'!
privateExtent: aPoint
	"Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent _ aPoint max: self minimumExtent.
	^extent = newExtent
		ifFalse: [ extent _ newExtent ]; not! !


!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:14'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			magnifiedIcon _ nil ]; yourself! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:10'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			"Now reset widget sizes"
			scroller adjustExtent.
			self updateScrollBarsBounds.
			self setScrollDeltas ]; yourself! !


!LimitedHeightTextMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:12'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			maxHeight _ extent y ]; yourself! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:11'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			self flag: #jmv.
			"Most times it is not necessary to recreate the buttons"
			self recreateSubmorphs ]; yourself! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:04'!
adjustExtent
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!! See #fit"
	self morphExtent: owner viewableExtent! !

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 10:08'!
privateExtent: aPoint
	| newExtent |

	"Resist changing the extent if no wordwrap. this should be checked."
	wrapFlag ifFalse: [ ^ false ].
	
	"Just update width. Height is set by ourselves. See #fit"
	newExtent _ aPoint x truncated @ extent y.

	^ (super privateExtent: newExtent)
		ifTrue: [
			self resetTextComposition.
			self editor recomputeSelection.	
			self updateFromTextComposition ]; yourself! !

!methodRemoval: StringMorph #privateExtent:!
StringMorph removeSelector: #privateExtent:!
!methodRemoval: SystemWindow #privateExtent:!
SystemWindow removeSelector: #privateExtent:!
