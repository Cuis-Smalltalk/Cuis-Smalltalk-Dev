'From Cuis7.5 [latest update: #7664] on 21 October 2025 at 5:10:53 pm'!

!Text methodsFor: 'attributes' stamp: 'jmv 10/21/2025 17:10:14'!
hasAnyTextAlignment
	"Return false if there are no Text Alignment attributes"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isTextAlignment ]]! !


!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 10/21/2025 16:37:51'!
shrink
	"Automatically shrink extent to whatever the text needs"
	self shrinkFromExtent: 9999999@9999999
! !


!TextAttribute methodsFor: 'testing' stamp: 'jmv 10/21/2025 17:05:03'!
isTextAlignment
	^false! !


!TextAlignment methodsFor: 'testing' stamp: 'jmv 10/21/2025 17:05:11'!
isTextAlignment
	^true! !


!TextComposition methodsFor: 'composition' stamp: 'jmv 10/21/2025 17:10:27'!
composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY 
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult composer |
	composer := TextComposer new.
	editor ifNotNil: [
		composer editor: editor ].
	newResult := composer
				composeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				text: model actualContents
				extentForComposing: extentForComposing.
	lines := newResult first asArray.

	"If there is any justified, rightFlush or centered text, assume all available width has been used."
	model actualContents hasAnyTextAlignment ifTrue: [ ^extentForComposing x ].

	^newResult second "right border of recomposed lines (bounded by extentForComposing x)"! !

