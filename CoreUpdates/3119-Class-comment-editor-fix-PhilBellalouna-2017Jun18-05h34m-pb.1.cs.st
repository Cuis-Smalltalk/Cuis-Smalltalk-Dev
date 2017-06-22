'From Cuis 5.0 of 7 November 2016 [latest update: #3103] on 18 June 2017 at 5:34:41 am'!

!Browser methodsFor: 'class comment pane' stamp: 'pb 6/18/2017 05:34:23'!
newClassComment: aText
	"The user has just entered aText.
	It may be all red (a side-effect of replacing the default comment), so remove the color if it is."
	| theClass |
	theClass _ self selectedClassOrMetaClass theNonMetaClass.
	theClass ifNotNil: [
		theClass classComment: aText asString ].
	self changed: #classCommentText.
	^ true! !

