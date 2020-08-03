'From Cuis 5.0 [latest update: #4322] on 31 July 2020 at 4:52:46 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/31/2020 16:48:16'!
displayBounds: aRectangle

	self displayBounds ~= aRectangle ifTrue: [
		privateDisplayBounds _ aRectangle.
		self setProperty: #displayBounds toValue: aRectangle ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Morph allSubInstancesDo: [ :m | m displayBounds ifNotNil: [ :r | m instVarNamed: 'privateDisplayBounds' put: r ]]!

