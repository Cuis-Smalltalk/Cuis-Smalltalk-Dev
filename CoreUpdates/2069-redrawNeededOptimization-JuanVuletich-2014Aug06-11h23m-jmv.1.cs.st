'From Cuis 4.2 of 25 July 2013 [latest update: #2068] on 6 August 2014 at 11:36:32.40225 am'!

!TextEditor methodsFor: 'new selection' stamp: 'jmv 8/6/2014 11:18'!
selectFrom: start to: stop
	"Select the specified characters inclusive.
	I.e. if contents is 'hello' and we want to select the $e, do 'self selectFrom: 2 to: 2'
	If we want to put cursor after e, but with no selection, do 'self selectFrom: 3 to: 2', or better yet, call #selectAt:"
	self selectInvisiblyFrom: start to: stop.
	self storeSelectionInComposition.
	"Preserve current emphasis if selection is empty"
	start > stop ifTrue: [
		self setEmphasisHereFromText ]! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/6/2014 11:10'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"
	| oldBoundsInWorld |
	self flag: #jmvVer2.
	extent = newExtent ifFalse: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		oldBoundsInWorld _ self morphBoundsInWorld.
		(self privateExtent: newExtent) ifTrue: [
			"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
			self invalidateDisplayRect: oldBoundsInWorld from: nil.
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/6/2014 11:08'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld |
	(location position = newPos and: [extent = newExtent]) ifTrue: [
		^ self ].		"Null change"

	"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	location setPosition: newPos.
	(self privateExtent: newExtent) ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		self invalidateDisplayRect: oldBoundsInWorld from: nil.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/6/2014 11:34'!
processKeyStroke: evt
	| action |

	(acceptOnCR and: [evt isReturnKey])
		ifTrue: [^ self acceptContents].

	self pauseBlinking.
	evt isReturnKey ifTrue: [	"Return - check for special action"
		action _ self crAction.
		action ifNotNil: [
			^action value]].
	self handleInteraction: [ editor processKeyStroke: evt ].

	"Is this really needed? It produces whole morph invalidation just by (for example)
	moving the cursor around... (jmv Aug 6, 2014)"
	"self updateFromTextComposition."

	self scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/6/2014 11:33'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	newExtent _ (self textComposition extent max: 9 @ AbstractFont default height) + (0 @ 2).
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				self redrawNeeded.
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !

