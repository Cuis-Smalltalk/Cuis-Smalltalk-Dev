'From Cuis 4.2 of 25 July 2013 [latest update: #2722] on 8 April 2016 at 9:40:37.713448 am'!

!TextAnchor commentStamp: '<historical>' prior: 0!
TextAnchors support anchoring of images in text.  A TextAnchor exists as an attribute of text emphasis, and it gets control like a FontReference, through the emphasizeScanner: message.  Depending on whether its anchoredMorph is a Morph or a Form, it repositions the morph, or displays the form respectively.  The coordination between composition, display and selection can best be understood by browsing the various implementations of placeEmbeddedObject:.

In the morphic world, simply embed any morph in text.  In the old world, you can create an image reference using code such as the following.

"A Form"
('Hello', (Text withForm: (EllipseMorph new imageForm: 2)), 'world') edit
('Hello', (Text withForm: (EllipseMorph new imageForm: 32)), 'world') edit

"A Morph"
((Text withAll: 'foo') , (Text string: '*' asString attribute: (TextAnchor new anchoredFormOrMorph: EllipseMorph new)) , (Text withAll: 'bar')) edit

In this case you select a piece of the screen, and it gets anchored to a one-character text in the editor's past buffer.  If you then paste into some other text, you will see the image as an embedded image.!


!TextAttribute methodsFor: 'testing' stamp: 'jmv 4/7/2016 18:39'!
isOverridenByShout
	"True if to be removed from code when styling"
	^self isParagraphAttribute not! !


!TextAnchor methodsFor: 'testing' stamp: 'jmv 4/7/2016 18:40'!
isOverridenByShout
	"True if to be removed from code when styling"
	^false! !


!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 4/8/2016 09:37'!
possiblyChanged
	| embeddedMorphs |
	embeddedMorphs _ model actualContents embeddedMorphs.
	self submorphsDo: [:each| 
		(embeddedMorphs includes: each) ifFalse: [
			self privateRemove: each.
			each privateOwner: nil ]].
	embeddedMorphs do: [:each| each owner == self ifFalse: [self addMorphFront: each]].
	owner possiblyChanged! !

!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 4/7/2016 18:20'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	textComposition ifNotNil: [ 
		textComposition composeAll.
		self fit.
		self selectionChanged ]! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 4/8/2016 09:33'!
formatAndStyle
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText := textModel actualContents.
	self privateFormatAndConvert.
	textModel basicActualContents: formattedText.
	formattedText size < 4096
		ifTrue: [
			formattedText := textModel actualContents "copy". "ok not to copy?"
			self privateStyle.
			textModel stylerStyled: formattedText checkForChanges: false ]
		ifFalse: [ self styleInBackgroundProcess ].! !

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 4/8/2016 09:33'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	formattedText _ textModel actualContents "copy". "ok not to copy?"
	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			self privateStyle.
			WorldState addDeferredUIMessage: [ textModel stylerStyled: formattedText checkForChanges: true ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/7/2016 18:40'!
setAttributesIn: aText fromRanges: ranges in: anInterval
	"modified by jmv to keep existing attributes if they answer true to #isParagraphAttribute"
	| attributes defaultAttributes paragraphAttributes attr newRuns newValues lastAttr lastCount beginning chunkRuns end | 		

	anInterval
		ifNil: [
			beginning _ 1.
			end _ aText size ]
		ifNotNil: [
			beginning _ anInterval first.
			end _ anInterval last ].
	defaultAttributes := self attributesFor: #default.
	paragraphAttributes := Array new: end-beginning+1.
	1 to: paragraphAttributes size do: [ :i |
		paragraphAttributes at: i put:
			((aText attributesAt: i+beginning-1) reject: [ :each | each isOverridenByShout ])].
	attributes := Array new: end-beginning+1.
	1 to: attributes size do: [ :i | attributes at: i put: (paragraphAttributes at: i), defaultAttributes].
	ranges do: [ :range |
		(attr := self attributesFor: range rangeType)
			ifNotNil: [ range start to: range end do: [:i |
				attributes at: i put: (paragraphAttributes at: i), attr]]].
	newRuns := OrderedCollection new: attributes size // 10.
	newValues := OrderedCollection new: attributes size // 10.
	1 to: attributes size do: [:i |
		attr := attributes at: i.
		i = 1 
			ifTrue: [
				newRuns add: 1.
				lastCount := 1.
				lastAttr := newValues add: attr]
			ifFalse:[
				attr == lastAttr
					ifTrue: [
						lastCount := lastCount + 1.
						newRuns at: newRuns size put: lastCount]
					ifFalse: [
						newRuns add: 1.
						lastCount := 1.
						lastAttr := newValues add: attr]]].
	chunkRuns _ (RunArray runs: newRuns values: newValues) coalesce.
	aText
		basicReplaceAttributesFrom: beginning
		to: end
		with: chunkRuns! !

