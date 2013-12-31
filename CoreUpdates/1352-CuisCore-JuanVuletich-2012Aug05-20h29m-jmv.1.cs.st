'From Cuis 4.0 of 21 April 2012 [latest update: #1351] on 5 August 2012 at 8:30:34 pm'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs fullBounds color extension position extent layoutNeeded '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'fileIn/out' stamp: 'jmv 8/5/2012 19:49'!
prepareToBeSaved
	"Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session."

	self releaseCachedState.
	fullBounds _ nil.
	layoutNeeded _ true! !

!Morph methodsFor: 'private' stamp: 'jmv 8/5/2012 19:48'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |
	self flag: #jmvVer2.
	"
	(aMorph notNil and: [
		bounds origin ~= self defaultBounds origin ]) ifTrue: [
			'                                ---------------- Nos mandan #privateOwner: , pero nos han mandado bounds antes (no necesariamente un problema!!!!!!!!!!)!!' print.
			thisContext printStack: 10 ].
	"

	self validatePositionAndBounds.
	self validateExtentAndBounds.
			
	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner _ owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition _ self morphPositionInWorld ].
	owner _ aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			bounds _ position extent: extent.
			fullBounds _ nil.
			layoutNeeded _ true.
			self validatePositionAndBounds.
			self validateExtentAndBounds.
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					bounds _ (owner externalizeToWorld: position) extent: extent.
					fullBounds _ nil.
					layoutNeeded _ true.
					self validatePositionAndBounds.
					self validateExtentAndBounds.
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					position _ owner internalizeFromWorld: oldGlobalPosition.
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					self validatePositionAndBounds.
					self validateExtentAndBounds.
					]]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/5/2012 19:50'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [	
		layoutNeeded _ false.
		^fullBounds _ bounds].

	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ]! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 8/5/2012 19:49'!
adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth doNotResizeBelow |
	doNotResizeBelow _ self minPaneWidthForReframe.
	i _ submorphs indexOf: aLayoutAdjustMorph.
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ aPoint x - aLayoutAdjustMorph referencePosition x.
	delta _ delta max: doNotResizeBelow - lCurrentWidth.
	delta _ delta min: rCurrentWidth - doNotResizeBelow.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [	"If both proportional, update them"
			ls setProportionalWidth: (1.0 * lNewWidth / lCurrentWidth * ls proportionalWidth).
			rs setProportionalWidth: (1.0 * rNewWidth / rCurrentWidth * rs proportionalWidth) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.
	fullBounds _ bounds.
	layoutNeeded _ false.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 8/5/2012 19:49'!
adjustVerticallyBy: aLayoutAdjustMorph at: aPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight doNotResizeBelow |
	doNotResizeBelow _ self minPaneHeightForReframe.
	i _ submorphs indexOf: aLayoutAdjustMorph.
	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ aPoint y - aLayoutAdjustMorph referencePosition y.
	delta _ delta max: doNotResizeBelow - tCurrentHeight.
	delta _ delta min: bCurrentHeight - doNotResizeBelow.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [	"If both proportional, update them"
			ts setProportionalHeight: (1.0 * tNewHeight / tCurrentHeight * ts proportionalHeight).
			bs setProportionalHeight: (1.0 * bNewHeight / bCurrentHeight * bs proportionalHeight) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs.
	fullBounds _ bounds.
	layoutNeeded _ false.! !

!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs fullBounds color extension position extent layoutNeeded'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
