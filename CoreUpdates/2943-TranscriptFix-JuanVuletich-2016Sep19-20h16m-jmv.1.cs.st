'From Cuis 4.2 of 25 July 2013 [latest update: #2942] on 19 September 2016 at 8:58:41 pm'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay innerRectangle lastDisplayPosition bounds '!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/19/2016 20:45:55'!
displayOn: aForm in: aRectangle
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f canvas innerR |
	aForm fill: aRectangle fillColor: Color white.
	font _ AbstractFont default.

	innerR _ aRectangle insetBy: self padding.
	fh _ font height.
	count _ innerR height // fh-1.
	x _ innerR left.
	y _ innerR top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerR.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/19/2016 20:26:15'!
showsOnDisplay
	^showOnDisplay! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 9/19/2016 20:56:13'!
bounds: aRectangle

	bounds _ aRectangle! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/19/2016 20:54:25'!
display
	showOnDisplay ifTrue: [
		self displayOn: Display in: bounds.
		DisplayScreen screenUpdateRequired: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	self triggerEvent: #redraw! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/19/2016 20:53:35'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas r innerR |
	innerR _ bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font _ AbstractFont default.
		fh _ font height.
		count _ innerR height // fh-1.
		x _ innerR left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerR top.
		canvas _ aForm getCanvas.
		r _ innerR left: lastDisplayPosition.
		canvas newClipRect: r.
		(canvas drawString: string at: x@y font: font color: Color veryDarkGray) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !

!Transcript class methodsFor: 'class initialization' stamp: 'jmv 9/19/2016 20:56:07'!
initialize
	"
	self initialize
	"
	showOnDisplay _ true.
	bounds _ 20@20 extent: 300@500.
	logToFile _ false.
	entries _ Array new: self maxEntries.
	unfinishedEntry _ String new writeStream.
	lastDisplayPosition _ 0.
	accessSemaphore _ Semaphore forMutualExclusion.
	self clear! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 9/19/2016 20:57:43'!
drawOn: aCanvas
	Transcript
		showOnDisplay: true;
		displayOn: form in: (0@0 extent: extent).
	aCanvas image: form at: 0@0.
	Transcript
		bounds: self morphBoundsInWorld;
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace! !

!methodRemoval: Transcript class #displayOn:!
Transcript class removeSelector: #displayOn:!
Transcript initialize!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds'!
Display restore!