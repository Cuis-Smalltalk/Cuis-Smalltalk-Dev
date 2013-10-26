'From Cuis 4.0 of 21 April 2012 [latest update: #1427] on 4 September 2012 at 6:13:51 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 9/4/2012 18:08'!
zzinvalidRect: aRectangle

	"warning. Senders are using global coordinates. Redesign!!"
	self flag: #jmvVer2.	"ok?"
	"local now!!!!!!!!!!"
	owner ifNotNil: [
		owner zzinvalidRect: (location displayBoundsOfTransformOf: aRectangle) ]! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 9/4/2012 17:47'!
invalidRect: damageRect
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self outermostWorldMorph 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: self morphBoundsInWorld ) ]
                ifFalse: [ super invalidRect: damageRect ]
! !

!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 9/4/2012 17:57'!
zzinvalidRect: damageRect
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self outermostWorldMorph 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: self morphBoundsInWorld ) ]
                ifFalse: [ super zzinvalidRect: damageRect ]
! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 9/4/2012 17:49'!
invalidRect: aRectangle

	"warning. Senders are using global coordinates. Redesign!!"
	self flag: #jmvVer2.	"ok?"
	owner ifNotNil: [
		owner invalidRect: aRectangle ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 9/4/2012 18:08'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	
	self layoutSubmorphsIfNeeded.
	self zzinvalidRect: (0@0 extent: self morphExtent)! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:52'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas image: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		self invalidRect: (savedPatch offset extent: savedPatch extent + self shadowOffset).
		Sensor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:18'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| drawBounds f |
	drawBounds _ self drawBoundsForRow: row.
	drawBounds _ drawBounds intersect: (0@0 extent: extent).
	f _ (item is: #Text) ifTrue: [ font emphasized: (item emphasisAt: 1) ] ifFalse: [ font ].
	canvas zzdrawString: item in: drawBounds font: f color: (self colorForRow: row).! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:20'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas zzfillRectangle: selectionDrawBounds colorOrInfiniteForm: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:25'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	aCanvas
		zzfillRectangle: selectionDrawBounds
		colorOrInfiniteForm: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 9/4/2012 17:57'!
onBlinkCursor
	"Blink the cursor"
	showCaret _ showCaret not | pauseBlinking.
	pauseBlinking _ false.
	caretRect ifNotNil: [ :r | self zzinvalidRect: r]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 9/4/2012 17:57'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	"Show cursor right now if needed"
	showCaret ifFalse: [
		showCaret _ true.
		caretRect ifNotNil: [ :r | self zzinvalidRect: r ]]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 9/4/2012 17:57'!
stopBlinking
	"And do not show cursor anymore."
	self stopSteppingSelector: #onBlinkCursor.
	"Hide cursor right now if needed"
	showCaret ifTrue: [
		showCaret _ false.
		caretRect ifNotNil: [ :r | self zzinvalidRect: r ]]! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:35'!
displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas
	| caretColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	caretColor _ Theme current insertionPoint.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!!"
			d _ isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0 < halfW ifTrue: [
		x1 _ x1 - x0 + halfW.
		x0 _ halfW ].
	r _ self morphExtentInOwner x-halfW-1.
	r < x1 ifTrue: [
		x0 _ x0 + r - x1.
		x1 _ r ].
	caretRect _ x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		zzline: x0@(bottom-halfW) to: x1@(top+halfW)
		width: w color: caretColor! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:37'!
drawCaretOn: aCanvas
	"Essentially copied from #displayInsertionMarkAtX:top:bottom:emphasis:on:"
	|  bottom x |

	showCaret ifTrue: [
		bottom _ self baseFont height.
		x _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayInsertionMarkAtX: x top: 0 bottom: bottom emphasis: emphasis on: aCanvas ]! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2012 17:33'!
drawSelectionOn: aCanvas
	| rightX leftX bottom |

	bottom _ self baseFont height.
	leftX _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	rightX _ self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.

	aCanvas
		zzfillRectangle: (leftX @ 0 corner: rightX @ bottom)
		colorOrInfiniteForm: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!Sonogram methodsFor: 'all' stamp: 'jmv 9/4/2012 18:10'!
plotColumn: dataArray

	| chm1 i normVal r |
	columnForm unhibernate.
	chm1 _ columnForm height - 1.
	0 to: chm1 do: [ :y | 
		i _ y*(dataArray size-1)//chm1 + 1.
		normVal _ ((dataArray at: i) - minVal) / (maxVal - minVal).
		normVal < 0.0 ifTrue: [normVal _ 0.0].
		normVal > 1.0 ifTrue: [normVal _ 1.0].
		columnForm bits at: chm1-y+1 put: (pixValMap at: (normVal * 255.0) truncated + 1)].
	(lastX _ lastX + 1) > (image width - 1) ifTrue:
		[self scroll].
	image copy: (r _ (lastX@0 extent: 1@image height))
			from: (32//image depth-1)@0
			in: columnForm rule: Form over.
	"self changed."
	self zzinvalidRect: r! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 9/4/2012 17:26'!
activateAndForceLabelToShow
	self activate.
	self morphPositionInOwner y < 0 ifTrue: [
		self morphPositionInOwner: (self morphPositionInOwner x @ 0)]! !

!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 9/4/2012 18:12'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(#titleAreaRect and #titleAreaInnerRect)"
	self zzinvalidRect: (0@0 extent: self morphExtent x @ (self labelHeight + borderWidth))! !


!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 9/4/2012 17:30'!
testHorizontalAlignment

	self should: [ taskbar morphPositionInWorld x = 0 ]! !

!methodRemoval: PasteUpMorph #invalidRect:from:!
PasteUpMorph removeSelector: #invalidRect:from:!
!methodRemoval: HandMorph #invalidRect:!
HandMorph removeSelector: #invalidRect:!
!methodRemoval: HandMorph #invalidRect:from:!
HandMorph removeSelector: #invalidRect:from:!
!methodRemoval: Morph #invalidRect:from:!
Morph removeSelector: #invalidRect:from:!
