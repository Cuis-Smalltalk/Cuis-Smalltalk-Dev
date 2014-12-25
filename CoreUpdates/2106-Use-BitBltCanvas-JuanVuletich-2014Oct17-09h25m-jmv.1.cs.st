'From Cuis 4.2 of 25 July 2013 [latest update: #2094] on 17 October 2014 at 9:25:57.534746 am'!

!GrafPort commentStamp: '<historical>' prior: 0!
Add services for BitBltCanvas!


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 10/1/2014 09:48'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.
	BitBltCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Form methodsFor: 'accessing' stamp: 'jmv 10/1/2014 09:39'!
getCanvas
	"Return a Canvas that can be used to draw onto the receiver"
	^BitBltCanvas onForm: self! !


!Form class methodsFor: 'import / export' stamp: 'jmv 10/1/2014 09:45'!
generateContentPack
	"
	Ensure you have an empty ChangeSet and evaluate

	Form generateContentPack

	Then file out the ChangeSet.
	"
	| forms packClsName |
	packClsName _ #GUIFormsPack.
	ContentPack
		variableSubclass: packClsName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Morphic-Support'.

	forms _ Dictionary new.
	forms at: 'gradientUpArrow' put: (BitBltCanvas arrowWithGradientOfDirection: #up).
	forms at: 'gradientDownArrow' put: (BitBltCanvas arrowWithGradientOfDirection: #down).
	forms at: 'gradientLeftArrow' put: (BitBltCanvas arrowWithGradientOfDirection: #left).
	forms at: 'gradientRightArrow' put: (BitBltCanvas arrowWithGradientOfDirection: #right).
	forms at: 'biggerNormalCursor' put: CursorWithAlpha biggerNormal.


	(Smalltalk at: packClsName)
		withDictionary: forms.

	self notify: 'ContentPack created. You can file out current ChangeSet'! !


!Morph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 09:47'!
imageForm: depth
	| canvas |
	canvas _ BitBltCanvas depth: depth over: self morphFullBoundsInWorld.
	canvas fullDraw: self.
	^ canvas form! !

!Morph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 09:48'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ BitBltCanvas forShadowOver: self morphBoundsInWorld.
	canvas into: self.
	self drawOn: canvas.
	^ canvas form! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 09:49'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas frameAndFillRectangle: (0@0 extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			frameRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: extent x - scrollbarThickness@0.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: 0@0 + extent - scrollbarThickness.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			color: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].
			aCanvas
				drawString: (completer entries at: index) asString
				at: rectangle topLeft
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 10/1/2014 09:47'!
magnifiedForm
	| srcRect form neededExtent |
	lastPos _ self sourcePoint.
	srcRect _ self sourceRectFrom: lastPos.
	((srcRect intersects: self morphBoundsInWorld) and: [ RecursionLock == nil ])
		ifTrue: [
			RecursionLock _ self.
			"try to reuse form if appropriate"
			auxCanvas _ (auxCanvas notNil and: [ auxCanvas extent = srcExtent ])
				ifTrue: [
					"Just in case we go out of the Display"
					srcRect origin > (0@0) ifFalse: [
						auxCanvas form fillBlack ].
					BitBltCanvas on: auxCanvas form over: srcRect ]
				ifFalse: [ BitBltCanvas depth: 32 over: srcRect ].
			auxCanvas fullDraw: self world.
			form _ auxCanvas form.
			RecursionLock _ nil]
		ifFalse: [
			"cheaper method if the source is not occluded"
			form _ Display copy: srcRect].
	"smooth if non-integer scale"
	neededExtent _ (srcExtent * magnification ) truncated.
	(magnifiedForm isNil or: [ magnifiedForm extent ~=  neededExtent ])
		ifTrue: [ magnifiedForm _ Form extent: neededExtent depth: 32 ].
	(WarpBlt toForm: magnifiedForm)
		sourceForm: form;
		colorMap: (form colormapIfNeededFor: magnifiedForm);
		cellSize: (magnification isInteger ifTrue: [1] ifFalse: [2]);  "installs a new colormap if cellSize > 1"
		combinationRule: 3;
		copyQuad: form boundingBox innerCorners toRect: magnifiedForm boundingBox.
	^magnifiedForm.! !


!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 10/1/2014 09:48'!
updateDownButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	self iconName: #drawDownIcon.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 10/1/2014 09:48'!
updateLeftButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	self iconName: #drawLeftIcon.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 10/1/2014 09:48'!
updateRightButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	self iconName: #drawRightIcon.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 10/1/2014 09:48'!
updateUpButtonImage
	"update the receiver's as a upButton. put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	self iconName: #drawUpIcon.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 09:47'!
expandedForm

	^BitBltCanvas arrowOfDirection: #down size: 13! !

!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 09:47'!
notExpandedForm

	^BitBltCanvas arrowOfDirection: #right size: 13! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 10:47'!
submorphsShadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	bnds ifNil: [ ^nil ].

	canvas _ BitBltCanvas forShadowOver: bnds.
	canvas into: self.
	submorphs reverseDo: [ :m | canvas fullDraw: m ].
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 10/1/2014 09:49'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld backgroundImage ifNil: [ self runningWorld color: CurrentTheme background ].
	SystemWindow initialize.
	FormCanvas clearFormsCache.
	BitBltCanvas clearFormsCache.
	Taskbar reset; initialize.
	self runningWorld restoreMorphicDisplay.
	
	^ CurrentTheme! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 10/1/2014 09:49'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= viewBox extent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (BitBltCanvas withExtent: viewBox extent depth: Display depth)]! !

!WorldState methodsFor: 'canvas' stamp: 'jmv 10/9/2014 23:03'!
setCanvas: aMorphicCanvas
	canvas _ aMorphicCanvas.
	canvas into: world.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].
	damageRecorder doFullRepaint! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
self runningWorld privateExtent: 100@100. self runningWorld restoreMorphicDisplay.
Smalltalk garbageCollect!

