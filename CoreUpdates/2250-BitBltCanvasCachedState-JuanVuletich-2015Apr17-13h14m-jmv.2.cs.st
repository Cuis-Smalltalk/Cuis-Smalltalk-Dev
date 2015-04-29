'From Cuis 4.2 of 25 July 2013 [latest update: #2250] on 17 April 2015 at 1:36:50.819607 pm'!

!Behavior methodsFor: 'system startup' stamp: 'jmv 4/17/2015 12:32'!
releaseClassCachedState
	"Will be called for each class on shutdown or snapshot.
	All class vars or class instVar vars that can be recreated lazily on demand, should be nilled.
	See implementors for examples"! !


!BitBltCanvas class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 13:09'!
accessProtect
	AccessProtect ifNil: [
		AccessProtect _ Semaphore forMutualExclusion ].
	^AccessProtect! !

!BitBltCanvas class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 13:15'!
cachedForms
	CachedForms ifNil: [
		CachedForms _ Dictionary new ].
	^CachedForms! !

!BitBltCanvas class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 13:19'!
releaseClassCachedState
	"
	BitBltCanvas releaseClassCachedState
	"
	CachedForms _ nil.
	AccessProtect _ nil.
	AuxForm _ nil.
	AuxBlitter _ nil! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/17/2015 12:33'!
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
	BitBltCanvas releaseClassCachedState.

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


!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 4/17/2015 13:18'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	Display getCanvas image: (SystemWindow roundedCornerTR: 20)multipliedBy: Color red at: 20@20
	"
	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 4/17/2015 13:18'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red.
	"

	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !


!BitBltCanvas class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 13:19'!
initialize
	"
	BitBltCanvas initialize.
	SystemDictionary initialize.
	"
	self releaseClassCachedState! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #bottomLeft. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #bottomRight. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
steButtonForm: extent
	^self cachedForms
		at: { #steButton . extent }
		ifAbsentPut: [
			| form canvas |
			form _ Form extent: extent depth: 32.
			canvas _ form getCanvas.
			canvas
				roundRect: (0@0 extent: extent)
				color: (Color gray: 0.4)
				radius: 4.
			canvas
				roundRect: (1@1 extent: extent-2)
				color: Color white
				radius: 4
				gradientTop: 1.0
				gradientCenter: 0.73
				gradientBottom: 0.94
				gradient1Height: (extent y-8+1 max: extent y//2).
			form]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
topLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #topLeft. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				topLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
topRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #topRight. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				topRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
verticalGrayGradient: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #vertical. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				verticalGradient: height
				topColor: (Color gray: gradientTopFactor)
				bottomColor: (Color gray: gradientBottomFactor) ]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 4/17/2015 12:16'!
arrowOfDirection: aSymbol size: finalSizeInteger
	^self cachedForms
		at: { aSymbol . finalSizeInteger }
		ifAbsentPut: [
			self buildArrowOfDirection: aSymbol size: finalSizeInteger ]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 4/17/2015 12:16'!
arrowWithGradientOfDirection: aSymbol
	"
	BitBltCanvas clearFormsCache.
	(BitBltCanvas arrowWithGradientOfDirection: #up) display.
	(BitBltCanvas arrowWithGradientOfDirection: #down) display.
	(BitBltCanvas arrowWithGradientOfDirection: #left) display.
	(BitBltCanvas arrowWithGradientOfDirection: #right) display.
	"
	^self cachedForms
		at: { aSymbol . #gradient }
		ifAbsentPut: [ | packName pack |
			packName _ #GUIFormsPack.
			pack _ (Smalltalk at: packName) new.
			(pack decodeContentFrom: pack contentMap)
				at: 'gradient', aSymbol capitalized, 'Arrow' ]! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 12:33'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld backgroundImage ifNil: [ self runningWorld color: CurrentTheme background ].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	Taskbar reset; initialize.
	self runningWorld restoreMorphicDisplay.
	
	^ CurrentTheme! !

!methodRemoval: BitBltCanvas class #clearFormsCache!
BitBltCanvas class removeSelector: #clearFormsCache!
!methodRemoval: BitBltCanvas class #shutDown!
BitBltCanvas class removeSelector: #shutDown!
BitBltCanvas initialize!

!BitBltCanvas class reorganize!
('class initialization' initialize)
('system startup')
('cached forms' bottomLeftCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom: steButtonForm: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom: verticalGrayGradient:gradientTop:gradientBottom:)
('cached arrow forms' arrowOfDirection:size: arrowWithGradientOfDirection: buildArrowOfDirection:size: buildArrowWith:borderForm:)
('cached state access' accessProtect cachedForms releaseClassCachedState)
!

!methodRemoval: SystemDictionary #someCleanup!
SystemDictionary removeSelector: #someCleanup!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeFromShutDownList: BitBltCanvas!

