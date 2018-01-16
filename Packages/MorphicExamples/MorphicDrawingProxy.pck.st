'From Cuis 5.0 of 7 November 2016 [latest update: #3244] on 7 January 2018 at 10:35:34 pm'!
'Description Example uses of Morph drawing proxies.  Note: this overrides some Morph behavior to demonstrate use cases.  The classes used for the examples are mainly just ones that will be instantiated with a couple of browser windows open.  The use cases are:
- A caching proxy (with and without statistics) to speed up drawing of ''expensive'' morphs
- An overlay proxy to provide additional drawing behavior
- A canvas override proxy to modify a Morph''s existing drawing behavior
- A simple profiler proxy

See MorphDrawingProxy class-side demo category.'!
!provides: 'MorphicDrawingProxy' 1 0!
SystemOrganization addCategory: #MorphicDrawingProxy!


!classDefinition: #TooColorfulMorph category: #MorphicDrawingProxy!
RectangleLikeMorph subclass: #TooColorfulMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'TooColorfulMorph class' category: #MorphicDrawingProxy!
TooColorfulMorph class
	instanceVariableNames: ''!

!classDefinition: #DesaturatingMorphicCanvas category: #MorphicDrawingProxy!
BitBltCanvas subclass: #DesaturatingMorphicCanvas
	instanceVariableNames: 'canvas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'DesaturatingMorphicCanvas class' category: #MorphicDrawingProxy!
DesaturatingMorphicCanvas class
	instanceVariableNames: ''!

!classDefinition: #MorphDrawingProxy category: #MorphicDrawingProxy!
Object subclass: #MorphDrawingProxy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'MorphDrawingProxy class' category: #MorphicDrawingProxy!
MorphDrawingProxy class
	instanceVariableNames: ''!

!classDefinition: #MorphCachingProxy category: #MorphicDrawingProxy!
MorphDrawingProxy subclass: #MorphCachingProxy
	instanceVariableNames: 'morph isDirty canvas form'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'MorphCachingProxy class' category: #MorphicDrawingProxy!
MorphCachingProxy class
	instanceVariableNames: ''!

!classDefinition: #MorphProfilingCachingProxy category: #MorphicDrawingProxy!
MorphCachingProxy subclass: #MorphProfilingCachingProxy
	instanceVariableNames: 'statisticsEnabled countHit countMissCanvas countMissDirty timeHit timeMissCanvas timeMissDirty'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'MorphProfilingCachingProxy class' category: #MorphicDrawingProxy!
MorphProfilingCachingProxy class
	instanceVariableNames: ''!

!classDefinition: #MorphCanvasOverrideProxy category: #MorphicDrawingProxy!
MorphDrawingProxy subclass: #MorphCanvasOverrideProxy
	instanceVariableNames: 'overrideCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'MorphCanvasOverrideProxy class' category: #MorphicDrawingProxy!
MorphCanvasOverrideProxy class
	instanceVariableNames: ''!

!classDefinition: #MorphDrawingOverlayProxy category: #MorphicDrawingProxy!
MorphDrawingProxy subclass: #MorphDrawingOverlayProxy
	instanceVariableNames: 'color'
	classVariableNames: 'Singleton'
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'MorphDrawingOverlayProxy class' category: #MorphicDrawingProxy!
MorphDrawingOverlayProxy class
	instanceVariableNames: ''!

!classDefinition: #MorphProfilingProxy category: #MorphicDrawingProxy!
MorphDrawingProxy subclass: #MorphProfilingProxy
	instanceVariableNames: 'drawCount drawTime drawDamageCount drawDamageTime isDirty morph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MorphicDrawingProxy'!
!classDefinition: 'MorphProfilingProxy class' category: #MorphicDrawingProxy!
MorphProfilingProxy class
	instanceVariableNames: ''!


!TooColorfulMorph commentStamp: '<historical>' prior: 0!
Something that should be a bit more muted!

!DesaturatingMorphicCanvas commentStamp: '<historical>' prior: 0!
For demonstration purposes only: incomplete and quite inefficient!

!MorphDrawingProxy commentStamp: '<historical>' prior: 0!
A MorphDrawingProxy is a wrapper object that can be injected into a Morph.  Its main function is to (optionally) perform an operaton before drawing (such as modifying the Canvas), call the Morph's #drawOn: method, then (optionally) perform an operation after drawing (such as drawing an overlay).  Depending on what the proxy is doing, it can either be a shared instance (i.e. something that simply draws an overlay over any Morph it is injected into but has no state itself) or per instance (i.e. a caching proxy with Form state per-Morph)

The examples deploy proxies horizontally across instances of a class.  But for the profiler proxies as a particular example, you would more likely want deploy vertically down a morph instance hierarchy to see where the bottlenecks are in a SystemWindow subclass. To do this, explore a morph and then send #addProfilingDrawingProxyToAllMorphs or #addDefaultDrawingProxyToAllMorphs to it.!

!MorphCachingProxy commentStamp: '<historical>' prior: 0!
A caching drawing proxy.  When active, intercepts #drawOn: calls for its Morph and only calls the Morph's #drawOn: when dirty=true, otherwise just draws its internal Form containing the last redraw of the Morph.  The primary use case for this is with Morphs that are relatively expensive to redraw and the majority of redraws have nothing to do with internal state changes in the Morph (i.e. display damage)

Note that caching should not be casually enabled for arbitrary Morphs as there is memory overhead per morph and for trivial morphs the overhead will likely result in a net increase in drawing time.

When in doubt, try the profiling subclass with statistics enabled and then interact with the morph normally for a while to see the results.!

!MorphProfilingCachingProxy commentStamp: '<historical>' prior: 0!
A caching proxy that also collects drawing statistics (i.e. cache hits/misses) to see how effective caching a given morph is.  !

!MorphCanvasOverrideProxy commentStamp: '<historical>' prior: 0!
A simple canvas override proxy which substitutes a desaturating canvas for the default canvas!

!MorphDrawingOverlayProxy commentStamp: '<historical>' prior: 0!
A simple overlay proxy which draws the morph, then draws itself!

!MorphProfilingProxy commentStamp: '<historical>' prior: 0!
Simple profiler which keeps track of counts and times for both 'dirty' redraws (the morph state changed) and 'damage' redraws (the display needed to be redrawn but the morph didn't change)!

!TooColorfulMorph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 19:52:10'!
wantsDrawingProxyWhenPreferenceEnabled
	"When the #autoEnableDrawingProxy Preference is enabled, does this Morph want it's cache enabled?"
	^ true.! !

!TooColorfulMorph methodsFor: 'drawing' stamp: 'pb 1/7/2018 22:35:21'!
drawOn: aCanvas
	aCanvas
		fillRectangle: (0 @ 0 corner: self morphExtent)
		color: (Color random alpha: 0.95).
	aCanvas
		fillRectangle: (0 @ 0 corner: 50 @ 50)
		color: Color hotPink.
	aCanvas
		fillRectangle: (50 @ 50 corner: 100 @ 100)
		color: Color brightGreen.
	aCanvas
		fillRectangle: (0 @ 100 corner: 50 @ 150)
		color: Color aqua.
	aCanvas
		drawString: 'Argh... my eyes!!'
		at: 0 @ 150
		font: AbstractFont default
		color: Color yellow .! !

!TooColorfulMorph methodsFor: 'initialization' stamp: 'pb 1/6/2018 19:59:58'!
initialize
	super initialize.
	self morphExtent: 100@180! !

!TooColorfulMorph methodsFor: 'nil' stamp: 'pb 1/7/2018 17:58:33'!
stepAt: millis
	self redrawNeeded ! !

!TooColorfulMorph methodsFor: 'stepping' stamp: 'pb 1/7/2018 18:00:51'!
stepTime
	^ 30! !

!TooColorfulMorph class methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 19:51:48'!
morphDrawingProxy
	"Return an instance (or *the* instance for a shared proxy) of the proxy"
	^ MorphCanvasOverrideProxy new.! !

!TooColorfulMorph class methodsFor: 'new-morph participation' stamp: 'pb 1/6/2018 19:56:31'!
categoryInNewMorphMenu
	^ 'Basic'! !

!DesaturatingMorphicCanvas methodsFor: 'accessing' stamp: 'pb 1/6/2018 20:21:40'!
canvas
	^ canvas ! !

!DesaturatingMorphicCanvas methodsFor: 'accessing' stamp: 'pb 1/6/2018 20:34:41'!
canvas: aCanvas
	canvas _ aCanvas.! !

!DesaturatingMorphicCanvas methodsFor: 'drawing' stamp: 'pb 1/6/2018 20:03:04'!
line: pt1 to: pt2 width: wp color: c
	^ canvas
		line: pt1
		to: pt2
		width: wp
		color: (self pvtNewColorFor: c).! !

!DesaturatingMorphicCanvas methodsFor: 'drawing-ovals' stamp: 'pb 1/6/2018 20:03:11'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor
	^ canvas
		ellipseCenterX: mcx
		y: mcy
		rx: mrx
		ry: mry
		borderWidth: mbw
		borderColor: (self pvtNewColorFor: mbc)
		fillColor: (self pvtNewColorFor: morphFillColor).! !

!DesaturatingMorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 1/6/2018 20:03:17'!
fillRectangle: aRectangle color: aColor
	^ canvas
		fillRectangle: aRectangle
		color: (self pvtNewColorFor: aColor).! !

!DesaturatingMorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 1/6/2018 20:03:22'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	^ canvas
		fillRectangle: aRectangle
		color: (self pvtNewColorFor: aColor)
		borderWidth: borderWidth
		borderStyleSymbol: aSymbol
		baseColorForBorder: (self pvtNewColorFor: baseColorForBorder).! !

!DesaturatingMorphicCanvas methodsFor: 'drawing-text' stamp: 'pb 1/6/2018 20:02:49'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	^ canvas
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint
		font: fontOrNil
		color: (self pvtNewColorFor: aColor).! !

!DesaturatingMorphicCanvas methodsFor: 'drawing-text' stamp: 'pb 1/6/2018 20:02:57'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	^ canvas 
		textComposition: aTextComposition
		bounds: boundsRect
		color: (self pvtNewColorFor: c)
		selectionColor: (self pvtNewColorFor: sc).! !

!DesaturatingMorphicCanvas methodsFor: 'printing' stamp: 'pb 1/6/2018 21:50:55'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll:' on canvas: '; print: canvas.! !

!DesaturatingMorphicCanvas methodsFor: 'private' stamp: 'pb 1/7/2018 02:39:39'!
pvtNewColorFor: aColor
	^ aColor ifNotNil: [
		aColor
			adjustSaturation: -0.7
			brightness: 0 ].! !

!DesaturatingMorphicCanvas class methodsFor: 'instance creation' stamp: 'pb 1/6/2018 20:35:21'!
on: aCanvas
	^ self new
		canvas: aCanvas;
		yourself.! !

!MorphDrawingProxy methodsFor: 'actions' stamp: 'pb 1/6/2018 18:36:54'!
markDirty: aMorph
	"FIXME - does it make more sense for the Morph to keep track of this?"
! !

!MorphDrawingProxy methodsFor: 'caching' stamp: 'pb 12/31/2017 22:58:22'!
releaseCachedState
! !

!MorphDrawingProxy methodsFor: 'drawing' stamp: 'pb 1/6/2018 17:28:53'!
draw: aMorph on: aCanvas
	self subclassResponsibility ! !

!MorphDrawingProxy class methodsFor: 'demo' stamp: 'pb 1/6/2018 20:26:29'!
demoDisableDrawingProxies
	"Proxies will not be created for new Morphs as needed"
	Preferences
		setPreference: #autoEnableDefaultDrawingProxy
		toValue: false.
	"Remove proxies from existing Morphs"
	Morph allSubInstancesDo: [ :ea |
		ea removeDrawingProxy ].! !

!MorphDrawingProxy class methodsFor: 'demo' stamp: 'pb 1/6/2018 20:26:22'!
demoEnableDrawingProxies
	"Proxies will be created for new Morphs as needed"
	Preferences
		setPreference: #autoEnableDefaultDrawingProxy
		toValue: true.
	"Inject proxies into existing Morphs"
	Morph allSubInstancesDo: [ :ea |
		ea wantsDrawingProxyWhenPreferenceEnabled ifTrue: [ ea addDefaultDrawingProxy ]].! !

!MorphDrawingProxy class methodsFor: 'demo' stamp: 'pb 1/6/2018 18:57:34'!
demoExploreProxies
	self allSubInstances explore.! !

!MorphDrawingProxy class methodsFor: 'testing' stamp: 'pb 1/6/2018 19:23:50'!
isAbstract
	^ thisContext methodClass == self class.! !

!MorphCachingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 16:55:22'!
morph
	^ morph! !

!MorphCachingProxy methodsFor: 'accessing' stamp: 'pb 1/6/2018 18:09:17'!
morph: aMorph
	morph := aMorph! !

!MorphCachingProxy methodsFor: 'actions' stamp: 'pb 1/7/2018 15:59:28'!
markDirty: aMorph
	"FIXME - does it make more sense for the Morph to keep track of this?"
	isDirty _ true.
	form ifNotNil: [
		aMorph morphExtent = self pvtForm extent
			ifTrue: [
				(BitBlt toForm: form)
					combinationRule: Form erase;
					copyBits ]
			ifFalse: [ self pvtReset ]].! !

!MorphCachingProxy methodsFor: 'caching' stamp: 'pb 12/31/2017 22:58:22'!
releaseCachedState
	self pvtReset.
! !

!MorphCachingProxy methodsFor: 'drawing' stamp: 'pb 1/6/2018 17:47:20'!
draw: aMorph on: aCanvas
	| redrawNeeded  |
	redrawNeeded _ false.
	isDirty ifTrue: [
		redrawNeeded _ true ].
	(form isNil or: [ canvas isNil ]) ifTrue: [
		
		redrawNeeded _ true ].

	redrawNeeded ifTrue: [ aMorph drawOn: self pvtCanvas ].
	aCanvas
		image: self pvtForm
		at: `0@0`.
	
	isDirty _ false.! !

!MorphCachingProxy methodsFor: 'initialization' stamp: 'pb 12/31/2017 22:54:24'!
initialize
	super initialize.
	isDirty _ true.
! !

!MorphCachingProxy methodsFor: 'printing' stamp: 'pb 1/6/2018 18:12:23'!
printOn: aStream
        aStream
                nextPutAll: self class name;
                nextPut: $(.
        aStream
                print: morph;
                nextPut: $)! !

!MorphCachingProxy methodsFor: 'private' stamp: 'pb 12/31/2017 17:30:36'!
pvtCanvas
	^ canvas ifNil: [ canvas _ BitBltCanvas onForm: self pvtForm ].! !

!MorphCachingProxy methodsFor: 'private' stamp: 'pb 12/31/2017 17:30:32'!
pvtForm
	^ form ifNil: [
		form _ Form
			extent: morph morphExtent
			depth: 32 ].! !

!MorphCachingProxy methodsFor: 'private' stamp: 'pb 12/31/2017 19:42:42'!
pvtReset
	canvas _ nil.
	form _ nil.! !

!MorphCachingProxy class methodsFor: 'instance creation' stamp: 'pb 12/31/2017 17:12:00'!
forMorph: aMorph
	^ self new
		morph: aMorph;
		yourself.! !

!MorphProfilingCachingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 17:04:00'!
avgDrawTime
	| counts times |
	counts _ countHit + countMissCanvas + countMissDirty.
	times _ timeHit + timeMissCanvas + timeMissDirty.
	^ counts = 0
		ifTrue: [ 0 ]
		ifFalse: [ times / counts ].! !

!MorphProfilingCachingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 17:05:15'!
cacheDrawCount
	^ statisticsEnabled
		ifTrue: [ countHit +countMissCanvas +countMissDirty ]
		ifFalse: [ -1 ].! !

!MorphProfilingCachingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 17:06:11'!
cacheHitRate
	^ statisticsEnabled
		ifTrue: [ |total|
			total := self cacheDrawCount .
			total > 0
				ifTrue: [ countHit / total asFloat ]
				ifFalse: [ 0 ]]
		ifFalse: [ -1 ].! !

!MorphProfilingCachingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 17:08:24'!
cacheSpeedup
	^ statisticsEnabled
		ifTrue: [ | missCount missTime |
			missCount _ countMissCanvas + countMissDirty.
			missTime _ timeMissCanvas + timeMissDirty.
			(countHit > 0 and: [ missCount > 0 ])
				ifTrue: [ 1 / (timeHit / countHit asFloat / (missTime / missCount asFloat)) ]
				ifFalse: [ 0 ]]
		ifFalse: [ -1 ].! !

!MorphProfilingCachingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 17:30:44'!
drawDamageRatio
	"What % of the calls have been damage? We're faking this a bit since we don't break out damage explicitly but we do know that hits *must* be damage, so assume everything else isn't"
	| total |
	total _ countHit + countMissCanvas + countMissDirty.
	^ total > 0
		ifTrue: [ countHit asFloat / total ]
		ifFalse: [ 0 ].! !

!MorphProfilingCachingProxy methodsFor: 'actions' stamp: 'pb 1/7/2018 17:36:23'!
disableStatistics
	statisticsEnabled _ false.
	self resetStatistics .! !

!MorphProfilingCachingProxy methodsFor: 'actions' stamp: 'pb 12/31/2017 22:56:37'!
enableStatistics
	statisticsEnabled _ true.! !

!MorphProfilingCachingProxy methodsFor: 'actions' stamp: 'pb 1/7/2018 17:35:13'!
resetStatistics
	countHit _ 0.
	countMissCanvas _ 0.
	countMissDirty _ 0.
	timeHit _ 0.
	timeMissCanvas _ 0.
	timeMissDirty _ 0.! !

!MorphProfilingCachingProxy methodsFor: 'caching' stamp: 'pb 1/7/2018 17:36:34'!
releasedCachedState
	super releaseCachedState.
	"Don't reset statisticsEnabled... if it is enabled, leave it that way"
	self resetStatistics .! !

!MorphProfilingCachingProxy methodsFor: 'drawing' stamp: 'pb 1/7/2018 17:44:02'!
draw: aMorph on: aCanvas
	| missReason drawStartTime |
	statisticsEnabled ifTrue: [
		drawStartTime _ Time localMicrosecondClock.
		isDirty ifTrue: [ missReason _ 'missDirty' ].
		(form isNil or: [ canvas isNil ]) ifTrue: [ missReason _ 'missCanvas' ]].
	super
		draw: aMorph
		on: aCanvas.
	statisticsEnabled ifTrue: [ |  drawTime  |

		drawTime _ Time localMicrosecondClock - drawStartTime.

		missReason
			ifNil: [
				countHit _ countHit + 1.
				timeHit _ timeHit + drawTime ]
			ifNotNil: [
				missReason = 'missCanvas'
					ifTrue: [
						countMissCanvas _ countMissCanvas + 1.
						timeMissCanvas _ timeMissCanvas + drawTime ]
					ifFalse: [
						countMissDirty _ countMissDirty + 1.
						timeMissDirty _ timeMissDirty + drawTime ]]].! !

!MorphProfilingCachingProxy methodsFor: 'initialization' stamp: 'pb 1/7/2018 17:36:29'!
initialize
	super initialize.
	self resetStatistics .
	statisticsEnabled _ true.! !

!MorphProfilingCachingProxy methodsFor: 'printing' stamp: 'pb 1/6/2018 23:46:46'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPutAll: '(avg: '.
	self avgDrawTime
		printOn: aStream
		fractionDigits: 1.
	aStream
		nextPut: (Character numericValue: 181);
		nextPutAll: 'S, hit: '.
	self cacheHitRate * 100
		printOn: aStream
		fractionDigits: 1.
	aStream
		nextPut: $%;
		nextPutAll: ' for: ';
		print: morph;
		nextPut: $).! !

!MorphCanvasOverrideProxy methodsFor: 'drawing' stamp: 'pb 1/6/2018 20:37:20'!
draw: aMorph on: aCanvas
	aMorph drawOn: (self pvtOverrideCanvasFor: aCanvas).! !

!MorphCanvasOverrideProxy methodsFor: 'private' stamp: 'pb 1/6/2018 20:23:39'!
pvtOverrideCanvasFor: aCanvas
	^ overrideCanvas
		ifNil: [ overrideCanvas _ DesaturatingMorphicCanvas on: aCanvas ]
		ifNotNil: [ :oc |
			oc canvas = aCanvas ifFalse: [ oc canvas: aCanvas ].
			oc].! !

!MorphDrawingOverlayProxy methodsFor: 'drawing' stamp: 'pb 1/6/2018 18:43:09'!
draw: aMorph on: aCanvas
	aMorph drawOn: aCanvas.
	aMorph hasKeyboardFocus ifFalse: [
		"aMorph morphBounds"
		aCanvas
			fillRectangle: (0 @ 0 corner: aMorph morphExtent)
			color: self pvtColor ].! !

!MorphDrawingOverlayProxy methodsFor: 'private' stamp: 'pb 1/6/2018 18:40:33'!
pvtColor
	^ color ifNil: [color := Color veryLightGray alpha: 0.3]! !

!MorphDrawingOverlayProxy class methodsFor: 'instance creation' stamp: 'pb 1/6/2018 18:22:29'!
singleton
	^ Singleton ifNil: [Singleton := self new]! !

!MorphProfilingProxy methodsFor: 'accessing' stamp: 'pb 1/6/2018 23:30:41'!
avgDrawTime
	| count time |
	count _ drawCount + drawDamageCount.
	time _ drawTime + drawDamageTime.
	^ count > 0
		ifTrue: [ time asFloat / count ]
		ifFalse: [ 0 ].! !

!MorphProfilingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 17:29:26'!
drawDamageRatio
	"What % of the calls have been damage?"
	| total |
	total _ drawCount + drawDamageCount.
	^ total > 0
		ifTrue: [ drawDamageCount asFloat / total ]
		ifFalse: [ 0 ].! !

!MorphProfilingProxy methodsFor: 'accessing' stamp: 'pb 1/6/2018 23:26:14'!
markDirty: aMorph
	"FIXME - does it make more sense for the Morph to keep track of this?"
	isDirty _ true.
! !

!MorphProfilingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 16:55:12'!
morph
	^ morph! !

!MorphProfilingProxy methodsFor: 'accessing' stamp: 'pb 1/7/2018 01:23:49'!
morph: aMorph
	morph := aMorph! !

!MorphProfilingProxy methodsFor: 'actions' stamp: 'pb 1/7/2018 17:37:27'!
resetStatistics
	drawCount _ 0.
	drawDamageCount _ 0.
	drawTime _ 0.
	drawDamageTime _ 0.! !

!MorphProfilingProxy methodsFor: 'drawing' stamp: 'pb 1/6/2018 23:29:46'!
draw: aMorph on: aCanvas
	| drawStartTime time |
	drawStartTime _ Time localMicrosecondClock.
	isDirty
		ifTrue: [ drawCount _ drawCount + 1 ]
		ifFalse: [ drawDamageCount _ drawDamageCount + 1 ].
	aMorph drawOn: aCanvas.
	time _ Time localMicrosecondClock - drawStartTime.
	isDirty
		ifTrue: [ drawTime _ drawTime + time ]
		ifFalse: [ drawDamageTime _ drawDamageTime + time ].
	isDirty _ false.! !

!MorphProfilingProxy methodsFor: 'initialization' stamp: 'pb 1/7/2018 17:37:45'!
initialize
	super initialize.
	isDirty _ true.
	self resetStatistics.! !

!MorphProfilingProxy methodsFor: 'printing' stamp: 'pb 1/7/2018 01:25:07'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPutAll: '(avg: '.
	self avgDrawTime
		printOn: aStream
		fractionDigits: 1.
	aStream
		nextPut: (Character numericValue: 181);
		nextPutAll: 'S for: ';
		print: morph;
		nextPut: $).! !

!Morph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/7/2018 15:16:56'!
addDefaultDrawingProxy
	drawingProxy ifNil: [|dp|
		dp _ self class morphDrawingProxy.
		self drawingProxy: dp].! !

!Morph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/7/2018 16:52:00'!
addDefaultDrawingProxyToAllMorphs
	self allMorphsDo: [ :eaMorph |
		eaMorph wantsDrawingProxyWhenPreferenceEnabled ifTrue: [ eaMorph addDefaultDrawingProxy ]].! !

!Morph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/7/2018 16:38:09'!
addProfilingDrawingProxyToAllMorphs
	self allMorphsDo: [ :eaMorph |
		eaMorph drawingProxy: MorphProfilingProxy new ].! !

!Morph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 18:39:36'!
initialize
	"initialize the state of the receiver"
	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation new.
	layoutNeeded _ false.
	(Preferences
		valueOfFlag: #autoEnableDefaultDrawingProxy
		ifAbsent: [ false ]) ifTrue: [
		self wantsDrawingProxyWhenPreferenceEnabled ifTrue: [ self addDefaultDrawingProxy ]].! !

!Morph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 12/31/2017 23:53:37'!
wantsDrawingProxyWhenPreferenceEnabled
	"When the #autoEnableDrawingProxy Preference is enabled, does this Morph want it's cache enabled?"
	^ false.! !

!Morph class methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 23:47:39'!
morphDrawingProxy
	"Return an instance (or *the* instance for a shared proxy) of the proxy"
	"
	^ MorphProfilingCachingProxy new.
	^ MorphProfilingProxy new.
	"
	^ MorphProfilingCachingProxy new.
! !

!PluggableButtonMorph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 23:55:21'!
wantsDrawingProxyWhenPreferenceEnabled
	"When the #autoEnableDrawingProxy Preference is enabled, does this Morph want it's cache enabled?"
	^ true.! !

!PluggableButtonMorph class methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 23:54:32'!
morphDrawingProxy
	"Return an instance (or *the* instance for a shared proxy) of the proxy"
	^ MorphProfilingProxy new.
! !

!InnerListMorph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/7/2018 14:49:25'!
wantsDrawingProxyWhenPreferenceEnabled
	^ true.! !

!InnerTextMorph methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 18:49:39'!
wantsDrawingProxyWhenPreferenceEnabled
	^ true.! !

!InnerTextMorph class methodsFor: '*MorphicDrawingProxy' stamp: 'pb 1/6/2018 18:49:58'!
morphDrawingProxy
	"Return an instance (or *the* instance for a shared proxy) of the proxy"
	^ MorphDrawingOverlayProxy singleton ! !
