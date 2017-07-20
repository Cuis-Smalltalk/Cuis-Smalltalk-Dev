'From Cuis 5.0 of 7 November 2016 [latest update: #3130] on 18 July 2017 at 6:20:17 am'!
'Description A port of Bouncing Atoms with the atoms in a SystemWindow. A pop-up menu enables selection of options, including an active plot of infection history, also in a SystemWindow.'!
!provides: 'BouncingAtoms' 1 32!
!requires: 'Morphic-Widgets-Extras' 1 0 nil!
!classDefinition: #AtomMorph category: #BouncingAtoms!
EllipseMorph subclass: #AtomMorph
	instanceVariableNames: 'velocity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BouncingAtoms'!
!classDefinition: 'AtomMorph class' category: #BouncingAtoms!
AtomMorph class
	instanceVariableNames: ''!

!classDefinition: #HeaterCoolerAtom category: #BouncingAtoms!
AtomMorph subclass: #HeaterCoolerAtom
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BouncingAtoms'!
!classDefinition: 'HeaterCoolerAtom class' category: #BouncingAtoms!
HeaterCoolerAtom class
	instanceVariableNames: ''!

!classDefinition: #InfectionGraph category: #BouncingAtoms!
FunctionGraphMorph subclass: #InfectionGraph
	instanceVariableNames: 'data points yScaleFactor startTime deltaT ticN tics yFactor xFactor resumed hoverPoint'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BouncingAtoms'!
!classDefinition: 'InfectionGraph class' category: #BouncingAtoms!
InfectionGraph class
	instanceVariableNames: ''!

!classDefinition: #BouncingAtomsMorph category: #BouncingAtoms!
RectangleLikeMorph subclass: #BouncingAtomsMorph
	instanceVariableNames: 'infectionHistory transmitInfection recentTemperatures temperature historyWindow nAtoms fpsEnabled fps fpsCurrentFrameCount fpsCurrentFrameCountTime resume'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BouncingAtoms'!
!classDefinition: 'BouncingAtomsMorph class' category: #BouncingAtoms!
BouncingAtomsMorph class
	instanceVariableNames: ''!


!AtomMorph commentStamp: '<historical>' prior: 0!
AtomMorph represents an atom used in the simulation of
an ideal gas. It's container is typically a BouncingAtomsMorph.

Try:

	BouncingAtomsMorph  initializedInstance openInWorld

to open the gas simulation or:

	AtomMorph example

to open an instance in the current world!

!HeaterCoolerAtom commentStamp: 'dhn 7/4/2015 13:53' prior: 0!
A heater atom or a cooler atom, its temperature determined by the sign of its velocityDelta property.!

!InfectionGraph commentStamp: 'dhn 5/11/2016 15:55' prior: 0!
Normalize the number of infected atoms, treat them as a time series, and plot them in a graph.!

!BouncingAtomsMorph commentStamp: 'dhn 7/4/2015 17:01' prior: 0!
This morph shows how simulation of an ideal gas might work. When the morph gets step messages, it makes all its atom submorphs move along their velocity vectors, bouncing when they hit the window sides. It also exercises the Morphic damage reporting and display architecture. Here are some things to try:

  1. Resize this morph as the atoms bounce around.
  2. Use the pop-up menu, try the available choices.
  3. Switch the shape of atoms from ellipse to rectangle.
  4. Start an infection.
  5. Show infection history.
  6. Start another infection with the history window open.
  7. Set the atom count, then
     add a Heater/Cooler Atom(s), then
     start an infection and note the effect as atoms pass over the Heater/Cooler Atom(s).
  8. Repeatedly start an infection with the same atoms from step 7.!

!AtomMorph methodsFor: 'private' stamp: 'jm 8/10/1998 17:40'!
bounceIn: aRect
	"Move this atom one step along its velocity vector and make it bounce if it goes outside the given rectangle. Return true if it is bounced."

	| p vx vy px py bounced |
	p := self morphPosition.		"dhn 4/4/2015"
	vx := velocity x.		vy := velocity y.
	px := p x + vx.		py := p y + vy.
	bounced := false.
	px > aRect right ifTrue: [
		px := aRect right - (px - aRect right).
		vx := velocity x negated.
		bounced := true].
	py > aRect bottom ifTrue: [
		py :=  aRect bottom - (py - aRect bottom).
		vy := velocity y negated.
		bounced := true].
	px < aRect left ifTrue: [
		px := aRect left - (px - aRect left).
		vx := velocity x negated.
		bounced := true].
	py < aRect top ifTrue: [
		py :=  aRect top - (py - aRect top).
		vy := velocity y negated.
		bounced := true].
	self morphPosition: px @ py.	"dhn 4/4/2015"
	bounced ifTrue: [self velocity: vx @ vy].
	^ bounced
! !

!AtomMorph methodsFor: 'initialization' stamp: 'dgd 3/7/2003 14:13'!
defaultBorderWidth
"answer the default border width for the receiver"
	^ 0! !

!AtomMorph methodsFor: 'initialization' stamp: 'dgd 3/7/2003 14:13'!
defaultColor
"answer the default color/fill style for the receiver"
	^ Color blue! !

!AtomMorph methodsFor: 'drawing' stamp: 'jmv 6/16/2015 10:02'!
drawOn: aCanvas
	"Note: Set 'drawAsRect' to true to make the atoms draw faster. When testing the speed of other aspects of Morphic, such as its damage handling efficiency for large numbers of atoms, it is useful to make drawing faster."

	| drawAsRect |
	drawAsRect _ owner valueOfProperty: #drawAsRect.
	drawAsRect 
		ifNil: [super drawOn: aCanvas]
		ifNotNil: [
			drawAsRect 	"rectangles are faster to draw"
				ifTrue: [aCanvas fillRectangle: self morphLocalBounds color: color]
				ifFalse: [super drawOn: aCanvas]]! !

!AtomMorph methodsFor: 'accessing'!
infected

	^ color = Color red! !

!AtomMorph methodsFor: 'accessing' stamp: 'dhn 6/3/2015 17:57'!
infected: aBoolean

	(self valueOfProperty: #velocityDelta) ifNil: [
		aBoolean
			ifTrue: [self color: Color red]
			ifFalse: [self color: Color blue]]! !

!AtomMorph methodsFor: 'initialization' stamp: 'jmv 6/16/2015 09:58'!
initialize
	"Make a new atom with a random position and velocity."
	super initialize.
""
	self morphExtent: 8 @ 7.
	self
		randomPositionIn: (0 @ 0 corner: 300 @ 300)
		maxVelocity: 10! !

!AtomMorph methodsFor: 'testing' stamp: 'jmv 8/16/2015 11:40'!
is: aSymbol
	^aSymbol == #AtomMorph or: [ super is: aSymbol ]! !

!AtomMorph methodsFor: 'initialization' stamp: 'jmv 6/16/2015 09:57'!
randomPositionIn: aRectangle maxVelocity: maxVelocity
	"Give this atom a random position and velocity."

	| origin xtent |
	origin := aRectangle origin.
	xtent := (aRectangle extent - self morphExtent) rounded.
	self morphPosition:	
		(origin x + xtent x atRandom) @
		(origin y + xtent y atRandom).
	velocity :=
		(maxVelocity - (2 * maxVelocity) atRandom) @
		(maxVelocity - (2 * maxVelocity) atRandom).
! !

!AtomMorph methodsFor: 'updating' stamp: 'jmv 8/16/2015 11:41'!
redrawNeeded

	(owner is: #BouncingAtomsMorph) ifFalse: [
		super redrawNeeded ]! !

!AtomMorph methodsFor: 'accessing'!
velocity

	^ velocity! !

!AtomMorph methodsFor: 'accessing'!
velocity: newVelocity

	velocity := newVelocity.! !

!AtomMorph class methodsFor: 'examples' stamp: 'dhn 4/4/2015 17:41'!
example
	"
	AtomMorph example
	"
	|a b|
	a _ AtomMorph new openInWorld. 
	a color: Color random.
	b _ Display boundingBox.
 	[1000 timesRepeat:  [a bounceIn: b.  (Delay forMilliseconds: 20) wait]. 
	 a delete] fork.! !

!AtomMorph class methodsFor: 'new-morph participation' stamp: 'di 6/22/97 09:07'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !

!HeaterCoolerAtom methodsFor: 'private' stamp: 'dhn 6/17/2015 10:59'!
bounceIn: aRect

	^ super bounceIn: aRect
! !

!HeaterCoolerAtom methodsFor: 'initialization' stamp: 'dhn 10/7/2015 19:14'!
defaultColor
	"Set the color of the receiver based on property #velocityDelta"
	
	self valueOfProperty: #velocityDelta ifPresentDo: [:x |
	x < 0
		ifTrue: [^ Color white ]
		ifFalse: [^ Color lightRed]]! !

!HeaterCoolerAtom methodsFor: 'drawing' stamp: 'dhn 6/17/2015 18:02'!
drawOn: aCanvas

	super drawOn: aCanvas! !

!HeaterCoolerAtom methodsFor: 'initialization' stamp: 'dhn 10/7/2015 18:49'!
initialize
	| f x |
	f _ FillInTheBlankMorph
		request: '-100 < factor < 100 % Cooler/Hotter' 
		initialAnswer: '-10'.
	x _ f asNumber.
	(x > -100 and: x < 100) 
		ifTrue: [self setProperty: #velocityDelta toValue: x]
		ifFalse: [self setProperty: #velocityDelta toValue: 0].
	color _ self defaultColor.
	super initialize.
	self borderWidth: 1
! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 5/12/2016 10:50'!
asNormalizedPoints: aCollection
	"Answer an array of points which are mapped on [0, T] in both dimensions, where T is a multiple of 10"
	| yArray xArray result |
	
	aCollection size == 2 ifTrue: [
		self ticN: 1000.
		self startTime: Time localMillisecondClock.
		tics _ OrderedCollection new].
	((deltaT _ Time localMillisecondClock - self startTime) > self ticN) ifTrue: [
		self tics add: aCollection size.
		ticN _ ticN + 1000].
	yArray _ yFactor * (self normalize: aCollection asOrderedCollection with: yScaleFactor).
	xArray _ xFactor * (self normalize: (0 to: aCollection size) asArray).
	
	result _ OrderedCollection new.
	1 to: yArray size	do: [ :i | result add: (xArray at: i) @ (yArray at: i) ].
	
	^ result.! !

!InfectionGraph methodsFor: 'accessing' stamp: 'dhn 5/23/2015 16:09'!
data: aCollection

	data _ aCollection
! !

!InfectionGraph methodsFor: 'drawing' stamp: 'pb 7/18/2017 06:19:35'!
drawOn: aCanvas
	| fully |
	fully _ 10 raisedTo: yMax log asInteger.

	aCanvas line: (self toMorphic:0@yMin) to: (self toMorphic: 0 @ yMax) width: 2 color: Color lightGray.
	aCanvas line: (self toMorphic: xMin@0) to: (self toMorphic: xMax@0) width: 2 color: Color lightGray.
	aCanvas line: (self toMorphic: 0@fully) to: (self toMorphic: xMax@fully) width: 2 color: Color lightRed.

	points ifNotNil: [
		1 to: points size - 1 do: [:n | | p fromPoint toPoint |
			fromPoint := (self toMorphic: (p _ points at: n)).
			toPoint := (self toMorphic: (points at: n + 1)).
			aCanvas 	"continue the graph"
				line: fromPoint 	"from point"
				to: toPoint 	"to point"
				width: 2 color: Color black.
			(self tics includes: n) ifTrue: [
				aCanvas 	"plot the tic"
					line: (self toMorphic: (p x) @ 0) 
					to: (self toMorphic: (p x) @ (self class ticLength)) 
					width: 2 color: Color lightGray].
			hoverPoint ifNotNil: [|font hoverString|
				((hoverPoint x  >= fromPoint x) and: [hoverPoint x <= toPoint x])  ifTrue: [
					font := StrikeFont default.
					aCanvas
						line:  hoverPoint x@(self yToMorphic: yMin)
						to: hoverPoint x@fromPoint y
						width: 2
						color: Color green.
						hoverString := (p x truncateTo: 0.1) asString, ' @ ', (p y truncateTo: 0.1) asString.
					aCanvas
						drawString: hoverString
						at: (hoverPoint x - ((font widthOfString: hoverString)/2))@(fromPoint y- font height )
						font: font
						color: Color green
						embossed: true]]]]! !

!InfectionGraph methodsFor: 'event handling testing' stamp: 'pb 7/16/2017 15:35:43'!
handlesMouseHover
	^ true! !

!InfectionGraph methodsFor: 'event handling testing' stamp: 'pb 7/18/2017 06:11:48'!
handlesMouseOver: aMorphicEvent
	^ true! !

!InfectionGraph methodsFor: 'events' stamp: 'pb 7/16/2017 15:44:18'!
mouseHover: event localPosition: localPosition
	hoverPoint := localPosition.
	self redrawNeeded ! !

!InfectionGraph methodsFor: 'events' stamp: 'pb 7/18/2017 06:12:45'!
mouseLeave: evt
	hoverPoint := nil.
	self redrawNeeded ! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 5/23/2015 11:50'!
normalize: aCollection
	"Answer aCollection divided by its maximum"
	| max |
	max _ aCollection inject: 0 into: [:a :c | (a > c)
		ifTrue: [a]
		ifFalse: [c]].
	^ OrderedCollection new
		addAll: aCollection / (max * 1.0)! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 5/26/2015 20:09'!
normalize: aCollection with: aFactor
	"Answer aCollection divided by its maximum and scaled by maximum/aFactor"
	| max |
	max _ aCollection inject: 0 into: [:a :c | (a > c)
		ifTrue: [a]
		ifFalse: [c]].
	^ OrderedCollection new
		addAll: (aCollection / (max * 1.0)) * (max / aFactor)! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 6/19/2016 20:46'!
plotData
	"Re-normalize data"

	data size > 1 ifTrue: [
		points _ self asNormalizedPoints: data.
		self redrawNeeded]! !

!InfectionGraph methodsFor: 'accessing' stamp: 'dhn 6/11/2016 12:13'!
resumed: anObject
	"Set the value of resumed"

	resumed _ anObject! !

!InfectionGraph methodsFor: 'initialization' stamp: 'dhn 5/11/2016 21:17'!
setXYFactors
	"Set the values of xFactor and yFactor"

	xFactor _ 10 raisedTo: xMax log asInteger.
	yFactor _ 10 raisedTo: yMax log asInteger
! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 5/12/2016 10:39'!
startTime
	"Answer the value of startTime"
	
	startTime ifNil: [startTime _ Time localMillisecondClock].
	^ startTime! !

!InfectionGraph methodsFor: 'accessing' stamp: 'dhn 5/11/2016 02:06'!
startTime: anObject
	"Set the value of startTime"

	startTime _ anObject! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 5/12/2016 10:49'!
ticN
	"Answer the value of ticN"
	
	ticN ifNil: [ticN _ 1000].
	^ ticN! !

!InfectionGraph methodsFor: 'accessing' stamp: 'dhn 5/12/2016 10:15'!
ticN: anObject
	"Set the value of ticN"

	ticN _ anObject! !

!InfectionGraph methodsFor: 'drawing' stamp: 'dhn 5/12/2016 10:19'!
tics
	"Answer the value of tics"
	
	tics ifNil: [tics _ OrderedCollection new].
	^ tics! !

!InfectionGraph methodsFor: 'accessing' stamp: 'dhn 5/26/2015 12:25'!
yScaleFactor: aNumber

	yScaleFactor _ aNumber
! !

!InfectionGraph class methodsFor: 'display' stamp: 'dhn 5/11/2016 16:01'!
ticLength
	"Answer the length of tic marks"
	
	^ 0.2! !

!BouncingAtomsMorph methodsFor: 'other' stamp: 'jmv 6/16/2015 10:02'!
addAtoms: n
	"Add a bunch of new atoms."

	nAtoms _ n.
	n timesRepeat: [
		| a |
		a _ AtomMorph new.
		self addMorph: a.
		a randomPositionIn: self morphLocalBounds maxVelocity: 10.].
	historyWindow ifNotNil: [historyWindow yScaleFactor: n].
	self stopStepping.
! !

!BouncingAtomsMorph methodsFor: 'submorphs-add/remove'!
addMorphFront: aMorph
	"Called by the 'embed' meta action. We want non-atoms to go to the back."
	"Note: A user would not be expected to write this method. However, a sufficiently advanced user (e.g, an e-toy author) might do something equivalent by overridding the drag-n-drop messages when they are implemented."

	(aMorph isMemberOf: AtomMorph)
		ifTrue: [super addMorphFront: aMorph]
		ifFalse: [super addMorphBack: aMorph].! !

!BouncingAtomsMorph methodsFor: 'other' stamp: 'dhn 10/7/2015 18:29'!
collisionPairs
	"Return a list of pairs of colliding atoms, which are assumed to be
circles of known radius. This version uses the morph's positions--i.e.
the top-left of their bounds rectangles--rather than their centers."

	| count sortedAtoms radius twoRadii radiiSquared collisions p1 continue j p2 distSquared m1 m2 |
	count := submorphs size.
	sortedAtoms := submorphs 
				asSortedCollection: [:mt1 :mt2 | mt1 morphPosition x < mt2 morphPosition x].
	radius := 8.
	twoRadii := 2 * radius.
	radiiSquared := radius squared * 2.
	collisions := OrderedCollection new.
	1 to: count - 1
		do: 
			[:i | 
			m1 := sortedAtoms at: i.
			p1 := m1 morphPosition.
			continue := (j := i + 1) <= count.
			[continue] whileTrue: [
				m2 := sortedAtoms at: j.
				p2 := m2 morphPosition.
				continue := p2 x - p1 x <= twoRadii  
					ifTrue: [
						distSquared := (p1 x - p2 x) squared + (p1 y - p2 y) squared.
						distSquared < radiiSquared 
							ifTrue: [
								((m1 class == HeaterCoolerAtom) and: m2 class == HeaterCoolerAtom)
									ifFalse: [
										collisions add: (Array with: m1 with: m2).
										m1 valueOfProperty: #velocityDelta ifPresentDo: [:x |
											m2 velocity:  m2 velocity * (100 + x) / 100.0].
										m2 valueOfProperty: #velocityDelta ifPresentDo: [:x |
											m1 velocity:  m1 velocity * (100 + x) / 100.0]]].
								(j := j + 1) <= count]
							ifFalse: [false]]].
	^collisions! !

!BouncingAtomsMorph methodsFor: 'initialization' stamp: 'dgd 3/7/2003 14:14'!
defaultColor
"answer the default color/fill style for the receiver"
	^ Color
		r: 0.8
		g: 1.0
		b: 0.8! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'dhn 5/20/2015 11:08'!
drawAsRect
	"Toggle the #drawAsRect property"
	self 
		setProperty: #drawAsRect 
		toValue: (self valueOfProperty: #drawAsRect) not
! !

!BouncingAtomsMorph methodsFor: 'drawing' stamp: 'jmv 8/15/2015 19:50'!
drawOn: aCanvas
	super drawOn: aCanvas.
	fpsEnabled ifTrue: [
		aCanvas fillRectangle: (10@10 extent: 20@15) color: Color lightGray.
		aCanvas
			drawString: fps asString
			at: 10 @ 10
			font: nil
			color: Color black ].! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'dhn 4/23/2015 18:05'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!BouncingAtomsMorph methodsFor: 'initialization' stamp: 'dhn 6/11/2016 11:26'!
initialize
	"initialize the state of the receiver"
	super initialize.
	fpsEnabled _ false.
	fps _ 0.
	fpsCurrentFrameCount _ 0.
	fpsCurrentFrameCountTime _ 0.
	resume _ true.
	self reset.! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'dhn 6/3/2016 19:45'!
installHeaterCooler
	"Replace a regular atom with a heater/cooler atom"
	| non x |

	non _ self submorphs select: [:a | (a hasProperty: #velocityDelta) not].
	non ifNotEmpty: [	
		
		non shuffled first delete.	"Take away an atom"
		nAtoms _ nAtoms - 1.

		x _ HeaterCoolerAtom new.
		x randomPositionIn: self morphLocalBounds maxVelocity: 10.
		self addMorph: x]	"Add an atom"
! !

!BouncingAtomsMorph methodsFor: 'testing' stamp: 'jmv 8/16/2015 11:41'!
is: aSymbol
	^aSymbol == #BouncingAtomsMorph or: [ super is: aSymbol ]! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'dhn 6/11/2016 11:41'!
mouseButton2Activity
	"Show a pop-up menu"
	| tuples list index |
	
	tuples _ #(
		#('Set Atom Count...' 			#setAtomCount) 
		#('Start Infection' 				#startInfection) 
		#('Show Infection History' 		#showInfectionHistory) 
		#('Toggle Atom as Rectangle' 	#drawAsRect) 
		#('Heater/Cooler Atom' 		#installHeaterCooler) 
		#('Toggle FPS' 					#toggleFPS) 
		#('Pause/Resume' 				#pauseResume)).
		
	list _ tuples collect: [ :ea | ea first ].
	index _ (PopUpMenu labelArray: list) startUpWithCaption: 'Bouncing Atoms'.
	index > 0 ifTrue: [ self perform: ((tuples at: index) at: 2) ].! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'dhn 6/11/2016 11:37'!
pauseResume
	"Toggle the action"
	
	resume _ resume not! !

!BouncingAtomsMorph methodsFor: 'other' stamp: 'dhn 6/19/2016 18:50'!
reportInfection
	"Produce a graph of infection history"
	| count |
	self collisionPairs do: [:pair | | infected |
		infected := false.
		pair do: [:atom | atom infected ifTrue: [infected := true]].
		infected
			ifTrue: [pair do: [:atom | atom infected: true]]].

	count := 0.
	self submorphsDo: [:m | m infected ifTrue: [count := count + 1]].
	count > 0 ifTrue: [infectionHistory addLast: count].
	historyWindow ifNotNil: [
		historyWindow 
			data: infectionHistory;
			yScaleFactor: nAtoms;
			plotData].
	count = nAtoms ifTrue: [
		transmitInfection := false.
		self stopStepping].
! !

!BouncingAtomsMorph methodsFor: 'initialization' stamp: 'jmv 6/16/2015 09:58'!
reset
	infectionHistory := OrderedCollection new: 500.
	infectionHistory add: 0.
	transmitInfection _ false.
! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'jmv 10/24/2015 23:30'!
setAtomCount

	| countString count |
	countString _ FillInTheBlankMorph 
		request: 'Number of atoms?' 
		initialAnswer: self submorphCount printString.
	countString isEmpty ifTrue: [^ self].
	self removeAllMorphs.
	count _ countString asInteger.
	self addAtoms: count.
	self reset.
	self startStepping.! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'jmv 1/2/2017 09:09:53'!
showInfectionHistory
	"Show a graph of the infection history"
	historyWindow _ InfectionGraph new.
	historyWindow 
		data: infectionHistory;
		yScaleFactor: nAtoms;
		domain: (0 to: 10);
		yRange: (0 to: 10);
		setXYFactors;
		plotData.
	(historyWindow embeddedInMorphicWindowLabeled: 'Infection History') openInWorld
! !

!BouncingAtomsMorph methodsFor: 'menu' stamp: 'dhn 6/3/2016 19:45'!
startInfection
	"Introduce one infected atom in the collection"
	| non |
	
	non _ self submorphs select: [:a | (a hasProperty: #velocityDelta) not].
	non ifNotEmpty: [	
		non do: [:ea | ea infected: false].	"disinfect"
		non shuffled first infected: true].	"randomly infect one"
	
	infectionHistory := OrderedCollection new: 500.
	infectionHistory add: 0.
	transmitInfection := true.
	self startStepping.
! !

!BouncingAtomsMorph methodsFor: 'stepping and presenter' stamp: 'dhn 6/11/2016 12:13'!
stepAt: millisecondSinceLast
	"Bounce those atoms!!"
	| r bounces |
	
	historyWindow ifNotNil: [historyWindow resumed: resume].
	
	resume ifTrue: [
		bounces := 0.
		r := 0 @ 0 corner: self morphExtent - (8 @ 8).
		self submorphsDo: [ :m |
			(m is: #AtomMorph) ifTrue: [
				(m bounceIn: r) ifTrue: [ bounces := bounces + 1 ]]].
		"compute a 'temperature' that is proportional to the number of bounces
		 divided by the circumference of the enclosing rectangle"
		self updateTemperature: 10000.0 * bounces / (r width + r height).
		transmitInfection ifTrue: [ self reportInfection ].
		fpsEnabled ifTrue: [
			fpsCurrentFrameCountTime < 1000
				ifTrue: [
					fpsCurrentFrameCount := fpsCurrentFrameCount + 1.
					fpsCurrentFrameCountTime _ fpsCurrentFrameCountTime + millisecondSinceLast.
					]
				ifFalse: [
					fps := fpsCurrentFrameCount.
					fpsCurrentFrameCount := 1.
					fpsCurrentFrameCountTime _ 0 ]].
		self redrawNeeded]
	
! !

!BouncingAtomsMorph methodsFor: 'testing' stamp: 'pb 7/16/2015 21:07'!
stepTime
	"desired step time in milliseconds: 0 to update as often as possible, 17 for ~60fps, 33 for ~30fps.  Note: at 30fps and above, Morphic will not reliably match the requested framerate"
	^ 33.! !

!BouncingAtomsMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 8/15/2015 21:15'!
submorphsDrawingOutsideReverseDo: aBlock
	"All our submorphs are inside us"! !

!BouncingAtomsMorph methodsFor: 'as yet unclassified' stamp: 'pb 7/16/2015 19:59'!
toggleFPS
	fpsEnabled _ fpsEnabled not! !

!BouncingAtomsMorph methodsFor: 'other' stamp: 'dgd 2/22/2003 13:36'!
updateTemperature: currentTemperature 
	"Record the current temperature, which is taken to be the number of atoms that have bounced in the last cycle. To avoid too much jitter in the reading, the last several readings are averaged."

	recentTemperatures isNil 
		ifTrue: 
			[recentTemperatures := OrderedCollection new.
			20 timesRepeat: [recentTemperatures add: 0]].
	recentTemperatures removeLast.
	recentTemperatures addFirst: currentTemperature.
	temperature := recentTemperatures sum asFloat / recentTemperatures size! !

!BouncingAtomsMorph methodsFor: 'stepping' stamp: 'pb 7/2/2015 01:00'!
wantsSteps
	^ true! !

!BouncingAtomsMorph class methodsFor: 'class initialization' stamp: 'dhn 11/7/2015 20:34'!
how
"
	Start from World->New morph... menu or:
	BouncingAtomsMorph open.
"! !

!BouncingAtomsMorph class methodsFor: 'class initialization' stamp: 'jmv 5/10/2016 09:40'!
initializedInstance
	"Establish the window with some bouncing atoms"
	| win morph |
	
	morph _ self new.
	win _ morph embeddedInMorphicWindowLabeled: 'Bouncing Atoms'.
	win setProperty: #minimumExtent toValue: 88@85.
	morph setProperty: #drawAsRect toValue: false.
	
	"ensure good initial atom distribution and movement"
	morph morphExtent: win morphExtent.
	morph addAtoms: 30.
	
	^ win! !

!BouncingAtomsMorph class methodsFor: 'class initialization' stamp: 'dhn 11/7/2015 20:34'!
open
"
	Start from World->New morph... menu or:
	BouncingAtomsMorph open.
"

	self initializedInstance openInHand! !
