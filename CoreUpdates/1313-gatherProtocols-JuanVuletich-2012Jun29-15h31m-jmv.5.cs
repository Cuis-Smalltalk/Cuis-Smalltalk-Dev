'From Cuis 4.0 of 21 April 2012 [latest update: #1312] on 29 June 2012 at 4:50:25 pm'!
!classDefinition: 'Object class' category: #'Kernel-Objects'!
Object class
	instanceVariableNames: 'protocols '!

!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 6/29/2012 16:48'!
statePointsTo: anObject
	"Answers true if anObject is among my named or indexed instance variables, and false otherwise"

	<primitive: 132>
	1 to: self class instSize do: [ :i |
		(self instVarAt: i) == anObject ifTrue: [ ^ true ]].
	1 to: self basicSize do: [ :i |
		(self basicAt: i) == anObject ifTrue: [ ^ true ]].
	^ false! !


!Object class methodsFor: 'class initialization' stamp: 'jmv 6/29/2012 16:38'!
initializeProtocols

	protocols _ self gatherProtocols! !

!Object class methodsFor: 'class initialization' stamp: 'jmv 6/29/2012 16:11'!
protocols

	^protocols! !

!Object class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 16:38'!
gatherProtocols
	"See comment at #is:"

	^#()! !

!Object class methodsFor: 'compiling' stamp: 'jmv 6/29/2012 16:28'!
noteCompilationOf: aSelector meta: isMeta
	"A hook allowing some classes to react to recompilation of certain selectors"

	aSelector == #gatherProtocols ifTrue: [
		self withAllSubclassesDo: [ :cls |
			cls isMeta ifFalse: [ 
				cls initializeProtocols ]]]! !


!Array class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:48'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Array)! !


!CodeProvider class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 16:08'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(CodeProvider #ShoutEnabled dynamic)! !


!Color class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:49'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Color)! !


!CompiledMethod class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:48'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(CompiledMethod)! !


!FloatArray class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:48'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(FloatArray)! !


!Form class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:49'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Form)! !


!ColorForm class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:49'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(ColorForm)! !


!Cursor class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:50'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Cursor)! !


!Matrix class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:50'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Matrix)! !


!MessageSend class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:50'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(MessageSend)! !


!Morph class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:51'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Morph)! !


!BorderedMorph class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:51'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(BorderedMorph)! !


!HandMorph class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:52'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(HandMorph)! !


!InnerTextMorph class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:53'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(InnerTextMorph)! !


!LayoutMorph class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:52'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(LayoutMorph)! !


!MorphicEvent class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:53'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(MorphicEvent)! !


!PluggableScrollPane class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:51'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(ScrollPane)! !


!PluggableTextModel class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 16:08'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(hasTextProvider dynamic)! !


!Stream class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:53'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Stream)! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/29/2012 16:34'!
allProtocols
	"Protocols that #is: can test for.  Note: for classes that implement a more dynamic #is: (see PluggableTextModel) care should be taken to ensure that there protocols are not defined that don't get listed.

	Smalltalk allProtocols
	"
	| all |
	all _ Set new.
"	Smalltalk allClasses do: [ :cls|"
	Object withAllSubclassesDo: [ :cls |
		cls isMeta ifFalse: [
			all addAll: cls protocols ]].
	^ all! !


!SystemWindow class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:52'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(SystemWindow)! !


!Text class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:49'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(Text)! !


!WeakMessageSend class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:54'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(MessageSend)! !


!Workspace class methodsFor: 'instance protocol testing' stamp: 'jmv 6/29/2012 15:47'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(ShoutEnabled)! !


!ProtoObject methodsFor: 'tracing' stamp: 'jmv 6/29/2012 16:48'!
pointsTo: anObject
"Answers true if I hold a reference to anObject, or false otherwise. Or stated another way:

Answers true if the garbage collector would fail to collect anObject because I hold a reference to it, or false otherwise"

	^ (self statePointsTo: anObject)
		or: [ self class == anObject ]! !


!Object methodsFor: 'testing' stamp: 'jmv 6/29/2012 16:49'!
is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	Suggested by Igor Stasenko at
	http://lists.squeakfoundation.org/pipermail/squeak-dev/2009-June/136793.html
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^self class protocols statePointsTo: aSymbol! !

!Object methodsFor: 'tracing' stamp: 'jmv 6/29/2012 16:48'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts. Adapted from PointerFinder class >> #pointersTo:except:"

	| object lastObject pointers objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers := OrderedCollection new: 1000.

	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object := self someObject.
	lastObject _ Object new.
	[lastObject == object] whileFalse: [
		object isInMemory
			ifTrue: [((object statePointsTo: self)
				or: [object class == self])
					ifTrue: [pointers add: object]].
		object := object nextObject].

	objectsToAlwaysExclude := {
		pointers collector.
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.

	^ pointers removeAllSuchThat: [:ea |
		(objectsToAlwaysExclude identityIncludes: ea)
			or: [objectsToExclude identityIncludes: ea]]! !


!CodeProvider methodsFor: 'testing' stamp: 'jmv 6/29/2012 16:02'!
is: aSymbol

	aSymbol = #refusingToAccept
		ifTrue: [ ^self isRefusingToAccept ].
	^super is: aSymbol! !


!MethodDictionary methodsFor: 'accessing' stamp: 'jmv 6/29/2012 16:48'!
includesKey: aSymbol
	"This override assumes that statePointsTo: uses a fast primitive"

	aSymbol ifNil: [^ false].
	^ self statePointsTo: aSymbol! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 6/29/2012 16:01'!
is: aSymbol

	aSymbol == #ShoutEnabled ifTrue: [ ^textProvider is: aSymbol ].
	^ super is: aSymbol! !


!SHParserST80 methodsFor: 'testing' stamp: 'jmv 6/29/2012 16:49'!
isMessage: aSymbol

	^#(binary incompleteBinary keyword incompleteKeyword unary incompleteUnary) statePointsTo: aSymbol! !

!SHParserST80 methodsFor: 'testing' stamp: 'jmv 6/29/2012 16:49'!
isPartialOrFullIdentifier: aSymbol

	(#(#incompleteIdentifier
		#blockTempVar #blockArg #tempVar #methodArg
		#instVar #classVar 
		#workspaceVar #poolConstant #globalVar ) 
			statePointsTo:aSymbol) ifTrue: [ ^ true ].
	(self isReservedName: aSymbol) ifTrue: [ ^ true ].
	^ false! !


!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 6/29/2012 15:38'!
knownInitialsAndNames
	"This list could include people who hasn't contributed code to the Cuis image, but some optional package."
"
| all ok |
all _ Smalltalk allContributors asSet.
ok _ (Smalltalk contributorInitialsAndNames collect: [ :pair | pair first ]) asSet.
self assert: all = ok

initials         name"
^ #(
	#('ab' 					'Alexandre Bergel')
	#('abc' 					'Colin Putney')
	#('acg' 					'Andrew C. Greenberg')
	#('ads' 					'Adam Spitz')
	#('AFi' 					'Alain Fischer')
	#('ajh' 					'Anthony Hannan')
	#('al' 					'Adrian Lienhard')
	#('aoy' 					'Andres Otaduy')
	#('apb' 					'Andrew P. Black')
	#('ar' 					'Andreas Raab')
	#('asm' 				'Alejandro Magistrello')
	#('avi' 					'Avi Bryant')
	#('bf' 					'Bert Freudenberg')
	#('BG' 					'Boris Gaertner')
	#('BJP' 					'Bijan Parsia')
	#('bkv' 					'Brent Vukmer')
	#('bolot' 				'Bolot Kerimbaev')
	#('bp' 					'Bernhard Pieber')
	#('BP' 					'Brent Pinkney') 
	#('brp' 					'Brent Pinkney')
	#('cbc' 					'Chris Cunningham')
	#('cbr'					'Casey Ransberger')
	#('ccn' 					'Chris Norton')
	#('cmm' 				'Chris Muller')
	#('crl' 					'Craig Latta')
	#('cwp' 				'Colin Putney')
	#('das' 					'David A Smith')
	#('dc' 					'Damien Cassou')
	#('dew' 				'Doug Way')
	#('dgd' 				'Diego Gomez Deck')
	#('dkh'					'Dale Henrichs')
	#('dhhi' 				'Dan Ingalls')
	#('di' 					'Dan Ingalls')
	#('djp' 					'David J. Pennell')
	#('DKL'					'Daniel K Lyons ')
	#('DSM' 				'Duane Maxwell')
	#('DSG'					'David Graham')
	#('dtl' 					'Dave Lewis')
	#('dvf' 					'Daniel Vainsencher')
	#('eat' 					'Eric Arseneau Tremblay')
	#('eem'					'Eliot Emilio Miranda')
	#('efc' 					'Eddie Cottongim')
	#('em' 					'Ernest Micklei?')
	#('emm' 				'Ernest Micklei')
	#('fbs' 					'Frank Shearar')
	#('FBS' 					'Frank Shearar')
	#('fc' 					'Frank Caggiano')
	#('fcs' 					'Frank Sergeant')
	#('FernandoOlivero' 	'Fernando Olivero')
	#('FernanodOlivero' 	'Fernando Olivero')
	#('GabrielOmarCotelli' 	'Gabriel Omar Cotelli')
	#('gh' 					'Goran Krampe (nee Hultgren)')
	#('gk' 					'Goran Krampe (nee Hultgren)')
	#('gm' 					'German Morales')
	#('go' 					'Georg Gollmann')
	#('gsa' 					'German Arduino')
	#('hmm' 				'Hans-Martin Mosner')
	#('hsj' 					'Henrik Sperre Johansen')
	#('Igor.Stasenko' 		'Igor Stasenko')
	#('ikp' 					'Ian Piumarta')
	#('Jb' 					'Jean Baptiste Arnaud')
	#('jcg' 					'Joshua Gargus')
	#('jdr' 					'Javier Diaz-Reinoso')
	#('je' 					'Joern Eyrich')
	#('jf' 					'Julian Fitzell')
	#('JF' 					'Julian Fitzell')
	#('jhm' 					'John Maloney')
	#('jlb' 					'Jim Benson')
	#('jm' '					John Maloney')
	#('jmb' 					'Hans Baveco')
	#('JMG'					'Jeff Gonis')
	#('JMM' 				'John McIntosh')
	#('jmv' 					'Juan Vuletich')
	#('JMV' 					'Juan Vuletich')
	#('jp' 					'Joseph Pelrine')
	#('jrm' 					'John-Reed Maffeo')
	#('jrp' 					'John Pierce')
	#('jsp' 					'Jeff Pierce')
	#('kfr' 					'Karl Ramberg')
	#('KLC'			 		'Ken Causey')
	#('kph'					'Keith Hodges')
	#('KTT' 				'Kurt Thams')
	#('laza' 				'Alexander Lazarevic')
	#('LC' 					'Leandro Caniglia')
	#('len' 					'Luciano Esteban Notarfrancesco')
	#('lr' 					'Lukas Renggli')
	#('Lukas Renggli' 		'Lukas Renggli')
	#('ls' 					'Lex Spoon')
	#('md' 					'Marcus Denker')
	#('MarcusDenker' 		'Marcus Denker')
	#('marcus.denker' 		'Marcus Denker')
	#('mdr' 				'Mike Rutenberg')
	#('mga' 				'Markus Galli')
	#('mha' 				'Michael Haupt')
	#('mir' 					'Michael Rueger')
	#('mjg' 					'Mark Guzdial')
	#('mk' 					'Matej Kosik')
	#('MPH' 				'Michael Hewner')
	#('mpw' 				'Marcel Weiher')
	#('MPW' 				'Marcel Weiher')
	#('mrm' 				'Martin McClure')
	#('mtf' 					'Matthew Fulmer')
	#('mu' 					'Masashi Umezawa')
	#('nb' 					'Naala Brewer')
	#('nice'				 	'Nicolas Cellier')
	#('nk' 					'Ned Konz')
	#('nop' 					'Jay Carlson')
	#('NS' 					'Nathanael Schaerli')
	#('panda' 				'Michael Rueger')
	#('pb'					'Phil Bellalouna')
	#('PHK' 				'Peter Keeler')
	#('Pmm' 				'Philippe Marschall')
	#('pnm' 				'Paul McDonough')
	#('r++' 				'Gerardo Richarte')
	#('raa' 					'Bob Arning')
	#('RAA' 					'Bob Arning')
	#('raok' 				'Richard A. O''Keefe')
	#('rca' 					'Russell Allen')
	#('reThink'			 	'Paul McDonough')
	#('rew' 					'Roger Whitney')
	#('rhi' 					'Robert Hirschfeld')
	#('rr' 					'Romain Robbes')
	#('rss' 					'Ron Spengler')
	#('rw' 					'Robert Withers')
	#('rww' 				'Robert Withers')
	#('Sames' 				'Samuel S. Shuster')
	#('sbw' 				'Stephan B. Wessels')
	#('sd' 					'Stephane Ducasse')
	#('SD' 					'Stephane Ducasse')
	#('sge' 					'Steve Elkins')
	#('sma' 				'Stefan Matthias Aust')
	#('sps' 					'Steven Swerling')
	#('SqR' 					'Andres Valloud')
	#('sr' 					'Stephan Rudlof')
	#('SSS' 				'Samuel S. Shuster')
	#('stephane.ducasse' 	'Stephane Ducasse')
	#('stephaneducasse' 	'Stephane Ducasse')
	#('stp' 					'Stephen Travis Pope')
	#('sumim' 				'Masato Sumi')
	#('svp' 					'Stephen Vincent Pair')
	#('sw' 					'Scott Wallace')
	#('TAG' 				'Travis Griggs')
	#('tak' 					'Takashi Yamamiya')
	#('tao' 					'Tim Olson')
	#('TBn' 					'Torsten Bergmann')
	#('tfei' 					'The Fourth Estate, Inc.')
	#('tfel' 					'Tim Felgentreff')
	#('th' 					'Torge Husfeldt')
	#('tk' 					'Ted Kaehler')
	#('tlk' 					'Tom Koenig')
	#('tpr' 					'Tim Rowledge')
	#('TPR' 					'Tim Rowledge')
	#('tween' 				'Andy Tween')
	#('ul' 					'Levente Uzonyi')
	#('vb' 					'Vassili Bykov')
	#('ward' 				'Ward Cunningham')
	#('wiz' 					'Jerome Peace')
	#('wod' 				'Bill Dargel')
	#('yo' 					'Yoshiki Ohshima')
	#('zz' 					'Serge Stinckwich'))! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/29/2012 16:32'!
allClasses  
	"Return all the class defines in the Smalltalk SystemDictionary"
	"
	Smalltalk allClasses
	"

	^ self classNames collect: [:name | self at: name]! !


!Workspace class reorganize!
('window color' windowColor)
('instance protocol testing' gatherProtocols)
!

!methodRemoval: Workspace #is:!
Workspace removeSelector: #is:!

!WeakMessageSend class reorganize!
('instance creation' new receiver:selector: receiver:selector:argument: receiver:selector:arguments:)
('instance protocol testing' gatherProtocols)
!

!methodRemoval: WeakMessageSend #is:!
WeakMessageSend removeSelector: #is:!

!Text class reorganize!
('instance creation' fromString: fromUser initialFont:string:attribute: initialFont:stringOrText: new: streamContents: string: string:attribute: string:attributes: withForm:)
('private' addAttribute:toArray: setParagraphAttributes:toArray: string:runs:)
('instance protocol testing' gatherProtocols)
!

!methodRemoval: Text #is:!
Text removeSelector: #is:!

!SystemWindow class reorganize!
('top window' closeTopWindow noteTopWindowIn: sendTopWindowToBack topWindow windowsIn:satisfying:)
('instance creation' editText:label:wrap: open: open:label:)
('instance protocol testing' gatherProtocols)
!

!methodRemoval: SystemWindow #is:!
SystemWindow removeSelector: #is:!

!Stream class reorganize!
('instance creation' new)
('instance protocol testing' gatherProtocols)
!

!methodRemoval: Stream #is:!
Stream removeSelector: #is:!

!PluggableTextModel class reorganize!
('instance creation' on:)
('instance protocol testing' gatherProtocols)
!


!PluggableScrollPane class reorganize!
('instance protocol testing' gatherProtocols)
!

!methodRemoval: PluggableScrollPane #is:!
PluggableScrollPane removeSelector: #is:!

!MorphicEvent class reorganize!
('instance creation' readFrom: type:readFrom:)
('instance protocol testing' gatherProtocols)
!


!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newRow)
('examples' example1 example11 example13 example2 example3 example4 example5 example6)
('instance protocol testing' gatherProtocols)
!


!InnerTextMorph class reorganize!
('new-morph participation' includeInNewMorphMenu)
('instance protocol testing' gatherProtocols)
!


!HandMorph class reorganize!
('accessing' doubleClickTime doubleClickTime:)
('class initialization' initialize)
('new-morph participation' includeInNewMorphMenu)
('utilities' fastDragBorderWidth)
('instance protocol testing' gatherProtocols)
!


!BorderedMorph class reorganize!
('instance protocol testing' gatherProtocols)
!


!Morph class reorganize!
('class initialization' initialize)
('initialize-release' unload)
('instance creation' initializedInstance)
('new-morph participation' includeInNewMorphMenu)
('instance protocol testing' gatherProtocols)
!


!MessageSend class reorganize!
('instance creation' receiver:selector: receiver:selector:argument: receiver:selector:arguments:)
('instance protocol testing' gatherProtocols)
!


!Matrix class reorganize!
('instance creation' columnFrom: fromArrayOfArrays: identity: m:n: newColumnVectorSize: newHeight:width: newIdentity: newRowVectorSize: newSize: newVectorSize: rowFrom:)
('instance protocol testing' gatherProtocols)
!


!Cursor class reorganize!
('class initialization' initBottomLeft initBottomRight initCorner initCrossHair initDown initMarker initMenu initMove initNormal initNormalWithMask initOrigin initRead initResizeLeft initResizeTop initResizeTopLeft initResizeTopRight initRightArrow initSquare initTarget initTopLeft initTopRight initUp initWait initWrite initXeq initialize makeCursorsWithMask startUp)
('instance creation' extent:fromArray:offset: new resizeForEdge:)
('current cursor' currentCursor currentCursor:)
('constants' blank bottomLeft bottomRight corner crossHair down execute marker menu move normal origin read resizeBottom resizeBottomLeft resizeBottomRight resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight rightArrow square target topLeft topRight up wait webLink write)
('instance protocol testing' gatherProtocols)
!


!ColorForm class reorganize!
('as yet unclassified' mappingWhiteToTransparentFrom: twoToneFromDisplay:using:backgroundColor:)
('instance protocol testing' gatherProtocols)
!


!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:fromStipple: extent:offset: fromBinaryStream: fromDisplay: fromDisplay:using: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' exampleBorder exampleEdits exampleMagnify exampleShrink exampleSketch exampleSpaceFill makeStar toothpaste: xorHack:)
('shut down' shutDown)
('initialize-release' initialize)
('fileIn/Out' fileReaderServicesForFile:suffix: openAsBackground: serviceImageAsBackground services)
('class initialization' unload)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
('instance protocol testing' gatherProtocols)
!


!FloatArray class reorganize!
('instance protocol testing' gatherProtocols)
!


!CompiledMethod class reorganize!
('class initialization' fullFrameSize initialize smallFrameSize)
('instance creation' basicNew: new new: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:flag: newFrom: newInstanceFrom:variable:size:map: newMethod:header: primitive:numArgs:numTemps:stackSize:literals:bytecodes:trailer: toReturnConstant:trailerBytes: toReturnField:trailerBytes: toReturnSelf toReturnSelfTrailerBytes:)
('constants' abstractMarkers conflictMarker disabledMarker explicitRequirementMarker implicitRequirementMarker subclassResponsibilityMarker)
('services' unboundMethods)
('evaluating' receiver:withArguments:executeMethod:)
('instance protocol testing' gatherProtocols)
!


!Color class reorganize!
('instance creation' colorFrom: colorFromPixelValue:depth: fromArray: fromRgbTriplet: fromString: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: new r:g:b: r:g:b:alpha: r:g:b:range: random random2)
('class initialization' initialize initializeGrayToIndexMap initializeHighLights initializeIndexedColors initializeNames initializeTranslucentPatterns named:put:)
('examples' colorRampForDepth:extent: experimentsTowarsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' black blue brown cyan darkGray darktan gray green lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightRed lightYellow magenta orange paleBlue paleBuff paleGreen paleMagenta paleOrange palePeach paleRed paleTan paleYellow red tan transparent veryDarkGray veryLightGray veryPaleRed veryVeryDarkGray veryVeryLightGray white yellow)
('colormaps' cachedColormapFrom:to: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapFrom:to: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor:)
('other' colorNames indexedColors maskingMap: pixelScreenForDepth: quickHighLight: shutDown translucentMaskFor:depth:)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper: fromUser oldColorPaletteForDepth:extent:)
('instance creation - css' css2NamedColors css2NamedColors1 css2NamedColors2 fromCSS2String:)
('instance protocol testing' gatherProtocols)
!


!CodeProvider class reorganize!
('instance protocol testing' gatherProtocols)
!


!Array class reorganize!
('brace support' braceStream: braceWith: braceWith:with: braceWith:with:with: braceWith:with:with:with: braceWithNone)
('instance protocol testing' gatherProtocols)
!

!classDefinition: 'Object class' category: #'Kernel-Objects'!
Object class
	instanceVariableNames: 'protocols'!

!Object class reorganize!
('instance creation' initializedInstance newFrom: readFrom: unStream:)
('documentation' howToModifyPrimitives whatIsAPrimitive)
('object serialization' createFrom:size:version:)
('class initialization' initialize initializeProtocols protocols)
('windowColor' windowColor)
('instance protocol testing' gatherProtocols)
('compiling' noteCompilationOf:meta:)
!

!methodRemoval: MorphicEvent #is:!
MorphicEvent removeSelector: #is:!
!methodRemoval: LayoutMorph #is:!
LayoutMorph removeSelector: #is:!
!methodRemoval: InnerTextMorph #is:!
InnerTextMorph removeSelector: #is:!
!methodRemoval: HandMorph #is:!
HandMorph removeSelector: #is:!
!methodRemoval: BorderedMorph #is:!
BorderedMorph removeSelector: #is:!
!methodRemoval: Morph #is:!
Morph removeSelector: #is:!
!methodRemoval: MessageSend #is:!
MessageSend removeSelector: #is:!
!methodRemoval: Matrix #is:!
Matrix removeSelector: #is:!
!methodRemoval: Cursor #is:!
Cursor removeSelector: #is:!
!methodRemoval: ColorForm #is:!
ColorForm removeSelector: #is:!
!methodRemoval: Form #is:!
Form removeSelector: #is:!
!methodRemoval: FloatArray #is:!
FloatArray removeSelector: #is:!
!methodRemoval: CompiledMethod #is:!
CompiledMethod removeSelector: #is:!
!methodRemoval: Color #is:!
Color removeSelector: #is:!
!methodRemoval: Array #is:!
Array removeSelector: #is:!
!methodRemoval: ProtoObject #instVarsInclude:!
ProtoObject removeSelector: #instVarsInclude:!
