'From Cuis 4.0 of 21 April 2012 [latest update: #1278] on 7 May 2012 at 10:10:48 pm'!

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 5/7/2012 22:09'!
contributorInitialsAndNames
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
	#('dhhi' 				'Dan Ingalls')
	#('di' 					'Dan Ingalls')
	#('djp' 					'David J. Pennell')
	#('DSM' 				'Duane Maxwell')
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


!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 5/7/2012 16:16'!
hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [(aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]]]
			ifFalse: [propertyOrPragma hasLiteralSuchThat: aBlock]) ifTrue: [^true]].
	^false! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/7/2012 21:34'!
at: aKey put: aValue
	"Replace the property value or pragma associated with aKey."

	| keyAlreadyExists |
	keyAlreadyExists _ false.
	
	1 to: self basicSize do: [ :i |
		| propertyOrPragma "<Association|Pragma>" |
		(propertyOrPragma _ self basicAt: i) key == aKey ifTrue: [
			keyAlreadyExists _ true.
			propertyOrPragma isVariableBinding
				ifTrue: [ propertyOrPragma value: aValue ]
				ifFalse: [ self basicAt: i put: aValue ]]].
	
	keyAlreadyExists ifFalse: [
		method propertyValueAt: aKey put: aValue ].
	
	^ aValue! !


!Color methodsFor: 'transformations' stamp: 'jmv 5/7/2012 15:05'!
twiceDarker
	"Answer a significantly darker shade of this color."

	^ self adjustSaturation: 0.076 brightness: -0.15! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/7/2012 15:26'!
noon
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: [ :d :m :y |
		self class year: y month: m day: d hour: 12 minute: 0 second: 0 ]! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 5/7/2012 15:25'!
epoch
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self julianDayNumber: SqueakEpoch! !


!ExceptionSet methodsFor: 'private' stamp: 'jmv 5/7/2012 21:38'!
add: anException

	^exceptions add: anException! !


!ExceptionTester methodsFor: 'tests' stamp: 'jmv 5/7/2012 21:38'!
simpleNoTimeoutTest

	[ self doSomething ]
		valueWithin: 1 days onTimeout:
			[ self doSomethingElse ]! !


!FloatTest methodsFor: 'IEEE 754' stamp: 'jmv 5/7/2012 15:15'!
testInfinity3
	self assert:
		(Float infinity negated asIEEE32BitWord
			printPaddedWith: $0
			to: 32
			base: 2) = '11111111100000000000000000000000'.
	self assert:
		(Float fromIEEE32Bit:
			(Integer
				readFrom: '11111111100000000000000000000000' readStream
				base: 2)) = Float infinity negated! !

!FloatTest methodsFor: 'IEEE 754' stamp: 'jmv 5/7/2012 15:15'!
testZero2
	self assert:
		(Float negativeZero asIEEE32BitWord
			printPaddedWith: $0
			to: 32
			base: 2) = '10000000000000000000000000000000'.
	self assert:
		(Float fromIEEE32Bit:
			(Integer
				readFrom: '10000000000000000000000000000000' readStream
				base: 2)) = Float negativeZero! !


!Heap methodsFor: 'removing' stamp: 'jmv 5/7/2012 21:54'!
removeAll

	array atAllPut: nil.
	tally _ 0! !


!Integer methodsFor: 'bit manipulation' stamp: 'jmv 5/7/2012 15:09'!
>> shiftAmount  "right shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount negated! !


!MethodContext methodsFor: 'printing' stamp: 'jmv 5/7/2012 20:25'!
printString
	"Answer an emphasized string in case of a breakpoint method"
	(self method notNil and: [ self method hasBreakpoint ])
		ifFalse: [ ^ super printString ].
	^ super printString , ' [break]' asText allBold! !


!ParserNotification methodsFor: 'as yet unclassified' stamp: 'jmv 5/7/2012 16:20'!
setName: aString

	name _ aString! !


!Process methodsFor: 'accessing' stamp: 'jmv 5/7/2012 16:16'!
isTerminated

	self isActiveProcess ifTrue: [^ false].
	^suspendedContext isNil
	  or: ["If the suspendedContext is the bottomContext it is the block in Process>>newProcess.
		   If so, and the pc is greater than the startpc, the bock has alrteady sent and returned
		   from value and there is nothing more to do."
		suspendedContext isBottomContext
		and: [
			suspendedContext pc > suspendedContext startpc]]! !


!SmallIntegerTest methodsFor: 'testing - basic' stamp: 'jmv 5/7/2012 21:35'!
testEven
	
	self assert: (SmallInteger minVal even).
	self deny: (SmallInteger maxVal even).
	
	self deny: ((SmallInteger minVal + 1) even).
	self assert: ((SmallInteger maxVal - 1) even).
	
	self deny: (1 even).
	self deny: (-1 even).
	
	self assert: (2 even).
	self assert: (-2 even).
	
	self assert: (0 even)! !

!SmallIntegerTest methodsFor: 'testing - basic' stamp: 'jmv 5/7/2012 21:35'!
testOdd
	
	self deny: (SmallInteger minVal odd).
	self assert: (SmallInteger maxVal odd).
	
	self assert: ((SmallInteger minVal + 1) odd).
	self deny: ((SmallInteger maxVal - 1) odd).
	
	self assert: (1 odd).
	self assert: (-1 odd).
	
	self deny: (2 odd).
	self deny: (-2 odd).
	
	self deny: (0 odd)! !


!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 5/7/2012 15:19'!
allContributors
"
	Smalltalk allContributors
"
	| bag author |
	bag _ Bag new.
	Smalltalk allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author _ compiledMethod author.
			author notEmpty ifTrue: [
				bag add: author ]]].
	^bag! !


!SystemDictionary class methodsFor: 'copyright' stamp: 'jmv 5/7/2012 14:49'!
copyright
	"The Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2012.'! !


!Time class methodsFor: 'squeak protocol' stamp: 'jmv 5/7/2012 15:26'!
hour: hour minute: minute second: second nanoSecond: nanoCount
	"Answer a Time"

	^ self 
		seconds: (hour * SecondsInHour) + (minute * SecondsInMinute) + second 
		nanoSeconds: nanoCount! !

!Time class methodsFor: 'squeak protocol' stamp: 'jmv 5/7/2012 15:27'!
noon

	^ self seconds: (SecondsInDay / 2)! !


!UndefinedVariable methodsFor: 'as yet unclassified' stamp: 'jmv 5/7/2012 16:19'!
openMenuIn: aBlock
	| labels caption index |
	labels _ #('yes' 'no' ).
	caption _ name , ' appears to be 
undefined at this point.
Proceed anyway?'.
	index _ aBlock
		value: labels
		value: #()
		value: caption.
	^ self resume: index = 1! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/7/2012 15:25'!
startDay
	^ StartDay ifNil: [ StartDay _ DayNames first ]! !

!methodRemoval: Utilities class #reportSenderCountsFor:!
Utilities class removeSelector: #reportSenderCountsFor:!

!Utilities class reorganize!
('class initialization' startUp)
('common requests' appendToCommonRequests: commonRequestStrings: editCommonRequestStrings eval: initialize initializeCommonRequestStrings offerCommonRequestsInMorphic saveDisplay saveScreenshot)
('identification' authorInitials authorInitialsPerSe authorName authorName: authorNamePerSe browseUncommentedMethodsWithInitials: changeStamp changeStampPerSe dateStamp dateTimeSuffix fixStamp: methodsWithInitials: monthDayTime24StringFrom: monthDayTimeStringFrom: setAuthorInitials setAuthorInitials: setAuthorName)
('miscellaneous' awaitMouseUpIn:repeating:ifSucceed: awaitMouseUpIn:whileMouseDownDo:whileMouseDownInsideDo:ifSucceed: cleanseOtherworldlySteppers createPageTestWorkspace decimalPlacesForFloatPrecision: decommissionTheAllCategory doesMethod:forClass:bearInitials: floatPrecisionForDecimalPlaces: garbageCollectAndReport instanceComparisonsBetween:and: keyLike:withTrailing:satisfying: oppositeCornerFrom: setClassAndSelectorFrom:in: timeStampForMethod:)
('recent method submissions' assureMostRecentSubmissionExists classCommented: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: mostRecentlySubmittedMessage noteMethodSubmission:forClass: numberOfRecentSubmissionsToStore numberOfRecentSubmissionsToStore: purgeFromRecentSubmissions: purgeRecentSubmissionsOfMissingMethods recentMethodSubmissions revertLastMethodSubmission)
('summer97 additions' browseVersionsForClass:selector: chooseFileWithSuffix: chooseFileWithSuffixFromList:withCaption: classCategoriesStartingWith: classFromPattern:withCaption: graphicsFileSuffixes hierarchyOfClassesSurrounding: hierarchyOfImplementorsOf:forClass: inviolateInstanceVariableNames isLegalInstVarName: wellFormedInstanceVariableNameFrom:)
('support windows' commandKeyMappings openCommandKeyHelp openStandardWorkspace standardWorkspaceContents)
('user interface' informUser:during: informUserDuring:)
('vm statistics' reportCPUandRAM textMarkerForShortReport vmStatisticsReportString vmStatisticsShortString)
('tailoring system' stripMethods:messageCode:)
('closure support' compileUsingClosures initializeClosures postRecompileCleanup)
!

!methodRemoval: SystemDictionary #agreedContributors!
SystemDictionary removeSelector: #agreedContributors!
!methodRemoval: SystemDictionary #missingAuthorsWithMethods!
SystemDictionary removeSelector: #missingAuthorsWithMethods!
!methodRemoval: SystemDictionary #newContributors!
SystemDictionary removeSelector: #newContributors!
!methodRemoval: SystemDictionary #okContributors!
SystemDictionary removeSelector: #okContributors!
!methodRemoval: SystemDictionary #relicenseEffortStartDate!
SystemDictionary removeSelector: #relicenseEffortStartDate!
!methodRemoval: SystemDictionary #returnedSignatories!
SystemDictionary removeSelector: #returnedSignatories!
!methodRemoval: BlockContext #argumentCount!
BlockContext removeSelector: #argumentCount!
