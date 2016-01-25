'From Cuis 4.2 of 25 July 2013 [latest update: #2667] on 24 January 2016 at 9:31:24.967646 pm'!

!StrikeFont class methodsFor: 'class cached access' stamp: 'jmv 1/24/2016 21:24'!
releaseClassCachedState
	"
	StrikeFont releaseClassCachedState
	"
	"Deallocate synthetically derived copies of base fonts to save space"
	self allSubInstancesDo: [ :sf | sf reset ]! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 1/24/2016 21:27'!
initClassCachedState

	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each isWorldMorph ifTrue: [
			each restoreMorphicDisplay ]]! !

!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 1/24/2016 21:27'!
releaseClassCachedState

	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each releaseCachedState ]! !


!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 1/24/2016 21:27'!
initialize
	"
	SystemDictionary initialize
	"
	
	"Hacer esto bien explicito al menos para las clases en la imagen base?
	Quisiera un mecanismo que sea igual de bueno para clases base que para clases en paquetes.
	Excepto para MUY pocas cosas quizas, como la 1ra parte de StartUpList... El resto deberia usar el mismo esquema
	este en la imagen base o en paquetes. Y seria quizas onda initialize.
	O sea, quizas al cargar un paquete deberia llamarse initialize y startup, no se.
	Como que initialize es cuando se crea la clase (o en cualquier momento que quieras reinicializar)
	y startUp se ejecuta TAMBIEN cuando levanta la imagen.
	Como initialize clashea con las instancias, quizas uniformar todo en startUp: y shutDown: / startUp y shutDown .... 
	
	Cargar todos los paquetes. estudiar implementors y senders de startUp: y shutDown: / startUp y shutDown
	ver senders de addToStartUpList: y addToShutDownList:"

	StartUpList _ OrderedCollection new.
	"These get processed from the top down..."
	#(
		#Delay
		#DisplayScreen
		#Cursor
		#InputSensor
		#ProcessorScheduler 	"Starts low space watcher and bkground."
		#WeakArray)
			do: [ :clsName | 	
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
			
	ShutDownList _ OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay
		#DisplayScreen
		#InputSensor
		#Symbol
		#ProcessBrowser
		#CPUWatcher)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]! !

!methodRemoval: PasteUpMorph class #shutDown!
PasteUpMorph class removeSelector: #shutDown!
!methodRemoval: PasteUpMorph class #startUp!
PasteUpMorph class removeSelector: #startUp!
!methodRemoval: StrikeFont class #shutDown!
StrikeFont class removeSelector: #shutDown!

!StrikeFont class reorganize!
('instance creation' create:size:bold:italic:boldItalic: install:)
('removing' limitTo16Bits removeForPDA removeMostFonts removeSomeFonts saveSpace)
('character shapes' buildLargerPunctuation: makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useUnderscore)
('class cached access' releaseClassCachedState)
!

SystemDictionary initialize!

!Taskbar class reorganize!
('as yet unclassified')
!

