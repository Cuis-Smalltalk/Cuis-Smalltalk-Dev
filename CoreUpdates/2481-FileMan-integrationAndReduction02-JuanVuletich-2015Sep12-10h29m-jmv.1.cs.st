'From Cuis 4.2 of 25 July 2013 [latest update: #2479] on 12 September 2015 at 10:40:10.682319 am'!

!FmFileEntry class methodsFor: 'system startup' stamp: 'jmv 9/12/2015 10:35'!
shutDown! !

!FmFileEntry class methodsFor: 'system startup' stamp: 'jmv 9/12/2015 10:37'!
startUp
	FmFileIOAccessor onSystemStartUp.
	self onSystemStartUp! !


!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 9/12/2015 10:34'!
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
		#FileDirectory 			"Enables file stack dump and opens sources."
		#FmFileEntry
		#ShortIntegerArray
		#ShortRunArray
		#PasteUpMorph
		#WeakArray
		#Taskbar)
			do: [ :clsName | 	
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
			
	ShutDownList _ OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay
		#DisplayScreen
		#InputSensor
		#PasteUpMorph
		#StrikeFont
		#FileDirectory
		#FmFileEntry
		#Symbol
		#ProcessBrowser
		#CPUWatcher)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]! !


!FmFileIOAccessor class methodsFor: 'events' stamp: 'jmv 9/12/2015 10:37'!
onSystemStartUp
	self initDefault.! !


!FmSqPortableUtil class methodsFor: 'startUp' stamp: 'jmv 9/12/2015 10:37'!
startUp
! !


!FmFileEntry class reorganize!
('accessing' drives)
('instance creation' entry: pathComponents: pathComponents:drive: pathName:)
('examples' example1 example2 example3 example4 example5 example6 example7)
('class initialization')
('starting up' onSystemStartUp)
('system startup' shutDown startUp)
!

SystemDictionary initialize!
