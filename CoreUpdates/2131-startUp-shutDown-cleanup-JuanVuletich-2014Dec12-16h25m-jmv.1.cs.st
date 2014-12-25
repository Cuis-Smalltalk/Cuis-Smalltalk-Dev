'From Cuis 4.2 of 25 July 2013 [latest update: #2128] on 12 December 2014 at 4:58:23.239899 pm'!

!ProcessBrowser class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:53'!
initialize
	"
	ProcessBrowser initialize.
	SystemDictionary initialize.
	"
	SuspendedProcesses ifNil: [
		SuspendedProcesses _ IdentityDictionary new ]! !


!WeakArray class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:57'!
initialize
	"
	WeakArray initialize.
	SystemDictionary initialize.
	"

	"Do we need to initialize specialObjectsArray?"
	Smalltalk specialObjectsArray size < 42 
		ifTrue:[ Smalltalk recreateSpecialObjectsArray ].

	self restartFinalizationProcess! !


!ShortIntegerArray class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:56'!
initialize
	"
	ShortIntegerArray initialize.
	SystemDictionary initialize.
	"
	LastSaveOrder _ self new: 2.
	LastSaveOrder at: 1 put: 42.
	LastSaveOrder at: 2 put: 13! !


!ShortRunArray class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:56'!
initialize
	"
	ShortRunArray initialize
	SystemDictionary initialize.
	"
	LastSaveOrder _ #(42 42 42) as: self! !


!Symbol class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:54'!
initialize

	"
	Symbol initialize.
	SystemDictionary initialize.
	"
	Symbol rehash.
	OneCharacterSymbols _ nil.
	OneCharacterSymbols _ (1 to: 256) collect: [ :i | (i - 1) asCharacter asSymbol]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/12/2014 16:49'!
shutDown
'SystemDictionary class shutDown corriendo!!!!!!!!!!' print.
"Si no, agregarlo al shutdonlist list en el initialize!!!!!!!!"
	^ self closeSourceFiles! !


!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 12/12/2014 16:43'!
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
	y startUp se ejecuta TAMVIEN cuando levanta la imagen.
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
		#Color
		#FileDirectory
		#Symbol
		#ProcessBrowser
		#CPUWatcher
		#BitBltCanvas)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]! !

!SystemDictionary class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:47'!
startUp
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk
	"
'SystemDictionary class startUp corriendo!!!!!!!!!!' print.
"Si no, agregarlo al startup list en el initialize!!!!!!!!"
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk! !


!Taskbar class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:56'!
initialize
	"
	Taskbar initialize.
	SystemDictionary initialize.
	"
	self singleton openInWorld.
	self startUp! !


!BitBltCanvas class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:52'!
initialize
	"
	BitBltCanvas initialize.
	SystemDictionary initialize.
	"
	AccessProtect _ Semaphore forMutualExclusion.
	self clearFormsCache! !

BitBltCanvas initialize!
Taskbar initialize!
SystemDictionary initialize!
Symbol initialize!
ShortRunArray initialize!
ShortIntegerArray initialize!
WeakArray initialize!
ProcessBrowser initialize!
!methodRemoval: CPUWatcher class #initialize!
CPUWatcher class removeSelector: #initialize!
CPUWatcher initialize!
