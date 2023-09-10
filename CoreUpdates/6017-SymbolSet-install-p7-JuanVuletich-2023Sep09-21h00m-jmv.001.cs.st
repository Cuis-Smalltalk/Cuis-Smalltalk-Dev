'From Cuis 6.0 [latest update: #6013] on 9 September 2023 at 9:00:55 pm'!

!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 9/9/2023 21:00:19'!
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

	StartUpList := OrderedCollection new.
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
			
	ShutDownList := OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay
		#DisplayScreen
		#InputSensor
		#ProcessBrowser
		#CPUWatcher)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]! !

!methodRemoval: Symbol class #shutDown stamp: 'jmv 9/9/2023 21:00:35'!
Symbol class removeSelector: #shutDown!
Compiler recompileAll.
SystemDictionary initialize!
